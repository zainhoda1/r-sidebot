# Libraries -------------------------------------------------------------------

library(shiny)
library(bslib)
library(fontawesome)
library(reactable)
library(ggplot2)
library(dplyr)
library(promises)
library(ellmer)
library(shinychat)
library(surveydown)
library(cowplot)

# Everything this app reads or writes lives inside its own directory. Paths are
# relative to the app directory, which Shiny makes the working directory; do not
# reintroduce here::here(), which resolves to the enclosing RStudio project and
# so leaks data and logs outside this folder.
# Chat transcripts and dashboard events go in this design's own logs/ folder.
# Pinned at startup, when Shiny has the working directory set to the app
# directory.
options(chatapp.log_dir = normalizePath(getwd(), winslash = "/", mustWork = FALSE))

# All of this design's Supabase tables share this prefix:
#   dashboard_llm_chat_events, dashboard_llm_conversation_turns,
#   dashboard_llm_dashboard_events. Change it here to rename all three.
options(chatapp.tbl_prefix = "dashboard_llm_")

# The dragon and coffee experiments share the same physical Supabase tables
# (same prefix above), so this tags every row from this design distinctly —
# without it, sb_design() would fall back to this folder's basename, which is
# identical to the dragon experiment's dashboard_with_llm folder.
options(chatapp.design = "coffee_dashboard_with_llm")

# shinyapps.io has no environment-variable settings — deployApp(envVars = ) is a
# Posit Connect feature only — so ANTHROPIC_API_KEY ships in the bundled
# .Renviron, the same way .env carries the database credentials. R normally reads
# ./.Renviron at startup; re-reading it here makes the key available regardless
# of the working directory the host started R in.
if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY")) && file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("R/supabase_log.R")
source("R/log_exchange.R")
source("R/log_dashboard_settings.R")
source("R/explain-plot.R")

# Data & config ---------------------------------------------------------------

design_id <- "coffee_dashboard_with_llm"

db <- sd_db_connect()

# Chat transcripts and dashboard events go to Supabase, creating the tables on
# first run. Independent of sd_db_connect() above, so the logs land in the
# database even while the survey itself runs in preview mode. A no-op when .env
# supplies no credentials, in which case logging stays file-only.
sb_connect()
onStop(sb_disconnect)

farms <- readr::read_csv("data/coffee.csv")

# In this design the participant drives the plot with the dashboard controls and
# the assistant only ever interprets what is on screen. prompt.md carries that
# whole brief, including the description of the controls.
explain_prompt <- paste(readLines("prompt.md", warn = FALSE), collapse = "\n")

scatter_vars <- c(
  "Price per kg (USD)"      = "price_per_kg_usd",
  "Altitude (m)"            = "altitude_m",
  "Farm Size (ha)"          = "farm_size_ha",
  "Tree Age (years)"        = "tree_age_years",
  "Yield (kg per hectare)"  = "yield_kg_per_ha"
)

varietals <- sort(unique(farms$varietal))
varietal_choices <- c("All Varietals", varietals)

# NOTE: the checkbox input id and the logging field are still called
# "coffee_types" so the existing dashboard_llm_dashboard_events table schema
# keeps working. Only the labels shown to participants changed.

# A 6:4 (wider-than-tall) plot, centred in whatever space the sidebar leaves.
# Each chart tab gets its own output, so only the visible one is ever drawn.
plot_box <- function(id) {
  tags$div(
    style = paste(
      "height: 100%; width: 100%; min-height: 0;",
      "display: flex; flex-direction: row;",
      "align-items: center; justify-content: center;"
    ),
    tags$div(
      style = paste(
        "height: 100%; aspect-ratio: 6 / 4;",
        "max-width: 100%; flex: 0 0 auto;"
      ),
      plotOutput(id, height = "100%")
    )
  )
}

# UI --------------------------------------------------------------------------

ui <- page_fillable(
  style = "background-color: rgb(248, 248, 248);",
  title = NULL,
  includeCSS("styles.css"),
  useBusyIndicators(),
  layout_columns(
    col_widths = c(6, 6),
    # Left: visualization on top, assistant underneath, equally sized. The
    # controls sit in a sidebar inside the visualization card, so they only
    # take width from the plot and the chat spans the full column.
    layout_columns(
      col_widths = 12,
      row_heights = c(1, 1),
      # Top: visualization. Chart type is the tab strip; the remaining controls
      # sit in a sidebar down the left of the card, sized to fit without a
      # scrollbar (see .viz-controls in styles.css).
      navset_card_tab(
        id = "viz_tab",
        title = "Choose a chart type:",
        selected = "Scatter",
        wrapper = function(...) card_body(..., class = "p-0", style = "min-height: 0;"),
        sidebar = sidebar(
          width = 170,
          open  = "always",
          class = "viz-controls",
          gap   = "0",
          padding = "0.5rem",
          checkboxGroupInput(
            "coffee_types",
            "Varietal",
            choices  = varietal_choices,
            selected = "All Varietals"
          ),
          conditionalPanel(
            condition = "input.viz_tab == 'Scatter'",
            selectInput(
              "scatter_x",
              "X axis",
              choices  = scatter_vars,
              selected = "farm_size_ha"
            ),
            selectInput(
              "scatter_y",
              "Y axis",
              choices  = scatter_vars,
              selected = "tree_age_years"
            )
          ),
          conditionalPanel(
            condition = "input.viz_tab == 'Bar'",
            selectInput(
              "bar_var",
              "Variable",
              choices  = scatter_vars,
              selected = "price_per_kg_usd"
            )
          )
        ),
        nav_panel("Scatter", plot_box("viz_plot_scatter")),
        nav_panel("Bar",     plot_box("viz_plot_bar"))
      ),
      # Bottom: ask the assistant about the plot above
      card(
        style = "min-height: 0;",
        card_header("Ask the AI about this plot"),
        card_body(
          class = "p-0",
          style = "min-height: 0;",
          chat_ui(
            "explain_chat",
            placeholder = "Ask a question about the current plot…",
            height = "100%"
          )
        )
      )
    ),
    # Right: Survey
    card(
      style = "min-height: 450px; overflow-y: auto;",
      sd_ui()
    )
  )
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Dashboard chat ---------------------------------------------------------

  chat <- chat_anthropic(system_prompt = explain_prompt, model = "claude-sonnet-4-6")

  # The survey session id, once surveydown's cookie JS has reported it; falls
  # back to the Shiny token, which is what surveydown itself uses. Everything
  # logged below carries this, so the chat log and the dashboard events join to
  # the same participant.
  survey_session_id <- function() {
    sid <- isolate(input$stored_session_id)
    if (!is.null(sid) && nzchar(sid)) sid else session$token
  }

  farms_data <- reactive(farms)

  # --- Scatter plot ------------------------------------------------------------

  scatter_plot_r <- reactive({
    req(nrow(farms_data()) > 0, input$scatter_x, input$scatter_y, length(input$coffee_types) > 0)
    all_selected <- "All Varietals" %in% input$coffee_types
    df <- if (all_selected) {
      farms_data()
    } else {
      farms_data() |> filter(varietal %in% input$coffee_types)
    }
    x_var <- input$scatter_x
    y_var <- input$scatter_y
    x_lab <- names(scatter_vars)[scatter_vars == x_var]
    y_lab <- names(scatter_vars)[scatter_vars == y_var]
    p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]]))
    if (all_selected) {
      p <- p + geom_point(color = "black", alpha = 0.7, size = 2)
    } else {
      p <- p +
        geom_point(aes(color = varietal), alpha = 0.7, size = 2) +
        scale_color_viridis_d() +
        labs(color = "Varietal")
    }
    p +
      labs(x = x_lab, y = y_lab) +
      theme_minimal(base_size = 13) +
      theme(legend.position = if (all_selected) "none" else "bottom")
  })

  # --- Bar plot ----------------------------------------------------------------

  bar_plot_r <- reactive({
    req(nrow(farms_data()) > 0, input$bar_var, length(input$coffee_types) > 0)
    df <- if ("All Varietals" %in% input$coffee_types) {
      farms_data()
    } else {
      farms_data() |> filter(varietal %in% input$coffee_types)
    }
    var <- input$bar_var
    lab <- names(scatter_vars)[scatter_vars == var]
    df |>
      group_by(varietal) |>
      summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(x = varietal, y = mean_val, fill = varietal)) +
      geom_col(alpha = 0.85, width = 0.6) +
      scale_fill_viridis_d() +
      labs(x = NULL, y = paste("Mean", lab)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  # The plot the participant is currently looking at, i.e. the one on the
  # selected tab. Used to attach the right image to the assistant.
  custom_plot <- reactive({
    if (isTRUE(input$viz_tab == "Bar")) bar_plot_r() else scatter_plot_r()
  })

  # One output per tab; Shiny suspends the hidden one, so only the visible
  # chart is ever drawn.
  output$viz_plot_scatter <- renderPlot({ scatter_plot_r() })
  output$viz_plot_bar     <- renderPlot({ bar_plot_r() })

  # --- Assistant panel ---------------------------------------------------------

  # What the plot currently shows; used to decide when the assistant needs a
  # fresh image rather than the one already in the conversation.
  plot_signature <- reactive({
    paste(
      input$viz_tab,
      paste(sort(input$coffee_types), collapse = ","),
      input$scatter_x,
      input$scatter_y,
      input$bar_var,
      sep = "|"
    )
  })

  # Signature of the plot whose image has already been sent to the assistant
  plot_seen_by_llm <- reactiveVal(NULL)

  # Send a message to the assistant, attaching the current plot as an image
  # whenever the plot has changed since the last thing it saw.
  ask_about_plot <- function(user_msg) {
    sid <- survey_session_id()
    log_query(user_msg, session_id = sid)

    args <- list(user_msg)
    sig  <- plot_signature()
    p    <- tryCatch(custom_plot(), error = function(e) NULL)
    if (!is.null(p) && !identical(sig, plot_seen_by_llm())) {
      # Never let a failed capture break the participant's question — but say so
      # in the log, otherwise the assistant silently answers without ever having
      # seen the plot, which is indistinguishable from it ignoring the image.
      img <- tryCatch(plot_to_img_content(p), error = function(e) {
        warning("plot image capture failed: ", conditionMessage(e))
        NULL
      })
      if (!is.null(img)) {
        args <- c(args, list(img))
        plot_seen_by_llm(sig)
        # Record which view the assistant was actually shown
        log_plot_image(plot = sig, session_id = sid)
      }
    }

    stream <- do.call(chat$stream_async, args)
    chat_append("explain_chat", stream) %...>%
      (function(...) {
        log_exchange(user_msg, chat$last_turn()@text, session_id = sid)
        log_conversation(chat, sid)
      }) %...!%
      (function(e) warning("explain_panel stream error: ", conditionMessage(e)))
  }

  observeEvent(input$explain_chat_user_input, {
    ask_about_plot(input$explain_chat_user_input)
  })

  # --- Dashboard settings logging ---------------------------------------------

  # One row per change, each a full snapshot of the controls, so the sequence of
  # views a participant built can be replayed and lined up against their
  # questions to the assistant.
  prev_settings <- reactiveVal(NULL)

  observe({
    current <- list(
      viz_tab        = input$viz_tab %||% "",
      scatter_x      = input$scatter_x %||% "",
      scatter_y      = input$scatter_y %||% "",
      bar_var        = input$bar_var %||% "",
      coffee_types = paste(input$coffee_types, collapse = ", ")
    )
    previous <- isolate(prev_settings())

    if (is.null(previous)) {
      event <- "init"
    } else {
      changed <- names(current)[!mapply(identical, current, previous)]
      if (length(changed) == 0) return()
      event <- paste(changed, collapse = "+")
    }
    prev_settings(current)

    log_dashboard_event(
      design         = design_id,
      session_id     = survey_session_id(),
      shiny_token    = session$token,
      event          = event,
      viz_tab        = current$viz_tab,
      scatter_x      = current$scatter_x,
      scatter_y      = current$scatter_y,
      bar_var        = current$bar_var,
      coffee_types = current$coffee_types
    )
  })

  # --- Mouse tracking (survey) ------------------------------------------------

  observeEvent(input$mouse_track, {
    tryCatch({
      events <- input$mouse_track$events
      message("[mouse] received, event count: ", length(events))
      if (length(events) == 0) return()

      df <- data.frame(
        session_id = session$token,
        page       = vapply(events, function(e) as.character(e$pg),  character(1)),
        x          = vapply(events, function(e) as.integer(e$x),     integer(1)),
        y          = vapply(events, function(e) as.integer(e$y),     integer(1)),
        win_width  = vapply(events, function(e) as.integer(e$wx),    integer(1)),
        win_height = vapply(events, function(e) as.integer(e$wy),    integer(1)),
        timestamp  = vapply(events, function(e) as.numeric(e$t),     numeric(1)),
        stringsAsFactors = FALSE
      )

      out_file     <- "mouse_tracking.csv"
      write_header <- !file.exists(out_file)
      write.table(df, file = out_file, append = !write_header,
                  sep = ",", col.names = write_header, row.names = FALSE, quote = TRUE)
      message("[mouse] wrote ", nrow(df), " rows to ", out_file)
    }, error = function(e) {
      message("[mouse] ERROR: ", conditionMessage(e))
    })
  })

  # --- Survey server ----------------------------------------------------------

  sd_skip_if()
  sd_server(db = db)
}

shinyApp(ui, server)
