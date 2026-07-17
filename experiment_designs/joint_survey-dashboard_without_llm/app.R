# Libraries -------------------------------------------------------------------

library(shiny)
library(bslib)
library(fontawesome)
library(reactable)
library(here)
library(ggplot2)
library(dplyr)
library(ellmer)
library(shinychat)
library(surveydown)

source(here::here("R", "log_exchange.R"))
source(here::here("R", "explain-plot.R"))

# Data & config ---------------------------------------------------------------

db <- sd_db_connect(ignore = TRUE)

dragons <- readr::read_csv(here("data", "dragons.csv"))

system_prompt <- paste(readLines(here("prompt.md"), warn = FALSE), collapse = "\n")

icon_explain <- tags$img(src = here("www", "stars.svg"))

scatter_vars <- c(
  "Flying Speed (km/h)"  = "flying_speed_kmh",
  "Wingspan (m)"         = "wingspan_m",
  "Weight (kg)"          = "weight_kg",
  "Claw Length (cm)"     = "claw_length_cm",
  "Claw Thickness (cm)"  = "claw_thickness_cm"
)

dragon_species <- sort(unique(dragons$dragon_type))
dragon_species_choices <- c("All Species", dragon_species)

# UI --------------------------------------------------------------------------

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = NULL,
  includeCSS(here("styles.css")),
  useBusyIndicators(),
  sidebar = sidebar(
    chat_ui("chat"),
    actionLink(
      "interpret_custom",
      tagList(icon_explain, " Explain plot"),
      class = "text-decoration-none",
      aria_label = "Explain custom plot"
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    # Left: Visualization
    card(
      style = "min-height: 450px;",
      card_header("Visualization"),
      tags$div(
        style = "padding: 0 1rem 0.25rem;",
        checkboxGroupInput(
          "dragon_species",
          "Dragon species",
          choices  = dragon_species_choices,
          selected = "All Species",
          inline   = TRUE
        )
      ),
      navset_tab(
        id = "viz_tab",
        nav_panel(
          "Scatter",
          tags$div(
            style = "padding: 0.5rem 1rem 0; display: flex; gap: 1rem;",
            selectInput(
              "scatter_x",
              "X axis",
              choices  = scatter_vars,
              selected = "flying_speed_kmh",
              width    = "50%"
            ),
            selectInput(
              "scatter_y",
              "Y axis",
              choices  = scatter_vars,
              selected = "wingspan_m",
              width    = "50%"
            )
          ),
          plotOutput("scatter_plot")
        ),
        nav_panel(
          "Bar",
          tags$div(
            style = "padding: 0.5rem 1rem 0;",
            selectInput(
              "bar_var",
              "Variable",
              choices  = scatter_vars,
              selected = "flying_speed_kmh",
              width    = "50%"
            )
          ),
          plotOutput("bar_plot")
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

  chat <- chat_claude(system_prompt = system_prompt, model = "claude-sonnet-4-6")

  session$onFlushed(function() {
    chat_append(
      "chat",
      paste(readLines(here("greeting.md"), warn = FALSE), collapse = "\n")
    )
  }, once = TRUE)

  observeEvent(input$chat_user_input, {
    user_msg <- input$chat_user_input
    stream <- chat$stream_async(user_msg)
    chat_append("chat", stream) %...>%
      (function(...) log_exchange(user_msg, chat$last_turn()$text)) %...!%
      (function(e) warning("chat stream error: ", conditionMessage(e)))
  })

  dragons_data <- reactive(dragons)

  # --- Scatter plot ------------------------------------------------------------

  scatter_plot_r <- reactive({
    req(nrow(dragons_data()) > 0, input$scatter_x, input$scatter_y, length(input$dragon_species) > 0)
    all_selected <- "All Species" %in% input$dragon_species
    df <- if (all_selected) {
      dragons_data()
    } else {
      dragons_data() |> filter(dragon_type %in% input$dragon_species)
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
        geom_point(aes(color = dragon_type), alpha = 0.7, size = 2) +
        scale_color_viridis_d() +
        labs(color = "Dragon type")
    }
    p +
      labs(x = x_lab, y = y_lab) +
      theme_minimal(base_size = 13) +
      theme(legend.position = if (all_selected) "none" else "bottom")
  })

  # --- Bar plot ----------------------------------------------------------------

  bar_plot_r <- reactive({
    req(nrow(dragons_data()) > 0, input$bar_var, length(input$dragon_species) > 0)
    df <- if ("All Species" %in% input$dragon_species) {
      dragons_data()
    } else {
      dragons_data() |> filter(dragon_type %in% input$dragon_species)
    }
    var <- input$bar_var
    lab <- names(scatter_vars)[scatter_vars == var]
    df |>
      group_by(dragon_type) |>
      summarise(mean_val = mean(.data[[var]], na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(x = dragon_type, y = mean_val, fill = dragon_type)) +
      geom_col(alpha = 0.85, width = 0.6) +
      scale_fill_viridis_d() +
      labs(x = NULL, y = paste("Mean", lab)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  custom_plot <- reactive({
    if (isTRUE(input$viz_tab == "Bar")) bar_plot_r() else scatter_plot_r()
  })

  output$scatter_plot <- renderPlot({ scatter_plot_r() })
  output$bar_plot     <- renderPlot({ bar_plot_r() })

  observeEvent(input$interpret_custom, {
    explain_plot(chat, custom_plot(), "chat", session)
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

      out_file     <- here::here("mouse_tracking.csv")
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
