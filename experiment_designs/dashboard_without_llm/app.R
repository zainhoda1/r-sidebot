# Libraries -------------------------------------------------------------------

library(shiny)
library(bslib)
library(fontawesome)
library(reactable)
library(here)
library(ggplot2)
library(cowplot)
library(dplyr)
library(surveydown)

source(here::here("R", "log_dashboard_settings.R"))
source(here::here("R", "log_mouse_tracking.R"))

# Data & config ---------------------------------------------------------------

design_id <- "dashboard_no_llm"

db <- sd_db_connect()

dashboard_events_init(db)
mouse_tracking_init()

dragons <- readr::read_csv(here("data", "dragons.csv"))

scatter_vars <- c(
  "Flying Speed (km/h)"  = "flying_speed_kmh",
  "Wingspan (m)"         = "wingspan_m",
  "Weight (kg)"          = "weight_kg",
  "Claw Length (cm)"     = "claw_length_cm",
  "Claw Thickness (cm)"  = "claw_thickness_cm"
)

dragon_species <- sort(unique(dragons$dragon_type))
dragon_species_choices <- c("All Species", dragon_species)

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
  includeCSS(here("styles.css")),
  tags$script(src = "mouse_tracking.js"),
  useBusyIndicators(),
  layout_columns(
    col_widths = c(6, 6),
    # Left: visualization on top, empty space underneath (where the LLM
    # assistant panel sits in the dashboard_with_llm design), equally sized.
    # The controls sit in a sidebar inside the visualization card, so they
    # only take width from the plot.
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
            "dragon_species",
            "Dragon species",
            choices  = dragon_species_choices,
            selected = "All Species"
          ),
          conditionalPanel(
            condition = "input.viz_tab == 'Scatter'",
            selectInput(
              "scatter_x",
              "X axis",
              choices  = scatter_vars,
              selected = "weight_kg"
            ),
            selectInput(
              "scatter_y",
              "Y axis",
              choices  = scatter_vars,
              selected = "claw_length_cm"
            )
          ),
          conditionalPanel(
            condition = "input.viz_tab == 'Bar'",
            selectInput(
              "bar_var",
              "Variable",
              choices  = scatter_vars,
              selected = "flying_speed_kmh"
            )
          )
        ),
        nav_panel("Scatter", plot_box("viz_plot_scatter")),
        nav_panel("Bar",     plot_box("viz_plot_bar"))
      ),
      # Bottom: empty white space where the assistant panel sits in the
      # dashboard_with_llm design.
      card(
        style = "min-height: 0;"
      )
    ),
    # Right: Survey
    card(
      id = "survey_container",
      style = "min-height: 450px; overflow-y: auto;",
      sd_ui()
    )
  )
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

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
      #theme_cowplot(font_size = 13) +
      #panel_border(color = "black", size = 1) +
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

  # One output per tab; Shiny suspends the hidden one, so only the visible
  # chart is ever drawn.
  output$viz_plot_scatter <- renderPlot({ scatter_plot_r() })
  output$viz_plot_bar     <- renderPlot({ bar_plot_r() })

  # --- Dashboard settings logging ---------------------------------------------

  # The survey session id, once surveydown's cookie JS has reported it;
  # falls back to the Shiny token, which is what surveydown itself uses.
  survey_session_id <- function() {
    sid <- isolate(input$stored_session_id)
    if (!is.null(sid) && nzchar(sid)) sid else session$token
  }

  prev_settings <- reactiveVal(NULL)

  observe({
    current <- list(
      scatter_x      = input$scatter_x %||% "",
      scatter_y      = input$scatter_y %||% "",
      bar_var        = input$bar_var %||% "",
      dragon_species = paste(input$dragon_species, collapse = ", ")
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
      db             = db,
      design         = design_id,
      session_id     = survey_session_id(),
      shiny_token    = session$token,
      event          = event,
      scatter_x      = current$scatter_x,
      scatter_y      = current$scatter_y,
      bar_var        = current$bar_var,
      dragon_species = current$dragon_species
    )
  })



  # --- Mouse tracking on the survey panel --------------------------------------

  observeEvent(input$mouse_move, {
    m <- input$mouse_move
    log_mouse_move(
      session_id  = survey_session_id(),
      shiny_token = session$token,
      x = m$x, y = m$y, width = m$width, height = m$height,
      client_x = m$client_x, client_y = m$client_y, client_ts = m$client_ts
    )
  })

  observeEvent(input$mouse_hover, {
    h <- input$mouse_hover
    log_mouse_hover(
      session_id  = survey_session_id(),
      shiny_token = session$token,
      hovering    = h$hovering,
      client_ts   = h$client_ts
    )
  })

  # --- Survey server ----------------------------------------------------------

  sd_skip_if()
  sd_server(db = db)
}

shinyApp(ui, server)
