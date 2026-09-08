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

# Data & config ---------------------------------------------------------------

design_id <- "coffee_dashboard_no_llm"

db <- sd_db_connect()

dashboard_events_init(db)

farms <- readr::read_csv(here("data", "coffee.csv"))

scatter_vars <- c(
  "Price per kg (USD)"      = "price_per_kg_usd",
  "Altitude (m)"            = "altitude_m",
  "Farm Size (ha)"          = "farm_size_ha",
  "Tree Age (years)"        = "tree_age_years",
  "Yield (kg per hectare)"  = "yield_kg_per_ha"
)

varietals <- sort(unique(farms$varietal))
varietal_choices <- c("All Varietals", varietals)

# NOTE: the checkbox input id and the logging field are called
# "coffee_types".

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
  useBusyIndicators(),
  layout_columns(
    col_widths = c(6, 6),
    layout_columns(
      col_widths = 12,
      row_heights = c(1, 1),
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
      card(
        style = "min-height: 0;"
      )
    ),
    card(
      style = "min-height: 450px; overflow-y: auto;",
      sd_ui()
    )
  )
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

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

  output$viz_plot_scatter <- renderPlot({ scatter_plot_r() })
  output$viz_plot_bar     <- renderPlot({ bar_plot_r() })

  # --- Dashboard settings logging ---------------------------------------------

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
      coffee_types   = paste(input$coffee_types, collapse = ", ")
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
      coffee_types   = current$coffee_types
    )
  })

  # --- Survey server ----------------------------------------------------------

  sd_skip_if()
  sd_server(db = db)
}

shinyApp(ui, server)
