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

design_id <- "dashboard_no_llm"

db <- sd_db_connect()

dashboard_events_init(db)

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

# A plot box with a fixed 6:4 (width:height) aspect ratio, centred in
# whatever space its container leaves. The exact pixel size is set by JS
# (see sized_plot_js below), scaling the box up to fill the outer box while
# preserving the aspect ratio -- aspect-ratio alone only caps the *narrower*
# dimension, so a container that doesn't match the ratio would otherwise
# leave the box under- or over-sized on one axis.
plot_box <- function(...) {
  tags$div(
    class = "sized-plot-outer",
    style = paste(
      "height: 100%; width: 100%; min-height: 0; min-width: 0;",
      "display: flex; flex-direction: row;",
      "align-items: center; justify-content: center;"
    ),
    tags$div(
      class = "sized-plot",
      style = "aspect-ratio: 6 / 4; flex: 0 0 auto;",
      ...
    )
  )
}

sized_plot_js <- "
function sizeSizedPlots() {
  var ratio = 6 / 4;
  document.querySelectorAll('.sized-plot-outer').forEach(function(outer) {
    var box = outer.querySelector('.sized-plot');
    if (!box) return;
    var width = outer.clientWidth;
    var height = outer.clientHeight;
    if (width / height > ratio) {
      width = height * ratio;
    } else {
      height = width / ratio;
    }
    box.style.width = Math.max(0, width) + 'px';
    box.style.height = Math.max(0, height) + 'px';
  });
}
document.addEventListener('DOMContentLoaded', function() {
  document.querySelectorAll('.sized-plot-outer').forEach(function(outer) {
    new ResizeObserver(sizeSizedPlots).observe(outer);
  });
  sizeSizedPlots();
});
"

# UI --------------------------------------------------------------------------

ui <- page_fillable(
  style = "background-color: rgb(248, 248, 248);",
  title = NULL,
  includeCSS(here("styles.css")),
  tags$head(tags$script(HTML(sized_plot_js))),
  useBusyIndicators(),
  layout_columns(
    col_widths = c(6, 6),
    # Left: Visualization
    card(
      style = "min-height: 450px; overflow: hidden;",
      card_header("Visualization"),
      # Top: dragon species filter, applies to both plots
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
      tags$div(
        class = "html-fill-item",
        style = paste(
          "flex: 1 1 0; min-height: 0; display: flex;",
          "flex-direction: row; gap: 1.25rem; padding: 0.5rem 1rem 1rem;",
          "overflow: hidden;"
        ),
        # Left: the rest of the dials, in a ribbon
        tags$div(
          style = paste(
            "flex: 0 0 200px; display: flex; flex-direction: column;",
            "gap: 1rem; overflow-y: auto;"
          ),
          radioButtons(
            "plot_type",
            "Plot type",
            choices  = c("Scatter", "Bar"),
            selected = "Scatter"
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Scatter'",
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
            condition = "input.plot_type == 'Bar'",
            selectInput(
              "bar_var",
              "Variable",
              choices  = scatter_vars,
              selected = "claw_length_cm"
            )
          )
        ),
        # Right: the square plot
        tags$div(
          style = "flex: 1 1 0; min-height: 0; min-width: 0; overflow: hidden;",
          plot_box(
            conditionalPanel(
              condition = "input.plot_type == 'Scatter'",
              style     = "height: 100%;",
              plotOutput("scatter_plot", height = "100%")
            ),
            conditionalPanel(
              condition = "input.plot_type == 'Bar'",
              style     = "height: 100%;",
              plotOutput("bar_plot", height = "100%")
            )
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
      theme_cowplot(font_size = 13) +
      panel_border(color = "black", size = 1) +
      theme(legend.position = "none")
  })

  output$scatter_plot <- renderPlot({ scatter_plot_r() })
  output$bar_plot     <- renderPlot({ bar_plot_r() })

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



  # --- Survey server ----------------------------------------------------------

  sd_skip_if()
  sd_server(db = db)
}

shinyApp(ui, server)
