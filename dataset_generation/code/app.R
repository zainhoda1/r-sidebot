library(shiny)
library(bslib)
library(tidyverse)
library(here)
library(thematic)

#thematic_shiny()

dragons <- read_csv(here("data", "dragons.csv"), show_col_types = FALSE)

numeric_vars <- c(
  "Claw Length (cm)"    = "claw_length_cm",
  "Claw Thickness (cm)" = "claw_thickness_cm",
  "Wingspan (m)"        = "wingspan_m",
  "Weight (kg)"         = "weight_kg",
  "Flying Speed (km/h)" = "flying_speed_kmh"
)

dragon_colors <- c(
  "Forest Dragon"   = "#2d7a2d",
  "Mountain Dragon" = "#8b5e3c",
  "Sea Dragon"      = "#1a6b8a"
)

ui <- page_sidebar(
  title = "Dragon Dataset Explorer",
  theme = bs_theme(version = 5, preset = "flatly"),
  sidebar = sidebar(
    selectInput("x_var", "X Axis", choices = numeric_vars, selected = "wingspan_m"),
    selectInput("y_var", "Y Axis", choices = numeric_vars, selected = "flying_speed_kmh"),
    hr(),
    checkboxGroupInput(
      "types", "Dragon Types",
      choices = sort(unique(dragons$dragon_type)),
      selected = unique(dragons$dragon_type)
    ),
    hr(),
    input_switch("show_smooth", "Show trend line", value = TRUE)
  ),
  layout_column_wrap(
    width = 1/3,
    fill = FALSE,
    value_box(
      title = "Dragons shown",
      value = textOutput("n_total", inline = TRUE),
      theme = "primary"
    ),
    value_box(
      title = "Avg flying speed",
      value = textOutput("avg_speed", inline = TRUE),
      theme = "success"
    ),
    value_box(
      title = "Avg wingspan",
      value = textOutput("avg_wingspan", inline = TRUE),
      theme = "info"
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Scatter Plot"),
    plotOutput("scatter", height = "450px")
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    dragons |> filter(dragon_type %in% input$types)
  })

  output$n_total <- renderText({
    nrow(filtered())
  })

  output$avg_speed <- renderText({
    paste0(round(mean(filtered()$flying_speed_kmh, na.rm = TRUE), 1), " km/h")
  })

  output$avg_wingspan <- renderText({
    paste0(round(mean(filtered()$wingspan_m, na.rm = TRUE), 2), " m")
  })

  output$scatter <- renderPlot({
    req(nrow(filtered()) > 0)

    x_label <- names(numeric_vars)[numeric_vars == input$x_var]
    y_label <- names(numeric_vars)[numeric_vars == input$y_var]

    p <- filtered() |>
      ggplot(aes(
        x     = .data[[input$x_var]],
        y     = .data[[input$y_var]],
        color = dragon_type
      )) +
      geom_point(alpha = 0.7, size = 2.5) +
      scale_color_manual(values = dragon_colors, name = NULL) +
      labs(x = x_label, y = y_label) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    if (input$show_smooth) {
      p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8)
    }

    p
  })
}

shinyApp(ui, server)
