library(surveydown)
library(here)
library(dplyr)
library(plotly)
library(ggplot2)


# Add this line at the very beginning of your app.R file
#setwd(here::here("demo1"))


tips <- readr::read_csv(here("data","tips.csv")) |>
  mutate(percent = round((tip / total_bill) * 100, 2))

data(mtcars)

db <- sd_db_connect(ignore = TRUE)

server <- function(input, output, session) {

  # 📊 Scatter plot ----------------------------------------------------------

  scatterplot <- reactive({
    req(nrow(tips) > 0)

    # Default to "none" if scatter_color input doesn't exist yet
    color <- if(is.null(input$scatter_color)) "day" else input$scatter_color

    data <- tips

    p <- plot_ly(
      data,
      x = ~total_bill,
      y = ~tip,
      type = "scatter",
      mode = "markers"
    )

    if (color != "none") {
      p <- plot_ly(
        data,
        x = ~total_bill,
        y = ~tip,
        color = as.formula(paste0("~", color)),
        type = "scatter",
        mode = "markers"
      )
    }

    p <- p |>
      add_lines(
        x = ~total_bill,
        y = fitted(loess(tip ~ total_bill, data = data)),
        line = list(color = "rgba(255, 0, 0, 0.5)"),
        name = "LOESS",
        inherit = FALSE
      )

    p <- p |> layout(showlegend = FALSE)

    return(p)
  })

  output$scatterplot <- renderPlotly({
    scatterplot()
  })



  mtcars_scatterplot <- reactive({
    req(input$scatter_x, input$scatter_y)

    plot_ly(
      data = mtcars,
      x = ~input$scatter_color,
      y = ~input$scatter_y,
      type = "scatter",
      mode = "markers"
    ) |> layout(showlegend = FALSE)
  })

  output$mtcars_scatterplot <- renderPlotly({
    mtcars_scatterplot()
  })


  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_forward()

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if()

  # Database designation and other settings
  sd_server(
    db = db,
    use_cookies = FALSE   # Fix it
  )



 # sd_server()
}

shiny::shinyApp(ui = sd_ui(), server = server)
