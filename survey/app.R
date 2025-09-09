# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

# Load packages
library(surveydown)
library(here)
library(dplyr)
library(plotly)
library(ggplot2)



tips <- readr::read_csv(here("tips.csv")) |>
  mutate(percent = round((tip / total_bill) * 100, 2))


# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(ignore = TRUE)

# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {

  # 📊 Scatter plot ----------------------------------------------------------

  scatterplot <- reactive({
    req(nrow(tips) > 0)

    # Default to "none" if scatter_color input doesn't exist yet
    color <- if(is.null(input$scatter_color)) "Day" else input$scatter_color

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


  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_forward()

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if()

  # Database designation and other settings
  sd_server(
    db = db,
    use_cookies = FALSE   # Fix it
  )

}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
