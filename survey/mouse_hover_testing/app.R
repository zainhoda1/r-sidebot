

library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).on('mousemove', function(e) {
        var x = e.pageX;
        var y = e.pageY;
        Shiny.setInputValue('mouse_coords', {x: x, y: y}, {priority: 'event'});
      });
    "))
  ),
  plotOutput("plot"),
  h3("Additional content to test mouse tracking"),
  p("Move your mouse anywhere on this page - all movements are being logged!"),
  sliderInput("slider", "Sample Slider:", min = 1, max = 100, value = 50),
  textInput("text", "Sample Text Input:", value = "Type here...")
)

server <- function(input, output, session) {
  # Initialize CSV file with headers if it doesn't exist
  csv_file <- "mouse_logs.csv"
  if (!file.exists(csv_file)) {
    write.csv(data.frame(x = numeric(0), y = numeric(0), timestamp = character(0)),
              csv_file, row.names = FALSE)
  }

  # Observe mouse movements across the entire app
  observeEvent(input$mouse_coords, {
    if (!is.null(input$mouse_coords)) {
      x <- input$mouse_coords$x
      y <- input$mouse_coords$y
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")  # Seconds precision

      # Save to CSV
      log_entry <- data.frame(x = x, y = y, timestamp = timestamp)
      write.table(log_entry, csv_file, sep = ",", append = TRUE,
                  row.names = FALSE, col.names = FALSE)
    }
  })

  output$plot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg,
         xlab = "Weight (1000 lbs)",
         ylab = "Miles Per Gallon",
         main = "Mouse movements across entire app are logged")
  }, res = 96)
}

shinyApp(ui, server)
