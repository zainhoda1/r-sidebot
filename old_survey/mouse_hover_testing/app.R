library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      var mouseTimer = null;
      var currentCoords = null;

      $(document).on('mousemove', function(e) {
        var x = e.pageX;
        var y = e.pageY;

        // Store current coordinates
        currentCoords = {x: x, y: y};

        // Clear existing timer
        if (mouseTimer) {
          clearTimeout(mouseTimer);
        }

        // Set new timer for 1 second (1000 milliseconds)
        mouseTimer = setTimeout(function() {
          // Only send coordinates if mouse hasn't moved for 1 second
          if (currentCoords) {
            Shiny.setInputValue('mouse_coords_delayed', {
              x: currentCoords.x,
              y: currentCoords.y
            }, {priority: 'event'});
          }
        }, 1000);
      });

      // Clear timer when mouse leaves the page
      $(document).on('mouseleave', function() {
        if (mouseTimer) {
          clearTimeout(mouseTimer);
          mouseTimer = null;
        }
      });
    "))
  ),
  plotOutput("plot"),
  h3("Additional content to test mouse tracking"),
  p("Move your mouse anywhere on this page - positions are logged only after staying still for 1 second!"),
  sliderInput("slider", "Sample Slider:", min = 1, max = 100, value = 50),
  textInput("text", "Sample Text Input:", value = "Type here..."),
  br(),
  div(style = "background-color: #f0f0f0; padding: 10px; margin: 10px 0;",
      h4("Instructions:"),
      p("• Move your mouse around the page"),
      p("• Only positions where you pause for 1+ seconds will be logged"),
      p("• Check the 'mouse_logs.csv' file to see the results")
  )
)

server <- function(input, output, session) {
  # Initialize CSV file with headers if it doesn't exist
  csv_file <- "mouse_logs.csv"
  if (!file.exists(csv_file)) {
    write.csv(data.frame(x = numeric(0), y = numeric(0), timestamp = character(0)),
              csv_file, row.names = FALSE)
  }

  # Observe mouse movements that have been delayed by 1 second
  observeEvent(input$mouse_coords_delayed, {
    if (!is.null(input$mouse_coords_delayed)) {
      x <- input$mouse_coords_delayed$x
      y <- input$mouse_coords_delayed$y
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      # Save to CSV
      log_entry <- data.frame(x = x, y = y, timestamp = timestamp)
      write.table(log_entry, csv_file, sep = ",", append = TRUE,
                  row.names = FALSE, col.names = FALSE)

      # Optional: Print to console for debugging
      cat("Logged position:", x, ",", y, "at", timestamp, "\n")
    }
  })

  output$plot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg,
         xlab = "Weight (1000 lbs)",
         ylab = "Miles Per Gallon",
         main = "Mouse positions logged after 1-second pause",
         col = "blue", pch = 16)

    # Add grid for better visualization
    grid()
  }, res = 96)
}

shinyApp(ui, server)
