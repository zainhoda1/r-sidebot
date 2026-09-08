# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

# Load packages
library(shiny)
library(surveydown)

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
  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if(
  )

  # Mouse tracking: receive batched events from JS and append to CSV
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

      out_file     <- file.path(getwd(), "mouse_tracking.csv")
      write_header <- !file.exists(out_file)
      write.table(df, file = out_file, append = !write_header,
                  sep = ",", col.names = write_header, row.names = FALSE, quote = TRUE)
      message("[mouse] wrote ", nrow(df), " rows to ", out_file)
    }, error = function(e) {
      message("[mouse] ERROR: ", conditionMessage(e))
    })
  })

  # Run surveydown server and define database
  sd_server(db = db)
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
