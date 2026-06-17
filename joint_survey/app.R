# Libraries -------------------------------------------------------------------

library(shiny)
library(bslib)
library(fontawesome)
library(reactable)
library(here)
library(ggplot2)
library(ggridges)
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

default_plot_code <- 'ggplot(df, aes(x = flying_speed_kmh, y = dragon_type, fill = dragon_type)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.6) +
  scale_fill_viridis_d() +
  theme_ridges() +
  labs(x = "Flying Speed (km/h)", y = NULL, title = "Flying Speed by Dragon Type") +
  theme(legend.position = "none")'

# UI --------------------------------------------------------------------------

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = NULL,
  includeCSS(here("styles.css")),
  useBusyIndicators(),
  sidebar = sidebar(
    chat_ui("chat")
  ),
  layout_columns(
    col_widths = c(7, 5),
    # Left: Dashboard
    card(
      style = "min-height: 450px;",
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Custom plot",
        span(
          actionLink(
            "interpret_custom",
            icon_explain,
            class = "me-3 text-decoration-none",
            aria_label = "Explain custom plot"
          ),
          actionButton(
            "run_custom",
            "Run",
            class = "btn btn-sm btn-primary"
          )
        )
      ),
      tags$div(
        style = "padding: 0 1rem;",
        textAreaInput(
          "custom_code",
          tags$span("R code — use ", tags$code("df"), " for the current dataset"),
          value = default_plot_code,
          rows = 6,
          width = "100%"
        )
      ),
      plotOutput("custom_plot")
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

  # --- Custom plot ------------------------------------------------------------

  plot_code <- reactiveVal(default_plot_code)

  observeEvent(input$run_custom, {
    plot_code(input$custom_code)
  })

  custom_plot <- reactive({
    req(nrow(dragons_data()) > 0)
    df <- dragons_data()
    env <- new.env(parent = globalenv())
    env$df <- df
    tryCatch(
      eval(parse(text = plot_code()), envir = env),
      error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message),
                   size = 4, color = "red") +
          theme_void()
      }
    )
  })

  output$custom_plot <- renderPlot({
    custom_plot()
  })

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
