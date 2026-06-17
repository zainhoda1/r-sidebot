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

dragons <- readr::read_csv(here("data", "dragons.csv"))

system_prompt <- paste(readLines(here("prompt.md"), warn = FALSE), collapse = "\n")

icon_explain <- tags$img(src = "stars.svg")

default_plot_code <- 'ggplot(df, aes(x = flying_speed_kmh, y = dragon_type, fill = dragon_type)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.6) +
  scale_fill_viridis_d() +
  theme_ridges() +
  labs(x = "Flying Speed (km/h)", y = NULL, title = "Flying Speed by Dragon Type") +
  theme(legend.position = "none")'

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = "Dragon Explorer",
  includeCSS(here("styles.css")),
  sidebar = sidebar(
    chat_ui("chat")
  ),
  useBusyIndicators(),

  layout_columns(
    style = "min-height: 450px;",
    col_widths = c(16),


    # 📊 Custom plot
    card(
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
  )
)

server <- function(input, output, session) {
  # 💬 Chat ------------------------------------------------------------------

  chat <- chat_claude(system_prompt = system_prompt, model = "claude-sonnet-4-6")

  # Show greeting on startup
  session$onFlushed(function() {
    chat_append(
      "chat",
      paste(readLines(here("greeting.md"), warn = FALSE), collapse = "\n")
    )
  }, once = TRUE)

  observeEvent(input$chat_user_input, {
    user_msg <- input$chat_user_input
    log_query(user_msg)
    stream <- chat$stream_async(user_msg)
    chat_append("chat", stream) %...>%
      (function(...) log_exchange(user_msg, chat$last_turn()@text)) %...!%
      (function(e) warning("chat stream error: ", conditionMessage(e)))
  })

  dragons_data <- reactive(dragons)

  # 📋 Survey (disabled) -----------------------------------------------------

  # sd_server(db = NULL)

  # 📊 Custom plot -----------------------------------------------------------

  # Holds the last successfully submitted code; starts with the default
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
    explain_plot(chat, custom_plot())
  })
}

shinyApp(ui, server)
