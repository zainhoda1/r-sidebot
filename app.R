library(shiny)
library(bslib)
library(fontawesome)
library(reactable)
library(here)
library(plotly)
library(ggplot2)
library(ggridges)
library(dplyr)
library(querychat)

tips <- readr::read_csv(here("data", "tips.csv")) |>
  mutate(percent = round((tip / total_bill) * 100, 2))

querychat_handle <- querychat_init(
  tips,
  # This is the greeting that should initially appear in the sidebar when the app
  # loads.
  greeting = readLines(here("greeting.md"), warn = FALSE)
)

icon_explain <- tags$img(src = "stars.svg")

default_plot_code <- 'ggplot(df, aes(x = percent, y = day, fill = day)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.6) +
  scale_fill_viridis_d() +
  theme_ridges() +
  labs(x = "Tip %", y = NULL, title = "Tip Percentages by Day") +
  theme(legend.position = "none")'

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = "Restaurant tipping",
  includeCSS(here("styles.css")),
  sidebar = querychat_sidebar("chat"),
  useBusyIndicators(),

  # 🏷️ Header
  textOutput("show_title", container = h3),
  verbatimTextOutput("show_query") |>
    tagAppendAttributes(style = "max-height: 100px; overflow: auto;"),


  layout_columns(
    style = "min-height: 450px;",
    col_widths = c(6, 6, 12),


    # 📊 Scatter plot
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Total bill vs tip",
        span(
          actionLink(
            "interpret_scatter",
            icon_explain,
            class = "me-3 text-decoration-none",
            aria_label = "Explain scatter plot"
          ),
          popover(
            title = "Add a color variable",
            placement = "top",
            fa_i("ellipsis"),
            radioButtons(
              "scatter_color",
              NULL,
              c("none", "sex", "smoker", "day", "time"),
              inline = TRUE
            )
          )
        )
      ),
      plotlyOutput("scatterplot")
    ),

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
  # ✨ querychat ✨ -----------------------------------------------------------

  querychat <- querychat_server("chat", querychat_handle)

  # We don't normally need the chat object, but in this case, we want it so we
  # can pass it to explain_plot
  chat <- querychat$chat

  # The reactive data frame. Either returns the entire dataset, or filtered by
  # whatever querychat decided.
  #
  # querychat$df is already a reactive data frame, we're just creating an alias
  # to it called `tips_data` so the code below can be more readable.
  tips_data <- querychat$df

  # 🏷️ Header outputs --------------------------------------------------------

  output$show_title <- renderText({
    querychat$title()
  })

  output$show_query <- renderText({
    querychat$sql()
  })





  # 📊 Scatter plot ----------------------------------------------------------

  scatterplot <- reactive({
    req(nrow(tips_data()) > 0)

    color <- input$scatter_color

    data <- tips_data()

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

  observeEvent(input$interpret_scatter, {
    explain_plot(chat, scatterplot(), .ctx = ctx)
  })

  # 📊 Custom plot -----------------------------------------------------------

  # Holds the last successfully submitted code; starts with the default
  plot_code <- reactiveVal(default_plot_code)

  observeEvent(input$run_custom, {
    plot_code(input$custom_code)
  })

  custom_plot <- reactive({
    req(nrow(tips_data()) > 0)
    df <- tips_data()
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
    explain_plot(chat, custom_plot(), .ctx = ctx)
  })
}

shinyApp(ui, server)
