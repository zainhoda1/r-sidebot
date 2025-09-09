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

tips <- readr::read_csv(here("tips.csv")) |>
  mutate(percent = round((tip / total_bill) * 100, 2))

querychat_handle <- querychat_init(
  tips,
  # This is the greeting that should initially appear in the sidebar when the app
  # loads.
  greeting = readLines(here("greeting.md"), warn = FALSE)
)

icon_explain <- tags$img(src = "stars.svg")

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = "Restaurant tipping",
  includeCSS(here("styles.css")),
  # sidebar = list(
  #   #querychat_sidebar("chat"),
  #   sliderInput("slider", "Party_size", min = 1, max = 6, value = 2),
  #   # Add more sidebar elements here as needed
  #   selectInput("select", "Meal Time",
  #               choices = c("Dinner", "Lunch")),
  #   checkboxInput("checkbox", "Smoker", value = TRUE)
  # ),
  sidebar = querychat_sidebar("chat"),
  useBusyIndicators(),



  layout_columns(
    style = "min-height: 350px;",
    col_widths = c(6, 6, 12),


    # 📊 Ridge plot
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Tip percentages",
        span(
          actionLink(
            "interpret_ridge",
            icon_explain,
            class = "me-3 text-decoration-none",
            aria_label = "Explain ridgeplot"
          ),
          popover(
            title = "Split ridgeplot",
            placement = "top",
            fa_i("ellipsis"),
            radioButtons(
              "tip_perc_y",
              "Split by",
              c("sex", "smoker", "day", "time"),
              "day",
              inline = TRUE
            )
          )
        )
      ),
      plotOutput("tip_perc")
    ),
  )
)

server <- function(input, output, session) {
  # ✨ querychat ✨ -----------------------------------------------------------

  querychat <- querychat_server("chat", querychat_handle)

  # We don't normally need the chat object, but in this case, we want it so we
  # can pass it to explain_plot


  # The reactive data frame. Either returns the entire dataset, or filtered by
  # whatever querychat decided.
  #
  # querychat$df is already a reactive data frame, we're just creating an alias
  # to it called `tips_data` so the code below can be more readable.

  tips_data <- querychat$df  # commnted out code

  # Create filtered data based on sidebar inputs
  # tips_data1 <- reactive({
  #   # Start with the querychat filtered data
  #   df <- querychat$df()
  #
  #   # Apply sidebar filters
  #   df <- df |>
  #     filter(size <= input$slider) |>  # Filter by party size
  #     filter(time == input$select)     # Filter by meal time
  #
  #   # Apply smoker filter if checkbox is checked
  #   if (!input$checkbox) {
  #     df <- df |> filter(smoker == "No")
  #   }
  #
  #   return(df)
  # })


  # 📊 Ridge plot ------------------------------------------------------------



  tip_perc <- reactive({
    req(nrow(tips_data()) > 0)

    df <- tips_data() |>
     # filter(size <= input$slider) |>  # Filter by party size
     # filter(time == input$select) |>    # Filter by meal time
      mutate(percent = tip / total_bill)

    ggplot(
      df,
      aes_string(x = "percent", y = input$tip_perc_y, fill = input$tip_perc_y)
    ) +
      geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.6) +
      scale_fill_viridis_d() +
      theme_ridges() +
      labs(x = "Percent", y = NULL, title = "Tip Percentages by Day") +
      theme(legend.position = "none")
  })

  output$tip_perc <- renderPlot({
    tip_perc()
  })

  observeEvent(input$interpret_ridge, {
    explain_plot(chat, tip_perc(), .ctx = ctx)
  })
}

shinyApp(ui, server)
