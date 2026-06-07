library(shiny)
library(bslib)
library(fontawesome)
library(here)
library(ggplot2)
library(ggridges)
library(dplyr)
library(querychat)
library(shinychat)
library(ellmer)
library(jsonlite)

dinosaurs <- readr::read_csv(here("data", "dinosaurs.csv"))

querychat_handle <- querychat_init(
  dinosaurs,
  greeting = readLines(here("greeting.md"), warn = FALSE)
)

icon_explain <- tags$img(src = "stars.svg")

NUMERIC_COLS  <- c("weight_kg", "bill_length_cm", "speed_kph", "claw_length_cm")
CATEGORY_COLS <- c("species", "habitat", "sex")

AXIS_PROMPT <- paste0(
  "You help users change the axes of a ridge plot for a dinosaur dataset. ",
  "Numeric columns (for x-axis): weight_kg, bill_length_cm, speed_kph, claw_length_cm. ",
  "Categorical columns (for y-axis / grouping): species, habitat, sex. ",
  "Given the user's request, return ONLY valid JSON with no markdown: ",
  "{\"x\": \"<column_or_null>\", \"y\": \"<column_or_null>\"} — ",
  "use null for any axis the user did not mention."
)

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = "Dinosaur Explorer",
  includeCSS(here("styles.css")),
  sidebar = querychat_sidebar("chat"),
  useBusyIndicators(),

  layout_columns(
    col_widths = 12,
    row_heights = c("50vh", "50vh"),

    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        textOutput("ridge_title", inline = TRUE),
        actionLink(
          "interpret_ridge",
          icon_explain,
          class = "text-decoration-none",
          aria_label = "Explain ridge plot"
        )
      ),
      plotOutput("weight_ridge", height = "270px"),
      card_footer(
        style = "padding: 0.5rem 1rem;",
        div(
          class = "d-flex gap-2 align-items-end",
          div(
            class = "flex-grow-1",
            textInput(
              "axis_request", NULL,
              placeholder = 'e.g. "show speed on x, split by habitat"',
              width = "100%"
            )
          ),
          actionButton("update_axes", "Update", class = "btn btn-primary btn-sm mb-3")
        )
      )
    ),

    card(
      card_header("Literature Review"),
      card_body(
        style = "overflow-y: auto;",
        uiOutput("random_text")
      )
    )
  )
)

server <- function(input, output, session) {
  querychat <- querychat_server("chat", querychat_handle)
  chat      <- querychat$chat
  dino_data <- querychat$df

  x_col <- reactiveVal("weight_kg")
  y_col <- reactiveVal("species")

  observeEvent(input$update_axes, {
    req(nchar(trimws(input$axis_request)) > 0)

    # Ask the LLM to parse the axis request
    spec <- tryCatch({
      ac  <- ellmer::chat_anthropic(system_prompt = AXIS_PROMPT)
      raw <- ac$chat(input$axis_request)
      raw <- gsub("```json\\s*|```\\s*", "", trimws(raw))
      jsonlite::fromJSON(raw)
    }, error = function(e) NULL)

    # Keyword fallback: match natural words to column names
    if (is.null(spec)) {
      q <- tolower(input$axis_request)

      x_new <- if      (grepl("speed",  q)) "speed_kph"
                else if (grepl("weight", q)) "weight_kg"
                else if (grepl("bill",   q)) "bill_length_cm"
                else if (grepl("claw",   q)) "claw_length_cm"
                else NULL

      y_new <- if      (grepl("species",       q)) "species"
                else if (grepl("habitat",       q)) "habitat"
                else if (grepl("sex|gender|male|female", q)) "sex"
                else NULL

      spec <- list(x = x_new, y = y_new)
    }

    if (!is.null(spec$x) && isTRUE(spec$x %in% NUMERIC_COLS))  x_col(spec$x)
    if (!is.null(spec$y) && isTRUE(spec$y %in% CATEGORY_COLS)) y_col(spec$y)

    updateTextInput(session, "axis_request", value = "")
  })

  output$ridge_title <- renderText({
    x_label <- gsub("_", " ", x_col()) |> tools::toTitleCase()
    y_label <- gsub("_", " ", y_col()) |> tools::toTitleCase()
    paste(x_label, "by", y_label)
  })

  weight_ridge <- reactive({
    req(nrow(dino_data()) > 0)
    df <- dino_data() |> filter(!is.na(.data[[y_col()]]))

    ggplot(df, aes_string(x = x_col(), y = y_col(), fill = y_col())) +
      geom_density_ridges(scale = 2.5, rel_min_height = 0.01, alpha = 0.7) +
      scale_fill_viridis_d() +
      theme_ridges() +
      labs(
        x = gsub("_", " ", x_col()) |> tools::toTitleCase(),
        y = NULL
      ) +
      theme(legend.position = "none")
  })

  output$weight_ridge <- renderPlot({
    weight_ridge()
  })

  observeEvent(input$interpret_ridge, {
    explain_plot(chat, weight_ridge())
  })

  output$random_text <- renderUI({
    lines      <- readLines(here("data", "random_text.txt"), warn = FALSE)
    paragraphs <- lines[nzchar(trimws(lines))]
    lapply(paragraphs, p)
  })
}

shinyApp(ui, server)
