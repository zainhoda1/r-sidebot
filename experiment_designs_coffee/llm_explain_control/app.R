# Libraries -------------------------------------------------------------------

library(shiny)
library(bslib)
library(fontawesome)
library(reactable)
library(here)
library(ggplot2)
library(ggridges)
library(dplyr)
library(promises)
library(ellmer)
library(shinychat)
library(surveydown)

# Resolve a file from this app's directory first, falling back to the project
# root. Lets each experiment design carry its own prompt/greeting/helpers while
# still sharing data/ and styles.css with the other designs.
app_file <- function(...) {
  local <- file.path(...)
  if (file.exists(local)) local else here::here(...)
}

# Chat logs and transcripts go in this design's own logs/ folder, not the shared
# project root. Pinned at startup, when Shiny has the working directory set to
# the app directory.
options(chatapp.log_dir = normalizePath(getwd(), winslash = "/", mustWork = FALSE))

# The dragon and coffee experiments share the same physical Supabase tables
# (chat_events, conversation_turns — see R/supabase_log.R), so this tags every
# row from this design distinctly — without it, sb_design() would fall back to
# this folder's basename, which is identical to the dragon experiment's
# llm_explain_control folder.
options(chatapp.design = "coffee_llm_explain_control")

source(app_file("R", "supabase_log.R"))
source(app_file("R", "log_exchange.R"))
source(app_file("R", "explain-plot.R"))

# Data & config ---------------------------------------------------------------

db <- sd_db_connect()

# Mirror the logs to Supabase when .env supplies credentials; a no-op otherwise,
# so the app still runs with files only. Uses the same SD_* variables as
# sd_db_connect() above.
sb_connect()
onStop(sb_disconnect)

farms <- readr::read_csv(app_file("data", "coffee.csv"))

system_prompt <- paste(readLines(app_file("prompt.md"), warn = FALSE), collapse = "\n")

icon_explain <- tags$img(src = "stars.svg")

# Plot code helpers -----------------------------------------------------------

#' Pull the single fenced R code block out of an assistant message
#'
#' Returns NULL when the message has no code block (i.e. the assistant is
#' discussing the current plot rather than replacing it) or when the block does
#' not parse.
extract_plot_code <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(NULL)

  m <- regexec("(?s)```[ \t]*[rR]?[ \t]*\r?\n(.*?)```", txt, perl = TRUE)
  groups <- regmatches(txt, m)[[1]]
  if (length(groups) < 2) return(NULL)

  code <- trimws(groups[2])
  if (!nzchar(code)) return(NULL)

  parsed <- tryCatch(parse(text = code), error = function(e) NULL)
  if (is.null(parsed)) return(NULL)

  code
}

#' Evaluate plot code in a sandbox whose only data is `df`
#'
#' ggplot defers most errors to print time, so build the plot here to surface
#' bad column names and aesthetics while we can still catch them.
eval_plot_code <- function(code, df) {
  env <- new.env(parent = globalenv())
  env$df <- df
  p <- eval(parse(text = code), envir = env)
  if (!inherits(p, "ggplot")) {
    stop("The code did not return a ggplot object.", call. = FALSE)
  }
  invisible(ggplot_build(p))
  p
}

#' Is the user asking about the plot on screen, rather than for a new one?
#'
#' Gates whether we attach a PNG of the current plot to the message: the
#' assistant only needs to see the rendered plot when it is being asked to read
#' it, and an image on every turn is wasted tokens. Deliberately errs towards
#' TRUE — a spurious image only costs tokens, a missing one leaves the assistant
#' interpreting from the code it wrote instead of what the participant sees.
wants_plot_interpretation <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(FALSE)

  patterns <- c(
    # Asked outright to read the plot
    "\\b(explain|interpret|describ|summari[sz]|analy[sz]|clarify|walk me through)",
    "\\b(tell me about|make sense of|thoughts on|comment on)\\b",
    # Questions pointed at what is on screen
    "\\bwhat\\b.*\\b(this|that|it|plot|chart|graph|figure|mean|show|tell|see|look|say)",
    "\\b(why|how come)\\b",
    "\\bwhich\\b.*\\b(highest|lowest|fastest|slowest|largest|smallest|biggest|widest|most|least|best|worst|more|less)\\b",
    "\\b(takeaway|insight|conclusion|conclude|pattern|trend|outlier|spread|relationship|correlat|difference|meaningful|significant|surprising|stands? out)",
    "\\b(this|that|the)\\s+(plot|chart|graph|figure|visuali[sz]ation)\\b"
  )

  grepl(paste(patterns, collapse = "|"), txt, ignore.case = TRUE, perl = TRUE)
}

message_plot <- function(txt, color = "grey40") {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = txt, size = 4.5, color = color) +
    theme_void()
}

# UI --------------------------------------------------------------------------

ui <- page_fillable(
  style = "background-color: rgb(248, 248, 248);",
  title = NULL,
  includeCSS(app_file("styles.css")),
  useBusyIndicators(),
  layout_columns(
    col_widths = c(6, 6),
    # Left: plot on top, assistant underneath
    layout_columns(
      col_widths = 12,
      row_heights = c(1, 1),
      card(
        style = "min-height: 300px;",
        card_header("Plot"),
        card_body(
          # Square plot: fill the card's height, derive width from it
          class = "d-flex justify-content-center",
          style = "min-height: 0;",
          tags$div(
            style = paste(
              "height: 100%; aspect-ratio: 1 / 1;",
              "max-width: 100%; margin: 0 auto;"
            ),
            plotOutput("custom_plot", height = "100%")
          )
        )
      ),
      card(
        style = "min-height: 300px;",
        card_header("Assistant"),
        card_body(
          class = "p-0",
          chat_ui("chat", height = "100%")
        ),
        card_footer(
          conditionalPanel(
            condition = "output.has_plot",
            actionLink(
              "interpret_custom",
              tagList(icon_explain, " Explain plot"),
              class = "text-decoration-none",
              aria_label = "Explain plot"
            )
          )
        )
      )
    ),
    # Right: the survey, full height
    card(
      style = "min-height: 450px; overflow-y: auto;",
      sd_ui()
    )
  )
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Dashboard chat ---------------------------------------------------------

  chat <- chat_anthropic(system_prompt = system_prompt, model = "claude-sonnet-4-6")
  session_id <- session$token

  session$onFlushed(function() {
    greeting <- paste(readLines(app_file("greeting.md"), warn = FALSE), collapse = "\n")
    chat_append("chat", greeting)
    log_app_message(greeting, event = "greeting", session_id = session_id)
  }, once = TRUE)

  farms_data <- reactive(farms)

  # Code for the plot currently on screen; NULL until the assistant writes one
  plot_code <- reactiveVal(NULL)
  # Code whose rendered image has already been shown to the assistant
  code_seen_by_llm <- reactiveVal(NULL)

  observeEvent(input$chat_user_input, {
    user_msg <- input$chat_user_input
    log_query(user_msg, session_id = session_id)

    # When the participant asks the assistant to read the plot, hand it a PNG of
    # what is actually on screen, so the interpretation comes from the rendered
    # figure and not from the code the assistant remembers writing. Sent once per
    # plot: ellmer keeps the image in the turn history, so later questions about
    # the same plot can still see it.
    args <- list(user_msg)
    code <- plot_code()
    p    <- current_plot()
    if (!is.null(p) && wants_plot_interpretation(user_msg) &&
        !identical(code, code_seen_by_llm())) {
      img <- tryCatch(
        plot_to_img_content(p),
        error = function(e) {
          # Losing the image degrades the answer without breaking the turn, so
          # we carry on — but say so in the log, or a hosted failure looks
          # exactly like a working app giving vaguer answers.
          warning("plot_to_img_content failed: ", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(img)) {
        args <- c(args, list(img))
        code_seen_by_llm(code)
        log_plot_image(session_id = session_id)
      }
    }

    stream <- do.call(chat$stream_async, args)
    chat_append("chat", stream) %...>%
      (function(...) {
        assistant_txt <- chat$last_turn()@text
        log_exchange(user_msg, assistant_txt, session_id = session_id)
        log_conversation(chat, session_id)

        new_code <- extract_plot_code(assistant_txt)
        if (!is.null(new_code)) {
          plot_code(new_code)
          log_plot_code(new_code, session_id = session_id)
        }
      }) %...!%
      (function(e) warning("chat stream error: ", conditionMessage(e)))
  })

  # --- Plot -------------------------------------------------------------------

  # NULL before the first plot; otherwise list(plot = <ggplot>, error = <chr|NULL>)
  plot_result <- reactive({
    code <- plot_code()
    if (is.null(code)) return(NULL)

    df <- farms_data()
    if (nrow(df) == 0) return(NULL)

    tryCatch(
      list(plot = eval_plot_code(code, df), error = NULL),
      error = function(e) {
        msg <- conditionMessage(e)
        list(plot = message_plot(paste("Error:", msg), color = "red"), error = msg)
      }
    )
  })

  # Only the successfully rendered plot, for anything that hands the plot to the
  # assistant as an image
  current_plot <- reactive({
    res <- plot_result()
    if (is.null(res) || !is.null(res$error)) NULL else res$plot
  })

  # Tell the participant when the assistant's code failed, so they can ask for a
  # fix instead of staring at a stale plot
  observeEvent(plot_result(), {
    res <- plot_result()
    if (is.null(res) || is.null(res$error)) return()
    log_plot_code(plot_code(), error = res$error, session_id = session_id)
    notice <- paste0("⚠️ That code didn't run: ", res$error)
    chat_append("chat", notice)
    log_app_message(notice, event = "plot_error_notice", session_id = session_id)
  })

  output$custom_plot <- renderPlot({
    res <- plot_result()
    if (is.null(res)) {
      message_plot("Ask the assistant to create a plot")
    } else {
      res$plot
    }
  })

  output$has_plot <- reactive(!is.null(current_plot()))
  outputOptions(output, "has_plot", suspendWhenHidden = FALSE)

  observeEvent(input$interpret_custom, {
    p <- current_plot()
    req(!is.null(p))
    explain_plot(chat, p)
  })

  # --- Survey server ----------------------------------------------------------

  sd_skip_if()
  sd_server(db = db)
}

shinyApp(ui, server)
