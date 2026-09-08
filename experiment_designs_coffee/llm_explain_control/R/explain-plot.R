library(ggplot2)
library(promises)
library(here)

#' Convert a plot object to a PNG data URI
#'
#' @param p The plot object; currently, plotly and ggplot2 are supported. Note
#'     that plotly requires Python, {reticulate}, and the PyPI packages {plotly}
#'     and {kaleido}.
plot_to_img_content <- function(p) {
  UseMethod("plot_to_img_content", p)
}

plot_to_img_content.plotly <- function(p) {
  # Create a temporary file
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))

  # Save the plot as an image
  save_image(p, tmp, width = 1200, height = 900)
  # resize = "none" on purpose: see plot_to_img_content.ggplot
  ellmer::content_image_file(tmp, resize = "none")
}

plot_to_img_content.ggplot <- function(p) {
  # Create a temporary file
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))

  # 8x6in at 150dpi = 1200x900px: sharp enough to read axis labels, and under
  # the 1568px edge above which the API downscales anyway.
  ggsave(tmp, p, width = 8, height = 6, units = "in", dpi = 150)

  # resize = "none" is load-bearing, not a default. Every other value routes
  # through {magick}, which is a Suggests of ellmer reached at runtime — so
  # rsconnect's static dependency scan never sees it and it is absent from the
  # deployed bundle. Hosted, the resize path fails and the assistant silently
  # ends up with no image. We size the PNG above, so there is nothing to resize.
  ellmer::content_image_file(tmp, resize = "none")
}

explain_plot <- function(
  chat,
  p,
  ...,
  .ctx = NULL,
  session = getDefaultReactiveDomain()
) {
  chat_id <- paste0("explain_plot_", sample.int(1e9, 1))
  chat <- chat$clone()
  session_id <- if (is.null(session)) NA else session$token
  # The modal runs on a clone, so its transcript is stored separately from the
  # main sidebar conversation
  convo_id <- paste0(session_id, "_", chat_id)

  img_content <- tryCatch(
    plot_to_img_content(p),
    error = function(e) {
      warning("plot_to_img_content failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(img_content)) {
    showNotification(
      "Sorry, the plot couldn't be prepared for the assistant.",
      type = "error"
    )
    return(invisible(NULL))
  }
  img_url <- paste0("data:", img_content@type, ";base64,", img_content@data)

  showModal(
    modalDialog(
      tags$button(
        type = "button",
        class = "btn-close d-block ms-auto mb-3",
        `data-bs-dismiss` = "modal",
        aria_label = "Close",
      ),
      tags$img(
        src = img_url,
        style = "max-width: min(100%, 400px);",
        class = "d-block border mx-auto mb-3"
      ),
      shinychat::chat_ui(chat_id),
      size = "l",
      easyClose = TRUE,
      title = NULL,
      footer = NULL,
    ) |>
      tagAppendAttributes(style = "--bs-modal-margin: 1.75rem;")
  )

  session$onFlushed(function() {
    user_msg <- "Interpret this plot, which is based on the current state of the data (i.e. with filtering applied, if any). Try to make specific observations if you can, but be conservative in drawing firm conclusions. Keep it brief, not more than 3-4 lines"
    log_query(user_msg, context = "explain_plot", session_id = session_id)
    log_plot_image(context = "explain_plot", session_id = session_id)
    stream <- chat$stream_async(user_msg, img_content)
    shinychat::chat_append(chat_id, stream) %...>%
      (function(...) {
        log_exchange(user_msg, chat$last_turn()@text,
                     context = "explain_plot", session_id = session_id)
        log_conversation(chat, convo_id, context = "explain_plot")
      }) %...!%
      (function(e) warning("explain_plot stream error: ", conditionMessage(e)))
  })

  observeEvent(session$input[[paste0(chat_id, "_user_input")]], {
    user_msg <- session$input[[paste0(chat_id, "_user_input")]]
    log_query(user_msg, context = "explain_plot", session_id = session_id)
    stream <- chat$stream_async(user_msg)
    shinychat::chat_append(chat_id, stream) %...>%
      (function(...) {
        log_exchange(user_msg, chat$last_turn()@text,
                     context = "explain_plot", session_id = session_id)
        log_conversation(chat, convo_id, context = "explain_plot")
      }) %...!%
      (function(e) warning("explain_plot stream error: ", conditionMessage(e)))
  })
}
