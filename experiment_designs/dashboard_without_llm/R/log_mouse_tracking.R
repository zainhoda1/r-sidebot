# Logging of raw mouse-movement samples over the survey panel.
#
# Each row is one throttled mousemove sample (see www/mouse_tracking.js):
# pixel position relative to the survey container's top-left corner, plus
# the container's current size so positions can be normalized to 0-1 later.
# Stored locally as a CSV for now (no database table).

mouse_tracking_file <- function() here::here("logs", "mouse_tracking.csv")
mouse_hover_file    <- function() here::here("logs", "mouse_hover.csv")

#' Ensure the logs directory exists
mouse_tracking_init <- function() {
  dir.create(here::here("logs"), showWarnings = FALSE, recursive = TRUE)
  invisible(TRUE)
}

#' Append one row to a CSV, writing the header only on first write
append_row <- function(out_file, row) {
  write_header <- !file.exists(out_file)
  write.table(row, file = out_file, append = !write_header, sep = ",",
              col.names = write_header, row.names = FALSE, quote = TRUE)
}

#' Append one mouse-move sample to the local CSV
#'
#' @param session_id Survey session id.
#' @param shiny_token `session$token`; constant per connection.
#' @param x,y Pixel position relative to the survey container's top-left corner.
#' @param width,height Current size of the survey container (for normalizing x/y later).
#' @param client_x,client_y Pixel position relative to the browser viewport.
#' @param client_ts Client-side timestamp (ms since epoch) when the sample was taken.
log_mouse_move <- function(session_id, shiny_token, x, y, width, height,
                            client_x, client_y, client_ts) {
  tryCatch({
    row <- data.frame(
      session_id  = session_id,
      shiny_token = shiny_token,
      ts          = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
      client_ts   = client_ts,
      x = x, y = y, width = width, height = height,
      client_x = client_x, client_y = client_y,
      stringsAsFactors = FALSE
    )
    append_row(mouse_tracking_file(), row)
    invisible(TRUE)
  }, error = function(e) {
    message("[mouse_tracking] log failed: ", conditionMessage(e))
    invisible(FALSE)
  })
}

#' Append one mouse enter/leave event for the survey container
#'
#' @param hovering TRUE on mouseenter, FALSE on mouseleave.
#' @param client_ts Client-side timestamp (ms since epoch) of the event.
log_mouse_hover <- function(session_id, shiny_token, hovering, client_ts) {
  tryCatch({
    row <- data.frame(
      session_id  = session_id,
      shiny_token = shiny_token,
      ts          = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
      client_ts   = client_ts,
      hovering    = hovering,
      stringsAsFactors = FALSE
    )
    append_row(mouse_hover_file(), row)
    invisible(TRUE)
  }, error = function(e) {
    message("[mouse_tracking] log failed: ", conditionMessage(e))
    invisible(FALSE)
  })
}
