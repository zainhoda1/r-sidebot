# Logging of dashboard control changes: the scatter x/y axes, the bar chart
# variable, and the dragon species filter.
#
# Each row is a full snapshot of the four controls plus an `event` column
# naming which one changed, so a participant's session can be replayed by
# ordering on `ts`.

dashboard_events_table <- "dashboard_events"

#' Create the dashboard_events table if it does not already exist
#'
#' Safe to call on every app start; does nothing when `db` is NULL (preview
#' mode), in which case logging falls back to a local CSV.
dashboard_events_init <- function(db) {
  if (is.null(db) || is.null(db$db)) {
    message("[dashboard] no database connection; logging to CSV instead")
    return(invisible(FALSE))
  }
  tryCatch({
    DBI::dbExecute(db$db, sprintf("
      CREATE TABLE IF NOT EXISTS public.%s (
        id             bigserial PRIMARY KEY,
        design         text,
        session_id     text NOT NULL,
        shiny_token    text,
        ts             timestamptz NOT NULL DEFAULT now(),
        event          text,
        scatter_x      text,
        scatter_y      text,
        bar_var        text,
        dragon_species text
      )", dashboard_events_table))
    # For tables created before bar_var was logged.
    DBI::dbExecute(db$db, sprintf(
      "ALTER TABLE public.%s ADD COLUMN IF NOT EXISTS bar_var text",
      dashboard_events_table))
    DBI::dbExecute(db$db, sprintf(
      "CREATE INDEX IF NOT EXISTS %s_session_idx ON public.%s (session_id)",
      dashboard_events_table, dashboard_events_table))
    message("[dashboard] table public.", dashboard_events_table, " ready")
    invisible(TRUE)
  }, error = function(e) {
    message("[dashboard] table init failed: ", conditionMessage(e))
    invisible(FALSE)
  })
}

#' Append one dashboard settings snapshot
#'
#' @param db Result of `sd_db_connect()`, or NULL for CSV fallback.
#' @param design Slug identifying the experiment arm.
#' @param session_id Survey session id (matches the surveydown responses table).
#' @param shiny_token `session$token`; constant per connection, so it joins
#'     rows together even if `session_id` changes mid-session on a resume.
#' @param event Which control changed: "init", "scatter_x", "scatter_y",
#'     "bar_var", "dragon_species", or several joined with "+".
log_dashboard_event <- function(db, design, session_id, shiny_token, event,
                                scatter_x, scatter_y, bar_var, dragon_species) {
  tryCatch({
    if (is.null(db) || is.null(db$db)) {
      out_file <- here::here("dashboard_events.csv")
      row <- data.frame(
        design = design, session_id = session_id, shiny_token = shiny_token,
        ts = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
        event = event, scatter_x = scatter_x, scatter_y = scatter_y,
        bar_var = bar_var, dragon_species = dragon_species,
        stringsAsFactors = FALSE
      )
      write_header <- !file.exists(out_file)
      write.table(row, file = out_file, append = !write_header, sep = ",",
                  col.names = write_header, row.names = FALSE, quote = TRUE)
      return(invisible(TRUE))
    }

    DBI::dbExecute(db$db, sprintf("
      INSERT INTO public.%s
        (design, session_id, shiny_token, event,
         scatter_x, scatter_y, bar_var, dragon_species)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8)", dashboard_events_table),
      params = list(design, session_id, shiny_token, event,
                    scatter_x, scatter_y, bar_var, dragon_species))
    message("[dashboard] logged ", event, ": ", scatter_x, " / ", scatter_y,
            " / ", bar_var, " / ", dragon_species)
    invisible(TRUE)
  }, error = function(e) {
    message("[dashboard] log failed: ", conditionMessage(e))
    invisible(FALSE)
  })
}
