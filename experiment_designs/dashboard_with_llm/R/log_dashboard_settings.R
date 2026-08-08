# Dashboard logging -----------------------------------------------------------
#
# Logging of dashboard control changes: the chart type tab, the scatter x/y
# axes, the bar chart variable, and the dragon species filter.
#
# Each row is a full snapshot of the five controls plus an `event` column naming
# which one changed, so a participant's session can be replayed by ordering on
# `ts`. Because the assistant only ever interprets the plot on screen, this is
# also the record of what it was looking at for any given question — join to the
# chat log on session_id and timestamp.
#
# Rows go to Supabase (see R/supabase_log.R) and, unconditionally, to
# logs/dashboard_events.csv. The CSV is the fallback record and keeps this
# design self-contained when no database is configured.

#' Append one dashboard settings snapshot
#'
#' @param design Slug identifying the experiment arm.
#' @param session_id Survey session id (matches the surveydown responses table).
#' @param shiny_token `session$token`; constant per connection, so it joins
#'     rows together even if `session_id` changes mid-session on a resume.
#' @param event Which control changed: "init", "viz_tab", "scatter_x",
#'     "scatter_y", "bar_var", "dragon_species", or several joined with "+".
log_dashboard_event <- function(design, session_id, shiny_token, event,
                                viz_tab, scatter_x, scatter_y, bar_var,
                                dragon_species) {
  fields <- list(
    design = design, session_id = session_id, shiny_token = shiny_token,
    ts = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
    event = event, viz_tab = viz_tab,
    scatter_x = scatter_x, scatter_y = scatter_y,
    bar_var = bar_var, dragon_species = dragon_species
  )

  tryCatch({
    out_file <- log_path("dashboard_events.csv")
    row <- as.data.frame(fields, stringsAsFactors = FALSE)
    write_header <- !file.exists(out_file)
    write.table(row, file = out_file, append = !write_header, sep = ",",
                col.names = write_header, row.names = FALSE, quote = TRUE)
  }, error = function(e) {
    message("[dashboard] csv log failed: ", conditionMessage(e))
  })

  # Best-effort mirror; a no-op when no database is configured
  sb_log_dashboard_event(fields)

  invisible(TRUE)
}
