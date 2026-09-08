# Supabase logging ------------------------------------------------------------
#
# Mirrors everything the loggers write into Postgres (Supabase), so records
# survive a shinyapps.io container recycling — the local logs/ folder does not.
#
# All tables for this experiment design share one prefix, so they sit apart from
# the other designs in the same Supabase project:
#
#   dashboard_llm_chat_events         one row per chat event (query, exchange, …)
#   dashboard_llm_conversation_turns  the transcript, one row per turn
#   dashboard_llm_dashboard_events    one row per dashboard control change
#
# Change `chatapp.tbl_prefix` in app.R to rename all three at once.
#
# Files stay the fallback: every write here is best-effort and swallows its own
# errors, so an unreachable database degrades the app to files-only instead of
# breaking a participant's session mid-experiment.
#
# Credentials are the same ones surveydown reads, so one .env configures both
# the survey responses and these logs:
#
#   SD_HOST, SD_PORT, SD_DBNAME, SD_USER, SD_PASSWORD
#
# Use the Supabase *pooler* host (aws-0-<region>.pooler.supabase.com, user
# postgres.<project-ref>). The direct db.<ref>.supabase.co host is IPv6-only and
# unreachable from shinyapps.io, so it works locally and fails on deploy.

.sb <- new.env(parent = emptyenv())
.sb$pool <- NULL

#' Full table name for one of this design's logging tables
sb_table <- function(suffix) {
  paste0(getOption("chatapp.tbl_prefix", "dashboard_llm_"), suffix)
}

#' Label distinguishing this experiment design's rows from the other designs
#' sharing the same Supabase project
sb_design <- function() {
  getOption(
    "chatapp.design",
    basename(getOption("chatapp.log_dir", getwd()))
  )
}

#' Open the logging pool, or return NULL when the app isn't configured for a
#' database
#'
#' Safe to call unconditionally at startup: with no credentials set it reports
#' that logging is file-only and the app runs unchanged.
sb_connect <- function(env_file = ".env") {
  if (!is.null(.sb$pool)) return(invisible(.sb$pool))

  if (file.exists(env_file)) {
    try(dotenv::load_dot_env(env_file), silent = TRUE)
  }

  params <- list(
    host     = Sys.getenv("SD_HOST"),
    port     = Sys.getenv("SD_PORT"),
    dbname   = Sys.getenv("SD_DBNAME"),
    user     = Sys.getenv("SD_USER"),
    password = Sys.getenv("SD_PASSWORD")
  )

  if (any(!nzchar(unlist(params)))) {
    message("[supabase] no database credentials; logging to files only")
    return(invisible(NULL))
  }

  # surveydown hits the same problem: some networks need GSSAPI encryption
  # turned off explicitly before libpq will connect.
  open_pool <- function(gssencmode) {
    pool::dbPool(
      RPostgres::Postgres(),
      host       = params$host,
      port       = as.integer(params$port),
      dbname     = params$dbname,
      user       = params$user,
      password   = params$password,
      gssencmode = gssencmode,
      minSize    = 1,
      maxSize    = 5
    )
  }

  pool <- tryCatch(
    open_pool("auto"),
    error = function(e) {
      message("[supabase] connect failed (", conditionMessage(e),
              "); retrying with gssencmode='disable'")
      tryCatch(open_pool("disable"), error = function(e2) {
        message("[supabase] connect failed: ", conditionMessage(e2))
        NULL
      })
    }
  )

  if (is.null(pool)) {
    message("[supabase] logging to files only")
    return(invisible(NULL))
  }

  .sb$pool <- pool
  message("[supabase] connected; mirroring logs to the database")
  sb_create_tables()
  invisible(pool)
}

sb_disconnect <- function() {
  if (is.null(.sb$pool)) return(invisible(NULL))
  try(pool::poolClose(.sb$pool), silent = TRUE)
  .sb$pool <- NULL
  invisible(NULL)
}

#' Run `fn(con)` against a pooled connection inside a transaction
#'
#' Returns TRUE when the write landed. Never throws: a failure is logged to the
#' console and reported as FALSE, leaving the caller's file write as the record.
.sb_with_con <- function(fn, what = "write") {
  if (is.null(.sb$pool)) return(FALSE)
  tryCatch({
    pool::poolWithTransaction(.sb$pool, fn)
    TRUE
  }, error = function(e) {
    message("[supabase] ", what, " failed: ", conditionMessage(e))
    FALSE
  })
}

.sb_append <- function(table, df, what = table) {
  if (nrow(df) == 0) return(FALSE)
  .sb_with_con(function(con) DBI::dbAppendTable(con, table, df), what)
}

#' Create the logging tables if they don't exist yet
#'
#' Idempotent, so first run against a fresh Supabase project needs no manual
#' setup. Requires a role with DDL rights (the postgres user has it); if yours
#' doesn't, run the same statements once by hand in the Supabase SQL editor.
sb_create_tables <- function() {
  ddl <- c(
    sprintf("create table if not exists %s (
       id         bigserial primary key,
       design     text,
       session_id text not null,
       ts         timestamptz not null default now(),
       event      text not null,
       context    text,
       user_msg   text,
       assistant  text,
       app_msg    text,
       plot       text
     )", sb_table("chat_events")),
    sprintf("create index if not exists %s_session_idx
       on %s (session_id, ts)",
       sb_table("chat_events"), sb_table("chat_events")),

    sprintf("create table if not exists %s (
       id              bigserial primary key,
       design          text,
       conversation_id text not null,
       session_id      text not null,
       context         text,
       updated_at      timestamptz not null default now(),
       turn_n          integer,
       role            text,
       text            text,
       has_image       boolean
     )", sb_table("conversation_turns")),
    sprintf("create index if not exists %s_convo_idx
       on %s (conversation_id, turn_n)",
       sb_table("conversation_turns"), sb_table("conversation_turns")),

    sprintf("create table if not exists %s (
       id             bigserial primary key,
       design         text,
       session_id     text not null,
       shiny_token    text,
       ts             timestamptz not null default now(),
       event          text,
       viz_tab        text,
       scatter_x      text,
       scatter_y      text,
       bar_var        text,
       coffee_types text
     )", sb_table("dashboard_events")),
    sprintf("create index if not exists %s_session_idx
       on %s (session_id, ts)",
       sb_table("dashboard_events"), sb_table("dashboard_events"))
  )

  .sb_with_con(function(con) {
    for (stmt in ddl) DBI::dbExecute(con, stmt)
  }, "table setup")
}

# Writers ---------------------------------------------------------------------
#
# These take the same `fields` lists the loggers already build, so the file and
# database records can't drift apart.

.chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]

#' Mirror one chat event into <prefix>chat_events
sb_log_event <- function(fields) {
  if (is.null(.sb$pool)) return(invisible(FALSE))

  row <- data.frame(
    design     = sb_design(),
    session_id = .chr(fields$session_id),
    ts         = Sys.time(),
    event      = .chr(fields$event),
    context    = .chr(fields$context),
    user_msg   = .chr(fields$user),
    assistant  = .chr(fields$assistant),
    app_msg    = .chr(fields$app),
    plot       = .chr(fields$plot),
    stringsAsFactors = FALSE
  )

  invisible(.sb_append(sb_table("chat_events"), row, "chat_events insert"))
}

#' Replace the stored transcript for one conversation
#'
#' log_conversation() rewrites the whole transcript after every turn so the
#' record always matches what the model saw; this keeps that property by
#' deleting and re-inserting the conversation's rows in a single transaction.
sb_log_conversation <- function(conversation_id, records, context = "explain_panel") {
  if (is.null(.sb$pool) || length(records) == 0) return(invisible(FALSE))

  now <- Sys.time()

  df <- data.frame(
    design          = sb_design(),
    conversation_id = as.character(conversation_id),
    session_id      = as.character(conversation_id),
    context         = context,
    updated_at      = now,
    turn_n          = vapply(records, function(r) as.integer(r$n), integer(1)),
    role            = vapply(records, function(r) .chr(r$role), character(1)),
    text            = vapply(records, function(r) .chr(r$text), character(1)),
    has_image       = vapply(records, function(r) isTRUE(r$has_image), logical(1)),
    stringsAsFactors = FALSE
  )

  tbl <- sb_table("conversation_turns")
  ok <- .sb_with_con(function(con) {
    DBI::dbExecute(
      con,
      sprintf("delete from %s where conversation_id = $1", tbl),
      params = list(as.character(conversation_id))
    )
    DBI::dbAppendTable(con, tbl, df)
  }, "conversation_turns rewrite")

  invisible(ok)
}

#' Mirror one dashboard settings snapshot into <prefix>dashboard_events
sb_log_dashboard_event <- function(fields) {
  if (is.null(.sb$pool)) return(invisible(FALSE))

  row <- data.frame(
    design         = sb_design(),
    session_id     = .chr(fields$session_id),
    shiny_token    = .chr(fields$shiny_token),
    ts             = Sys.time(),
    event          = .chr(fields$event),
    viz_tab        = .chr(fields$viz_tab),
    scatter_x      = .chr(fields$scatter_x),
    scatter_y      = .chr(fields$scatter_y),
    bar_var        = .chr(fields$bar_var),
    coffee_types = .chr(fields$coffee_types),
    stringsAsFactors = FALSE
  )

  invisible(.sb_append(sb_table("dashboard_events"), row, "dashboard_events insert"))
}
