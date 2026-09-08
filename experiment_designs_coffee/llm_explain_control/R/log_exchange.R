# Logging ---------------------------------------------------------------------
#
# Everything is written to this experiment design's own `logs/` folder, so each
# design keeps its own record instead of sharing one file at the project root:
#
#   logs/chat_log.json / .txt     append-only event stream across all sessions
#   logs/conversation_<session>.* the full verbatim transcript for one session,
#                                 rewritten after every turn
#
# When a database is configured (see R/supabase_log.R) the same records are also
# mirrored to Supabase, which is what survives a shinyapps.io restart. The files
# are written first and unconditionally, so they remain the fallback record.
#
# `session_id` is `session$token`; pass it everywhere so events from concurrent
# participants can be told apart.

#' Path inside this app's logs/ folder, created on first use
#'
#' Anchored on the `chatapp.log_dir` option, which app.R pins to the app
#' directory at startup; falls back to the working directory, which Shiny sets
#' to the app directory anyway.
log_path <- function(...) {
  dir <- file.path(getOption("chatapp.log_dir", getwd()), "logs")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, ...)
}

.log_event <- function(fields, txt_lines) {
  tryCatch({
    entry <- jsonlite::toJSON(fields, auto_unbox = TRUE, null = "null")
    cat(entry, "\n", file = log_path("chat_log.json"), append = TRUE, sep = "")

    cat(
      paste0(
        "[", fields$timestamp, "] [", fields$session_id, "] [", fields$context,
        "] [", fields$event, "]\n", txt_lines, "---\n"
      ),
      file = log_path("chat_log.txt"), append = TRUE, sep = ""
    )
  }, error = function(e) {
    message("log_event failed: ", conditionMessage(e))
  })

  # Best-effort mirror; a no-op when no database is configured
  sb_log_event(fields)
}

.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

#' Log a message shown to the participant that the model never sees
#' (the opening greeting, an "that code didn't run" notice)
log_app_message <- function(text, event = "app_message",
                            context = "main_chat", session_id = NA) {
  .log_event(
    list(timestamp = .now(), session_id = session_id, event = event,
         context = context, app = as.character(text)),
    paste0("APP: ", text, "\n")
  )
}

#' Log the participant's message as soon as it is submitted, before the model
#' has replied — so a query is on record even if the stream fails
log_query <- function(user_msg, context = "main_chat", session_id = NA) {
  .log_event(
    list(timestamp = .now(), session_id = session_id, event = "query",
         context = context, user = as.character(user_msg)),
    paste0("USER: ", user_msg, "\n")
  )
}

#' Log a completed user/assistant round trip
log_exchange <- function(user_msg, assistant_text, context = "main_chat",
                         session_id = NA) {
  assistant <- if (is.null(assistant_text)) "" else as.character(assistant_text)
  .log_event(
    list(timestamp = .now(), session_id = session_id, event = "exchange",
         context = context, user = as.character(user_msg), assistant = assistant),
    paste0("USER: ", user_msg, "\nASSISTANT: ", assistant, "\n")
  )
}

#' Log that a rendered PNG of the on-screen plot was attached to a user message
#'
#' The transcript dump below already marks which turns carried an image; this
#' puts the same fact in the event stream, next to the query it was sent with.
log_plot_image <- function(context = "main_chat", session_id = NA) {
  .log_event(
    list(timestamp = .now(), session_id = session_id, event = "plot_image_sent",
         context = context),
    "IMAGE: current plot attached to the user's message\n"
  )
}

#' Log the ggplot code pulled out of an assistant reply, and whether it ran
log_plot_code <- function(code, error = NULL, context = "main_chat",
                          session_id = NA) {
  .log_event(
    list(timestamp = .now(), session_id = session_id,
         event = if (is.null(error)) "plot_rendered" else "plot_error",
         context = context, code = as.character(code),
         error = if (is.null(error)) "" else as.character(error)),
    paste0(
      "CODE: ", code, "\n",
      if (is.null(error)) "" else paste0("ERROR: ", error, "\n")
    )
  )
}

#' Dump the model's full turn history for this session
#'
#' The event stream above can miss things (a stream that errors halfway, an
#' image attached to a turn). This writes every turn ellmer is actually holding,
#' so the stored conversation always matches what the model saw.
#'
#' `session_id` identifies the conversation, not always the session: the modal in
#' explain-plot.R runs on a clone and passes `<token>_explain_plot_<n>` so its
#' transcript is stored separately from the sidebar's.
log_conversation <- function(chat, session_id, context = "main_chat") {
  tryCatch({
    turns <- chat$get_turns(include_system_prompt = TRUE)
    records <- lapply(seq_along(turns), function(i) {
      turn <- turns[[i]]
      list(
        n          = i,
        role       = turn@role,
        text       = paste(as.character(turn@text), collapse = ""),
        # An assistant turn holds text; a user turn may also carry the plot
        # image we handed the model. Record that it was there, not the bytes.
        n_contents = length(turn@contents),
        has_image  = any(vapply(
          turn@contents,
          function(x) inherits(x, "ellmer::ContentImage") ||
                      inherits(x, "ellmer::ContentImageInline") ||
                      grepl("Image", class(x)[1], fixed = TRUE),
          logical(1)
        ))
      )
    })

    # Mirror before the file writes, so a filesystem problem can't cost us the
    # database copy too
    sb_log_conversation(session_id, records, context = context)

    stem <- log_path(paste0("conversation_", session_id))

    jsonlite::write_json(
      list(
        session_id  = session_id,
        context     = context,
        updated_at  = .now(),
        n_turns     = length(records),
        turns       = records
      ),
      paste0(stem, ".json"),
      auto_unbox = TRUE, pretty = TRUE
    )

    cat(
      paste0(
        "# Conversation ", session_id, " (", context, ")\n",
        "# last updated ", .now(), "\n\n",
        paste(
          vapply(records, function(r) {
            paste0("[", r$n, "] ", toupper(r$role),
                   if (isTRUE(r$has_image)) " (+plot image)" else "", "\n",
                   r$text, "\n")
          }, character(1)),
          collapse = "\n"
        )
      ),
      file = paste0(stem, ".txt"), sep = ""
    )
  }, error = function(e) {
    message("log_conversation failed: ", conditionMessage(e))
  })
}
