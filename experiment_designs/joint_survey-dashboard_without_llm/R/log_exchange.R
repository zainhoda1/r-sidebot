log_query <- function(user_msg, context = "main_chat") {
  tryCatch({
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    log_file <- here::here("chat_log.json")
    entry <- jsonlite::toJSON(
      list(timestamp = ts, event = "query", context = context, user = as.character(user_msg)),
      auto_unbox = TRUE
    )
    cat(entry, "\n", file = log_file, append = TRUE, sep = "")

    txt_file <- here::here("chat_log.txt")
    cat(paste0("[", ts, "] [", context, "] [query]\nUSER: ", user_msg, "\n---\n"),
        file = txt_file, append = TRUE, sep = "")
  }, error = function(e) {
    message("log_query failed: ", conditionMessage(e))
  })
}

log_exchange <- function(user_msg, assistant_text, context = "main_chat") {
  tryCatch({
    ts        <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    assistant <- if (is.null(assistant_text)) "" else as.character(assistant_text)

    log_file <- here::here("chat_log.json")
    entry <- jsonlite::toJSON(
      list(
        timestamp = ts,
        event     = "exchange",
        context   = context,
        user      = as.character(user_msg),
        assistant = assistant
      ),
      auto_unbox = TRUE
    )
    cat(entry, "\n", file = log_file, append = TRUE, sep = "")

    txt_file <- here::here("chat_log.txt")
    cat(paste0(
      "[", ts, "] [", context, "] [exchange]\n",
      "USER: ", user_msg, "\n",
      "ASSISTANT: ", assistant, "\n",
      "---\n"
    ), file = txt_file, append = TRUE, sep = "")
  }, error = function(e) {
    message("log_exchange failed: ", conditionMessage(e))
  })
}
