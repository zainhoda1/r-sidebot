# Parse dashboard_with_llm survey data ----------------------------------------
#
# Builds a per-participant, per-question record of how people used the
# dashboard + LLM chat panel while answering the coffee survey.
#
# Source tables (exported from Supabase into "data analysis/"; see
# R/supabase_log.R for how they're written):
#
#   design_coffee_dashboard_with_llm_rows.csv   surveydown responses + timing
#                                                (time_p_<page> = page arrival,
#                                                time_q_<question> = when that
#                                                answer was registered)
#   dashboard_llm_dashboard_events_rows.csv     one row per dashboard control
#                                                change (full snapshot)
#   dashboard_llm_chat_events_rows.csv          one row per chat query/exchange
#
# dashboard_llm_conversation_turns_rows.csv (the full ellmer transcript,
# including the system prompt) isn't read here — chat_events already carries
# every user/assistant pair with a timestamp, which is all this needs.
#
# Output (written to analysis/output/):
#
#   question_interactions.csv   one row per session x question — the main
#                                deliverable (view, typed text, LLM response,
#                                copy-paste flag, timing)
#   chat_exchanges_detail.csv   one row per chat exchange, tagged with the
#                                question window it falls into
#   session_summary.csv         one row per session — session-level totals

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(here)

# here() anchors on dashboard_with_llm.Rproj at the project root, so these
# paths resolve the same regardless of where the script is launched from.
data_dir   <- here("data analysis")
output_dir <- here("analysis", "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Timestamps come in two shapes: surveydown's "...UTC" and Postgres's
# "...+00" (both already UTC) — strip either suffix and parse as UTC.
parse_ts <- function(x) {
  x <- str_trim(str_remove(x, "\\s*(UTC|\\+00)$"))
  ymd_hms(x, tz = "UTC", quiet = TRUE)
}

# 1. Load ----------------------------------------------------------------------

responses  <- read_csv(file.path(data_dir, "design_coffee_dashboard_with_llm_rows.csv"), show_col_types = FALSE)
dash_events <- read_csv(file.path(data_dir, "dashboard_llm_dashboard_events_rows.csv"), show_col_types = FALSE) |>
  mutate(ts = parse_ts(ts))
chat_events <- read_csv(file.path(data_dir, "dashboard_llm_chat_events_rows.csv"), show_col_types = FALSE) |>
  mutate(ts = parse_ts(ts))

# 2. Question metadata -----------------------------------------------------
#
# Hand-built from survey.qmd: which page each question lives on, its exact
# label (for the copy-paste check), and whether the dashboard is relevant to
# it at all (the demographics questions don't need the dashboard, but the
# chat/dashboard panel stays on screen the whole session, so events can still
# land in their window).

question_meta <- tribble(
  ~question_id,                     ~page,           ~label,                                                                                                                                                                     ~needs_dashboard,
  "q_familiarity",                  "familiarity",   "Before today, how familiar were you with coffee growing or coffee sourcing?",                                                                                             FALSE,
  "practice_size_age",              "practice",      "Across all farms, how are farm size and tree age related?",                                                                                                               TRUE,
  "q1_altitude",                    "q1",            "A cooperative is buying land to plant a new coffee plot, and wants the coffee to sell for the highest possible price per kilogram. Based on the data, should they buy land at a higher or a lower altitude?", TRUE,
  "q2_size_altitude",               "q2",            "Among farms of the same size, which varietal tends to be grown at a higher altitude — Caturra or Typica?",                                                                  TRUE,
  "q3_price",                       "q3",            "Which varietal has the highest average price per kilogram?",                                                                                                              TRUE,
  "q4_altitude_varietal",           "q4",            "Which varietal is grown at the highest average altitude?",                                                                                                                TRUE,
  "q5_yield_price",                 "q5",            "How are yield per hectare and price per kilogram related?",                                                                                                               TRUE,
  "q_confidence",                   "wrap-up",       "How confident are you in your answers?",                                                                                                                                   FALSE,
  "q_reasoning",                    "wrap-up",       "How did you decide on your answers to these questions? Please briefly describe your reasoning.",                                                                          FALSE,
  "birth_year",                     "demographics",  "What is your year of birth?",                                                                                                                                             FALSE,
  "gender",                         "demographics",  "What is your gender?",                                                                                                                                                    FALSE,
  "ethnicity",                      "demographics",  "Are you of Hispanic, Latino or Spanish origin?",                                                                                                                          FALSE,
  "race",                           "demographics",  "Which race best describes you?",                                                                                                                                          FALSE,
  "employment",                     "demographics",  "Which of the following describes your current employment status?",                                                                                                       FALSE,
  "hh_income",                      "demographics",  "What is your annual household income?",                                                                                                                                   FALSE,
  "education",                      "demographics",  "What is the highest level of education you have completed?",                                                                                                             FALSE,
  "political_view",                 "demographics",  "How would you describe your political views?",                                                                                                                           FALSE,
  "party_voting",                   "demographics",  "Which political party do you typically vote for?",                                                                                                                       FALSE,
  "home_zipcode",                   "demographics",  "What is the zip code of your primary residence?",                                                                                                                        FALSE,
  "attention_check_survey_content", "demographics",  "Please briefly describe what this survey is about",                                                                                                                       FALSE
)

# Chronological page order as laid out in survey.qmd (pages a participant
# without skip logic moves through in order).
page_order <- c("welcome", "familiarity", "practice", "q1", "q2", "q3", "q4", "q5",
                 "wrap-up", "demographics", "end")

# 3. Page arrival timeline (time_p_<page>) --------------------------------------
#
# page_end is the next page's arrival time — i.e. the full time the
# participant spent with that page on screen, however long they lingered
# after answering before clicking "Continue".

page_timeline <- responses |>
  select(session_id, starts_with("time_p_")) |>
  pivot_longer(-session_id, names_to = "page", names_prefix = "time_p_", values_to = "page_start") |>
  mutate(page_start = parse_ts(page_start)) |>
  filter(!is.na(page_start)) |>
  group_by(session_id) |>
  arrange(page_start, .by_group = TRUE) |>
  mutate(page_end = lead(page_start)) |>
  ungroup() |>
  mutate(page = factor(page, levels = page_order))

# 4. Question answer timeline (time_q_<question>) + the answers themselves -----

question_times <- responses |>
  select(session_id, starts_with("time_q_")) |>
  pivot_longer(-session_id, names_to = "question_id", names_prefix = "time_q_", values_to = "answered_at") |>
  mutate(answered_at = parse_ts(answered_at))

question_answers <- responses |>
  select(session_id, all_of(question_meta$question_id)) |>
  pivot_longer(-session_id, names_to = "question_id", values_to = "response_value",
               values_transform = as.character)

question_timeline <- question_meta |>
  left_join(question_answers, by = "question_id", relationship = "many-to-many") |>
  left_join(question_times, by = c("session_id", "question_id")) |>
  left_join(page_timeline |> select(session_id, page, page_start, page_end),
            by = c("session_id", "page" = "page")) |>
  # Not everyone reached every page (some sessions end early) — drop rows with
  # no page arrival at all rather than report bogus zero-length windows.
  filter(!is.na(page_start))

# 5. Interaction helpers ---------------------------------------------------

# Last dashboard snapshot at or before `at` (session-wide lookback, not
# bounded to the question's page — a view set on an earlier question is still
# what's on screen if nothing changed since).
last_dashboard_state <- function(sid, at) {
  if (is.na(at)) return(NULL)
  rows <- dash_events |> filter(session_id == sid, ts <= at)
  if (nrow(rows) == 0) return(NULL)
  rows |> arrange(desc(ts)) |> slice(1)
}

describe_view <- function(row) {
  if (is.null(row)) return(NA_character_)
  if (row$viz_tab == "Bar") {
    sprintf("Bar | varietals: %s | variable: %s", row$coffee_types, row$bar_var)
  } else {
    sprintf("Scatter | varietals: %s | x: %s | y: %s", row$coffee_types, row$scatter_x, row$scatter_y)
  }
}

# All events of a given kind for a session within [start, end); end = NA means
# "through the last event" (used for the final page in a session).
events_in_window <- function(df, sid, start, end) {
  out <- df |> filter(session_id == sid, ts >= start)
  if (!is.na(end)) out <- out |> filter(ts < end)
  out |> arrange(ts)
}

# Normalize for the copy-paste check: lowercase, strip punctuation, collapse
# whitespace. A message counts as "copy-pasted" if the normalized question
# text sits inside it, or if it's a near-exact edit-distance match — either
# catches a pasted question with a word or two added/removed.
normalize_text <- function(x) {
  x |> str_to_lower() |> str_replace_all("[^a-z0-9 ]", " ") |> str_squish()
}

is_copy_paste <- function(user_msg, label) {
  if (is.na(user_msg) || !nzchar(user_msg)) return(FALSE)
  a <- normalize_text(user_msg)
  b <- normalize_text(label)
  if (!nzchar(a) || !nzchar(b)) return(FALSE)
  if (str_detect(a, fixed(b)) || str_detect(b, fixed(a))) return(TRUE)
  sim <- 1 - adist(a, b)[1, 1] / max(nchar(a), nchar(b))
  sim >= 0.85
}

# 6. Build the per-question record ----------------------------------------------

question_rows <- question_timeline |>
  mutate(row_id = row_number()) |>
  group_split(row_id) |>
  map_dfr(function(r) {
    sid    <- r$session_id
    start  <- r$page_start
    end    <- r$page_end
    at     <- r$answered_at

    dash_in_window <- events_in_window(dash_events, sid, start, end)
    chat_all       <- events_in_window(chat_events, sid, start, end) |> filter(event == "exchange")

    view_at_answer <- last_dashboard_state(sid, coalesce(at, end, start))

    # "Last interaction" = most recent dashboard change or LLM exchange at or
    # before the moment the answer was registered, looked up session-wide (an
    # earlier question's view/answer still counts if nothing changed since).
    last_dash_ts <- if (!is.na(at)) {
      x <- dash_events |> filter(session_id == sid, ts <= at) |> pull(ts)
      if (length(x)) max(x) else as.POSIXct(NA)
    } else as.POSIXct(NA)
    last_chat_ts <- if (!is.na(at)) {
      x <- chat_events |> filter(session_id == sid, event == "exchange", ts <= at) |> pull(ts)
      if (length(x)) max(x) else as.POSIXct(NA)
    } else as.POSIXct(NA)
    last_interaction_ts <- suppressWarnings(max(c(last_dash_ts, last_chat_ts), na.rm = TRUE))
    if (!is.finite(last_interaction_ts)) last_interaction_ts <- as.POSIXct(NA)

    typed_texts  <- chat_all$user_msg
    llm_texts    <- chat_all$assistant
    copy_flags   <- map_lgl(typed_texts, is_copy_paste, label = r$label)

    tibble(
      session_id           = sid,
      question_id          = r$question_id,
      page                 = as.character(r$page),
      question_label       = r$label,
      needs_dashboard      = r$needs_dashboard,
      response_value       = r$response_value,

      # 1) what they saw
      dashboard_view_at_answer = describe_view(view_at_answer),
      viz_tab    = if (!is.null(view_at_answer)) view_at_answer$viz_tab else NA_character_,
      scatter_x  = if (!is.null(view_at_answer)) view_at_answer$scatter_x else NA_character_,
      scatter_y  = if (!is.null(view_at_answer)) view_at_answer$scatter_y else NA_character_,
      bar_var    = if (!is.null(view_at_answer)) view_at_answer$bar_var else NA_character_,
      varietals_shown = if (!is.null(view_at_answer)) view_at_answer$coffee_types else NA_character_,
      n_dashboard_changes_this_question = nrow(dash_in_window),

      # 2) what they typed / got back, and whether it was pasted verbatim
      n_chat_exchanges_this_question = nrow(chat_all),
      typed_messages     = paste(typed_texts, collapse = " || "),
      llm_responses      = paste(llm_texts, collapse = " || "),
      copy_pasted_question = if (length(copy_flags)) any(copy_flags) else NA,

      # 3) timing
      page_arrival_time   = start,
      answered_at          = at,
      page_departure_time = end,
      total_time_on_question_secs = as.numeric(difftime(coalesce(end, at), start, units = "secs")),
      time_to_answer_secs         = as.numeric(difftime(at, start, units = "secs")),
      last_interaction_before_answer_ts = last_interaction_ts,
      thinking_time_secs = as.numeric(difftime(at, last_interaction_ts, units = "secs")),

      # 4) anything else useful
      used_dashboard_this_question = nrow(dash_in_window) > 0,
      used_chat_this_question      = nrow(chat_all) > 0,
      n_plot_images_shown = events_in_window(chat_events, sid, start, end) |>
        filter(event == "plot_image_sent") |> nrow()
    )
  })

# A negative thinking time means the answer was registered before the last
# logged interaction resolved (e.g. the exchange finished streaming a moment
# after the click) — clip at 0 rather than report a nonsensical negative gap.
question_rows <- question_rows |>
  mutate(thinking_time_secs = pmax(thinking_time_secs, 0, na.rm = FALSE))

write_csv(question_rows, file.path(output_dir, "question_interactions.csv"))

# 7. Chat exchange detail (supporting table) ------------------------------------
#
# One row per exchange, tagged with the question window it falls in — lets you
# inspect individual messages instead of the collapsed strings above.

chat_detail <- question_timeline |>
  mutate(row_id = row_number()) |>
  group_split(row_id) |>
  map_dfr(function(r) {
    ex <- events_in_window(chat_events, r$session_id, r$page_start, r$page_end) |>
      filter(event == "exchange")
    if (nrow(ex) == 0) return(NULL)
    ex |>
      transmute(
        session_id, ts, question_id = r$question_id, page = as.character(r$page),
        question_label = r$label, user_msg, assistant,
        copy_pasted_question = map_lgl(user_msg, is_copy_paste, label = r$label)
      )
  })

write_csv(chat_detail, file.path(output_dir, "chat_exchanges_detail.csv"))

# 8. Session-level summary ------------------------------------------------------

session_summary <- responses |>
  transmute(
    session_id, browser, ip_address,
    time_start = parse_ts(time_start), time_end = parse_ts(time_end),
    total_session_secs = as.numeric(difftime(parse_ts(time_end), parse_ts(time_start), units = "secs")),
    q_familiarity, q_confidence = as.numeric(q_confidence), q_reasoning,
    attention_check_survey_content, current_page
  ) |>
  left_join(
    dash_events |> count(session_id, name = "n_dashboard_changes_total"),
    by = "session_id"
  ) |>
  left_join(
    chat_events |> filter(event == "exchange") |> count(session_id, name = "n_chat_exchanges_total"),
    by = "session_id"
  ) |>
  left_join(
    question_rows |> filter(needs_dashboard) |>
      group_by(session_id) |>
      summarise(
        mean_time_to_answer_dashboard_qs = mean(time_to_answer_secs, na.rm = TRUE),
        mean_thinking_time_dashboard_qs  = mean(thinking_time_secs, na.rm = TRUE),
        n_dashboard_questions_used_chat  = sum(used_chat_this_question, na.rm = TRUE),
        n_dashboard_questions_copy_pasted = sum(copy_pasted_question, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "session_id"
  ) |>
  mutate(across(c(n_dashboard_changes_total, n_chat_exchanges_total), \(x) coalesce(x, 0L)))

write_csv(session_summary, file.path(output_dir, "session_summary.csv"))

message(
  "Wrote ", nrow(question_rows), " question-level rows, ",
  nrow(chat_detail), " chat-exchange rows, ",
  nrow(session_summary), " session summary rows to ", normalizePath(output_dir)
)
