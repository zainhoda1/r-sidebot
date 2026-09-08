-- Logging tables for the llm_explain_control app.
--
-- sb_create_tables() in R/supabase_log.R runs these automatically at startup, so
-- you normally don't need this file. Paste it into the Supabase SQL editor if
-- the database role the app connects as lacks DDL rights.
--
-- `design` distinguishes rows when several experiment designs share one Supabase
-- project. `session_id` is Shiny's session$token, which is also what surveydown
-- records, so these join to the survey responses table.

create table if not exists chat_events (
  id         bigserial primary key,
  design     text,
  session_id text not null,
  ts         timestamptz not null default now(),
  event      text not null,   -- greeting|query|exchange|plot_rendered|plot_error|app_message
  context    text,            -- main_chat | explain_plot
  user_msg   text,
  assistant  text,
  app_msg    text,            -- shown to the participant, never sent to the model
  code       text,
  error      text
);

create index if not exists chat_events_session_idx
  on chat_events (session_id, ts);

-- Rewritten in full after every turn, so it always matches what the model saw.
-- conversation_id is the session token for the sidebar chat and
-- <token>_explain_plot_<n> for a modal clone; session_id is the token either way.
create table if not exists conversation_turns (
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
);

create index if not exists conversation_turns_convo_idx
  on conversation_turns (conversation_id, turn_n);
