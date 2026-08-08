# Follow-Up Survey (1-Hour Delayed, ~5 Minutes)

Adapted from the 9-question / 15-minute follow-up in `summary_plan.md` (originally
written for a 2-arm dashboard-vs-chat design) down to a **5-minute** instrument for
the current **3-arm** design: `chat_with_llm`, `dashboard_with_llm`,
`dashboard_without_llm`. Cuts made to fit the time budget are noted inline.

## Administration

- Paper-based or a plain (non-interactive) form — **no dashboard, no chat, no AI access**.
- Given ~1 hour after the in-session survey ends, after an unrelated filler activity.
- Same for all three arms — this is the instrument that lets you compare retention
  *across* arms once the tool (dashboard and/or LLM) is taken away.
- Record `session_id` on the form so responses can be joined back to arm assignment
  and in-session accuracy/confidence/timing data.
- Target: **≤ 5 minutes total**. Estimated time per item is noted; stop adding
  items if piloting pushes the total over ~5:30.

## What was cut from the 15-minute version, and why

- **Section A (easy factual recall, FU-Q1/FU-Q2)** — dropped. Summary_plan itself
  flags these as "both groups expected to perform similarly," i.e. low diagnostic
  value; not worth the time budget here.
- **FU-Q6 (transfer to a new deep-sea dataset)** — dropped. Requires presenting a
  new stimulus table, which alone can eat 1–2 minutes; too expensive for a 5-minute
  form.
- **FU-Q7/FU-Q8 (open-ended process questions)** — collapsed into a single
  closed-form multiple-choice item (FU-Q4 below) so it can be answered in seconds
  instead of written out.
- **1–5 confidence scale** — changed to **0–10** to match the scale already used
  in-session (`q1_confidence`), so within-participant confidence-over-time
  comparisons don't require rescaling.

## Items

### FU-Q1 — Retention: claw length / wingspan (hardest in-session item)
*(retention check for `q2_claw_wingspan`)*

> At the same claw length, which dragon type tends to have a larger wingspan —
> Mountain Dragons or Sea Dragons?

- Mountain Dragon
- Sea Dragon
- Both have the same wingspan
- Cannot determine from the data

**Confidence:** How confident are you in that answer? (0 = not at all, 10 = completely) — slider 0–10

*Est. time: 30 sec*

---

### FU-Q2 — Retention: weight / flying speed relationship
*(retention check for `q5_weight_speed`)*

> How are weight and flying speed related, across all dragon types?

- Positively related
- Negatively related
- No relation
- Cannot determine from the data

**Confidence:** 0–10 slider

*Est. time: 30 sec*

---

### FU-Q3 — Transfer diagnostic (not asked in-session)
*(cleanest diagnostic per summary_plan's FU-Q4 logic: this variable relationship
was never surfaced by any in-session question, in either the dashboard tool or the
chat/AI output, so a correct answer here can't be attributed to having simply been
told the answer)*

> Of the dragon traits you saw — wingspan, weight, claw length, or claw thickness —
> which one is the *strongest single predictor* of flying speed?

- Wingspan
- Weight
- Claw length
- Claw thickness

**Confidence:** 0–10 slider

*Est. time: 45 sec*

---

### FU-Q4 — Meta-cognitive (closed-form, replaces open-ended FU-Q7/FU-Q8)

> During the session, how did you mostly arrive at your answers? Pick the single
> best description.

- I mostly explored the dashboard/data myself and reasoned it out
- I mostly relied on the AI's explanations or visualizations
- I used a roughly even mix of both
- I mostly guessed
- *(dashboard_without_llm respondents will not see the AI-related options — see note below)*

*Est. time: 20 sec*

> **Note:** for `dashboard_without_llm` participants (no AI was available), drop
> the "relied on the AI" and "even mix" options — offer only "explored the
> dashboard myself" vs. "mostly guessed."

---

### FU-Q5 — Confidence shift (retrospective)

> Compared to how confident you felt *during* the session, how confident do you
> feel right now answering similar questions *without* the dashboard or AI?

- More confident now
- About the same
- Less confident now

*Est. time: 20 sec*

---

**Total estimated time: ~2:45–3:00**, leaving buffer under the 5-minute cap for
instructions and transition between items.

## Answer Key (for scoring — not shown to participants)

Derived from `data/dragons.csv` (Forest n=151, Mountain n=68, Sea n=123):

| Item | Correct answer | Basis |
|---|---|---|
| FU-Q1 | **Sea Dragon** | Controlling for claw length, dragon_type=Sea has a wingspan +0.49 m higher than Mountain (linear model, p < .001) |
| FU-Q2 | **Negatively related** | corr(weight_kg, flying_speed_kmh) = -0.58 |
| FU-Q3 | **Claw length** | \|corr\| with flying_speed_kmh: claw_length_cm = 0.69 (highest) vs. wingspan_m = 0.59, weight_kg = 0.58, claw_thickness_cm = 0.50 |

FU-Q4 and FU-Q5 are process/self-report items, not scored for accuracy — analyze
as categorical distributions by arm (and, for FU-Q4, cross-tab against actual
logged chat/explain-plot usage where available, to check self-report accuracy).

## Analysis notes

- **Primary contrast**: FU-Q1 and FU-Q3 accuracy, compared across the 3 arms —
  FU-Q3 in particular isolates retention/understanding from "the AI told me the
  answer," since it was never asked in-session.
- **Calibration**: pair each FU-Q1–Q3 confidence rating with correctness; a
  `dashboard_with_llm`/`chat_with_llm` group that was confident in-session but
  shows a confidence-accuracy mismatch here (high confidence, low accuracy) is
  the calibration-degradation signal described in `summary_plan.md`.
- **FU-Q5** gives a cheap direct self-report of the same calibration-shift idea
  as a sanity check against the FU-Q1–Q3 confidence deltas.
- Session-level (not individual-level) randomization still applies — use
  cluster-robust SEs at the session level when comparing arms, consistent with
  `summary_plan.md`'s analysis plan.
