---
format: pdf
---


# Experiment Design Plan: LLM Assistance Mode and Data Comprehension

## 1. Overview

### Research Title
*Does AI assistance mode affect data comprehension, calibration, and retention? A study of manual dashboard exploration vs. chat-driven visualization.*

### Core Research Questions
1. Does the mode of AI assistance affect accuracy on data comprehension tasks during a session?
2. Does it affect *calibration* — the alignment between a user's confidence and their actual accuracy?
3. Does manual exploration of data produce better retention one hour later compared to AI-mediated visualization?
4. Does the mode of interaction affect transfer — the ability to reason about data without AI assistance?

### Primary Hypothesis
Group B (chat-only) will perform comparably or better on in-session survey questions because the AI produces accurate interpretations. However, Group A (dashboard + AI assistant) will outperform Group B at the 1-hour follow-up, particularly on questions requiring multi-variable reasoning — because active manipulation builds durable mental models, while AI-generated answers are not reliably encoded as the user's own understanding.

---

## 2. Dataset

The experiment uses a synthetic **dragon species dataset** with the following structure:

| Variable | Type | Range / Values |
|---|---|---|
| `dragon_type` | Categorical | Forest Dragon, Mountain Dragon, Sea Dragon |
| `region` | Categorical | Coastal Cliffs, Ancient Forest, Northern Peaks |
| `sex` | Categorical | Male, Female |
| `claw_length_cm` | Numeric | 17.0 – 31.9 cm |
| `claw_thickness_cm` | Numeric | 3.6 – 7.5 cm |
| `wingspan_m` | Numeric | 3.77 – 5.19 m |
| `weight_kg` | Numeric | 171.4 – 405.1 kg |
| `flying_speed_kmh` | Numeric | 123.7 – 173.3 km/h |

Using a fictional dataset removes prior domain knowledge as a confound — all participants start from the same baseline.

---

## 3. Treatment Groups

### Group A — Dashboard Interface (`joint_survey - dashboard interface/`)
Participants interact with a structured Shiny dashboard that shows a live visualization alongside an embedded survey. Controls available:
- **Scatter tab**: X-axis and Y-axis dropdowns (5 numeric variables each)
- **Bar tab**: Variable dropdown showing mean per dragon type
- **Species filter**: Checkbox group to include/exclude Forest, Mountain, and Sea Dragons
- **AI chat assistant** in the sidebar: answers data questions in natural language but does not control the visualization
- **"Explain plot" button**: sends the current plot to Claude for a brief interpretation in a modal

The user *decides what to look at*. The AI is advisory.

### Group B — Chat Interface (`joint_survey - chat interface/`)
Participants interact with an AI chat assistant that generates visualizations on demand. Controls available:
- **Chat input**: natural language requests (e.g., "show me how weight relates to flying speed")
- The AI writes and executes ggplot2 code; the resulting plot appears in the main panel
- **"Explain plot" button**: same as Group A
- No manual axis controls or species dials — the visualization is entirely AI-driven

The AI *decides what to show* based on what the user thinks to ask.

### Shared Elements
- Same dragon dataset
- Same 5 in-session survey questions (embedded right panel, via `surveydown`)
- All chat exchanges logged (content + timestamps)
- Session token identifies each participant

---

## 4. Procedure

### Pre-Session (5 minutes)
Participants complete a brief background questionnaire:
- Prior experience with data visualization tools (1–5 scale)
- Prior experience using AI chatbots for analytical tasks (1–5 scale)
- Self-rated comfort with interpreting charts and graphs (1–5 scale)

These serve as covariates in the analysis and allow checking that randomization balanced groups.

### Session (20–25 minutes)
1. Participants are randomly assigned to Group A or Group B **at the session level** — all participants in a given session receive the same condition. Different sessions (e.g., different class meetings or rooms) receive different conditions. This prevents within-session contamination and spillover between conditions.
2. A cover story is used to reduce demand characteristics: participants are told the study examines *"how different interface designs affect data exploration speed and efficiency."* This is truthful but does not prime participants to think about AI dependency or their own cognitive autonomy.
3. Brief on-screen instructions explain the interface. No guidance is given on *how* to explore.
4. Participants explore the dashboard / chat interface freely and then answer the 5 in-session survey questions embedded in the right panel. A **per-question confidence rating (1–5)** is collected immediately after each question — not as a single global rating at the end — to enable question-level calibration analysis.
5. After submitting, the session closes.

### 1-Hour Gap
Participants remain in the classroom. No phones, no revisiting the dashboard. The gap is structured (e.g., a lecture or reading) to prevent deliberate rehearsal.

### Follow-Up (15 minutes)
Paper-based or separate browser session — **no AI, no dashboard access**. Participants answer the follow-up survey described in Section 6.

---

## 5. In-Session Survey Questions

These are already implemented in `survey.qmd` and administered via `surveydown` during the session.

| ID | Question | What It Tests |
|---|---|---|
| Q1 | Which dragon type has the highest average flying speed? | Direct single-variable lookup |
| Q2 | Across all types combined, how are weight and flying speed related? | Cross-variable relationship, all species |
| Q3 | Which dragon type has the largest average wingspan? | Direct single-variable lookup |
| Q4 | When filtering to a single dragon type, how does wingspan relate to flying speed? | Relational reasoning within a filtered subset — requires species-level exploration |
| Q5 | At the same claw length, which type tends to have a larger wingspan — Mountain or Sea? | Controlling for a variable — the hardest question; requires either a filtered scatter or a specific chat request |

**Difficulty gradient**: Q1 and Q3 are straightforward lookups. Q2 requires combining two variables. Q4 requires filtering first. Q5 requires reasoning about a conditional relationship — the kind that only surfaces if the user specifically looks for it.

---

## 6. Follow-Up Survey Questions

Administered 1 hour after the session, with no access to the dashboard or AI. Each question includes a **confidence rating (1–5)** immediately after the answer.

---

### Section A — Factual Recall
*Tests whether basic facts from the session were encoded in memory. Both groups are expected to perform similarly here; large differences signal attention or engagement differences, not reasoning differences.*

**FU-Q1.** Without looking at any data, which dragon type tends to be the heaviest on average?
- Forest Dragon
- Mountain Dragon
- Sea Dragon
- All three are similar in weight

*Confidence: 1 (guessing) – 5 (certain)*

---

**FU-Q2.** Approximately what range best describes the flying speeds observed across all dragon types in the dataset?
- Under 100 km/h
- 100–140 km/h
- 130–180 km/h
- Over 200 km/h

*Confidence: 1 – 5*

---

### Section B — Multi-Variable Reasoning
*Tests retention of relationships between variables. Group A users who actively changed the scatter axes are more likely to have encoded these. Group B users only saw what they thought to ask the AI about.*

**FU-Q3.** Within a single dragon species (e.g., looking only at Forest Dragons), does a dragon with a larger wingspan tend to fly faster or slower?
- Faster
- Slower
- No consistent relationship
- I cannot recall

*Confidence: 1 – 5*

> **Design note**: This is a direct retention test of Q4 from the in-session survey. If Group B answers correctly but with low confidence, they may have had the AI answer it for them without encoding why. If Group A answers correctly with high confidence, active manipulation aided encoding.

---

**FU-Q4.** If you had to predict a dragon's flying speed using only one other measurement, which would you trust most?
- Claw length
- Wingspan
- Weight
- Claw thickness

*Confidence: 1 – 5*

> **Design note**: This question was *not* asked in the primary survey and the AI in Group B is unlikely to have been asked about it directly. Group A users who explored multiple axis combinations on the scatter plot are better positioned to answer. A performance gap here is the clearest evidence that manual exploration produces richer mental models than AI-mediated visualization.

---

**FU-Q5.** Among dragons of the same claw length, which species tends to have a larger wingspan — Mountain Dragons or Sea Dragons?
- Mountain Dragon
- Sea Dragon
- They are about the same
- I cannot recall

*Confidence: 1 – 5*

> **Design note**: Direct retention of Q5 — the hardest in-session question. Group B users who asked the AI this question explicitly may recall the answer but not the reasoning behind it. Group A users who set claw_length as X and wingspan as Y on the scatter, then compared species, are more likely to have a durable encoding.

---

### Section C — Transfer
*Tests whether participants can reason about data without AI or a pre-built visualization. Presents a new fictional dataset as a static summary table — no dashboard, no AI. Tests generalizable reasoning skill vs. recall of AI output.*

**Instructions for Section C**: The following table summarizes a dataset of three fictional deep-sea creatures. Use it to answer the questions below. You have not seen this data before.

| Species | Avg Fin Span (m) | Avg Body Mass (kg) | Avg Dive Speed (km/h) | Avg Jaw Strength (kg-force) |
|---|---|---|---|---|
| Abyssal Eel | 0.4 | 28 | 95 | 12 |
| Midnight Ray | 2.1 | 180 | 38 | 45 |
| Void Shark | 1.6 | 310 | 62 | 210 |

---

**FU-Q6.** Based on the table, how would you describe the relationship between body mass and dive speed across these three species?
- Heavier species tend to dive faster
- Heavier species tend to dive slower
- No clear relationship
- Cannot determine from this table

*Confidence: 1 – 5*

> **Design note**: This mirrors Q2 from the in-session survey (cross-variable relationship, all species). The correct answer is "heavier species tend to dive slower" — the same pattern direction as the dragon data's weight-speed relationship. If Group A transfers this reasoning better, it suggests their manual exploration built a generalizable schema, not just a memorized fact.

---

**FU-Q7.** You want to know whether fin span predicts dive speed within the Void Shark population (individual-level data, not shown here). What would you look for in a scatter plot to answer that question?

*(Open-ended, 2–3 sentences)*

> **Design note**: This is a process question, not an answer question. It tests whether participants understand *how* to reason about a within-species variable relationship — the skill that Q4 from the in-session survey required. Group A users who physically set up that kind of scatter plot can describe it concretely. Group B users who watched the AI produce it may describe what they saw without understanding the reasoning process.

---

### Section D — Meta-Cognitive
*Captures self-awareness of AI dependency and reasoning process.*

**FU-Q8.** *(Open-ended)* How did you approach answering the survey questions during the session? Describe your process in 2–3 sentences.

> **Coding scheme**: Responses will be coded for (a) references to AI output ("the AI showed me", "I asked it to"), (b) references to self-directed exploration ("I changed the axes", "I filtered to just Forest Dragons"), and (c) reasoning justifications ("because the scatter showed a downward slope"). Group B responses are expected to skew toward (a); Group A toward (b) and (c).

---

**FU-Q9.** How confident are you in your answers today compared to your answers during the session?
- Much more confident today
- Somewhat more confident today
- About the same
- Somewhat less confident today
- Much less confident today

> **Design note**: If Group B reports being more confident during the session (when the AI was present) but less confident now, this directly measures the confidence loss that accompanies AI removal — a behavioral signature of autonomy degradation.

---

## 7. Data Collection Summary

| Source | Group A | Group B | What It Measures |
|---|---|---|---|
| `surveydown` database | Yes | Yes | In-session answer accuracy + per-question confidence ratings |
| Mouse tracking (`mouse_tracking.csv`) | Yes | Yes | Time on each survey page, dwell patterns |
| Chat log (`log_exchange.R`) | Yes (voluntary AI consultation) | Yes (visualization requests + follow-up questions) | AI reliance — **analyzed separately per group; raw volumes are not comparable across conditions** |
| Dial/tab change events | Yes (log each `scatter_x`/`scatter_y`/`dragon_species` change with timestamp) | No | Actual exploration depth; manipulation check for Group A |
| Group B prompt quality ratings | No | Yes (rated 1–3 by two researchers: off-topic / imprecise / precise) | Controls within-group variance from poor prompting |
| Follow-up survey | Yes | Yes | Retention, transfer, calibration, metacognition |
| Background questionnaire | Yes | Yes | Covariate control |

---

## 8. Analysis Plan

### Power Analysis
A minimum detectable effect of 15 percentage points difference in follow-up Section B accuracy (e.g., 65% vs. 50% correct) is assumed based on similar human-AI interaction studies. At α = 0.05 (two-tailed) and 80% power, this requires approximately **90 participants per group** (180 total) for an independent proportions test. If recruitment is limited to one classroom (~40 participants), only effects ≥ 25 percentage points will be detectable — this must be acknowledged as a limitation in any report. Power should be re-estimated once effect sizes from a pilot run are available.

### Primary Outcomes
- **In-session accuracy**: % correct on Q1–Q5, compared between groups
- **Follow-up accuracy**: % correct on FU-Q1 through FU-Q6, compared between groups
- **Calibration index**: per-question confidence minus accuracy, computed at both time points per participant. Requires question-level confidence ratings (collected in-session per the updated procedure). A well-calibrated participant has a positive index; overconfident AI-dependent participants show near-zero or negative indices at follow-up.

### Key Tests
All four tests below are pre-specified as the confirmatory analysis. A Bonferroni-corrected α = 0.0125 is applied across them. All models use **cluster-robust standard errors** clustered at the session level to account for non-independence of participants within the same session.

1. **Group × Time interaction** on accuracy (in-session vs. follow-up): prediction is a crossover or divergence, with Group B holding up during the session but declining more steeply at follow-up.
2. **Question-type effect**: compare Section A vs. Section B vs. Section C follow-up scores separately — the accuracy gradient should steepen for Group B as question complexity increases.
3. **Calibration × Group interaction**: if Group B overconfidence at follow-up is confirmed, this is the autonomy degradation signal.
4. **FU-Q4 as a diagnostic**: this question tests a relationship not covered in the primary survey and unlikely to have been surfaced in Group B's chat session. A Group A advantage here, isolated from overall accuracy differences, is the cleanest evidence that exploration depth matters beyond task performance. *Note: verify the correct answer by running `cor(dragons[, numeric_cols])` before data collection to confirm one variable substantially outpredicts the others.*

### Subgroup Analysis
Group A participants who changed the scatter axes fewer than 3 times (low explorers, identified from dial-change logs) are analyzed separately. The active-exploration hypothesis applies only to high explorers; including passive Group A users in the main analysis would attenuate the effect.

### Qualitative
- FU-Q7 and FU-Q8 responses coded by two independent raters using the scheme above.
- Inter-rater reliability checked with Cohen's kappa before any group comparisons.
- Group differences in coding categories tested with chi-square or Fisher's exact test.

### Covariates
Prior visualization experience, prior AI chatbot experience, and self-rated statistical comfort are included as covariates in all accuracy models. Group B prompt quality ratings are included as an additional covariate in all Group B analyses.

---

## 9. Ethical Considerations

- **IRB approval** must be obtained before any data collection begins. The study involves behavioral data recording and deception (cover story), both of which require institutional review.
- **Informed consent** is obtained from all participants before the pre-session questionnaire. The consent form discloses that interaction data and responses are recorded, that the study involves an interface comparison, and that participants may withdraw at any time without academic penalty.
- Participants are told the study examines interface design and exploration efficiency (the cover story). The cover story is truthful but omits the AI-dependency framing to reduce demand characteristics.
- **Debrief** after the follow-up survey fully explains the study purpose, the two conditions, and the AI-dependency hypothesis. Participants are given the opportunity to withdraw their data after debriefing.
- No personally identifiable information is collected; sessions are identified by a randomly generated token. Chat logs, mouse tracking data, and survey responses are stored on a university-controlled server, retained for 5 years, and then deleted.

---

## 10. Methodological Limitations and Recommended Fixes

This section documents 15 issues identified through validation against existing research, organized by severity. Issues marked **[Addressed]** have been incorporated into the design above.

---

### Critical — Threatens Internal Validity

**L1. SUTVA violation from same-room randomization** **[Addressed]**
Assigning individuals within one classroom to different conditions violates the Stable Unit Treatment Value Assumption. Students can see each other's screens, discuss findings during the 1-hour gap, and inadvertently share treatment-condition information. Contamination literature (Raudenbush, 2011; J. Educational and Behavioral Statistics) shows this biases estimates toward null and makes condition assignment effectively unverifiable.
*Fix applied*: Randomization moved to the session level. Each session is a single condition. Separate rooms or separate class meetings are required.

---

**L2. Testing effect confound on follow-up questions** **[Partially Addressed]**
FU-Q3 and FU-Q5 are direct re-tests of in-session Q4 and Q5. Research by Roediger & Karpicke shows that answering a question during learning constitutes a retrieval practice event that independently strengthens memory. Both groups receive this benefit, masking condition differences. The follow-up scores for these items reflect retrieval practice effects, not purely the encoding benefit of active exploration.
*Fix applied*: FU-Q4 (the best predictor question) is explicitly flagged as the cleanest diagnostic because it was not asked in-session. Remaining limitation: FU-Q3 and FU-Q5 should be interpreted cautiously and are better treated as retention checks than as tests of the active-exploration hypothesis.

---

**L3. No power analysis or sample size justification** **[Addressed]**
No sample size was specified in the original design, making any null result uninterpretable. For a 15-percentage-point difference in follow-up accuracy at α = 0.05, 80% power requires ~90 participants per group.
*Fix applied*: Power analysis added to Section 8. Small-classroom limitation explicitly acknowledged.

---

### Moderate — Weakens Core Claims

**L4. Generation effect mechanism overstated for dropdown interaction**
The design implies the "generation effect" (d ≈ 0.40 for self-generated vs. read information; Slamecka & Graf, 1978) justifies Group A's expected retention advantage. However, the generation effect is strongest when participants produce answers from scratch, not when they select from a pre-built menu. A 2024 meta-analysis (Qin & Karimi, Quarterly Journal of Experimental Psychology) found that "under desktop conditions with minimal motor demands, active exploration does not confer a substantial advantage over passive observation." Selecting from a dropdown is near that threshold.
*Fix*: The retention mechanism for Group A is better described as **self-directed attentional allocation** — users choose which variable relationships to inspect, creating selective encoding tied to personal curiosity — rather than the generation effect proper. This is still theoretically grounded in Craik & Lockhart's levels-of-processing framework but more accurate about the specific mechanism. Reference these distinctions in the analysis section.

---

**L5. Transfer task conflates visualization format change with domain change** **[Partially Addressed]**
Section C presents group-level summary statistics (a 3-row mean table) as the transfer dataset, while the in-session task used individual-level scatter plots. This simultaneously changes the data domain and the visualization format. A Group A advantage on FU-Q6 could reflect better scatter-plot reading skill, not better cross-domain reasoning. Additionally, using 3 species in both datasets makes this near-transfer, not far transfer — the structural similarity is too high to claim it tests generalizable reasoning. Near and far transfer require different theoretical framing and different result interpretations (Barnett & Ceci, 2002).
*Recommended fix*: Present the transfer dataset as a static scatter plot image to match the in-session format. Explicitly label the transfer section as near-transfer in the discussion.

---

**L6. No manipulation check on Group A exploration depth** **[Addressed]**
The active-exploration hypothesis assumes Group A users actually changed the axis controls. There is no guarantee they did. An unengaged Group A participant is functionally as passive as a Group B participant who received AI-generated plots.
*Fix applied*: Dial-change events are now logged with timestamps (Section 7). Subgroup analysis on high vs. low explorers added to Section 8.

---

**L7. Calibration measured too coarsely in-session** **[Addressed]**
A single global confidence rating after all 5 questions cannot support the planned calibration index computation, which requires question-level confidence to compare with question-level accuracy.
*Fix applied*: Per-question confidence ratings (1–5) collected immediately after each in-session question (Section 4).

---

**L8. Chat volume not comparable across conditions** **[Addressed]**
In Group B, every visualization requires a chat message — chat use is mandatory. In Group A, chat is optional. Raw chat volume is not a valid cross-condition measure of AI reliance.
*Fix applied*: Chat logs analyzed separately per group with condition-appropriate interpretations (Section 7).

---

### Minor — Addressable without Structural Changes

**L9. 1-hour retention interval may be too short to differentiate groups**
Retention interval research (Roediger et al., 2009, Experimental Psychology) shows the testing effect and active-learning advantages grow larger at longer delays (24 hours, 1 week). At 1 hour, the forgetting curve has not fully differentiated durable from fragile encodings. With simple material (3 species, 5 variables), ceiling effects in retention are likely.
*Recommended fix*: Add harder numerical recall questions to the follow-up to reduce ceiling effects (e.g., "What was the approximate mean flying speed of Forest Dragons?"). If a second follow-up at the next class meeting is feasible, add it as an optional third measurement wave.

---

**L10. FU-Q4's correct answer is unverified**
The question "which single variable best predicts flying speed?" assumes one predictor clearly dominates. If correlations in the actual dataset are weak or clustered, there may be no objectively correct answer, making the item unscoreable as an accuracy measure.
*Required action before data collection*: Run `cor(dragons[, sapply(dragons, is.numeric)])` and confirm that one variable has a substantially higher correlation with `flying_speed_kmh` than others (Δr ≥ 0.15 suggested). If no clear winner emerges, add "all variables predict equally well" as a response option and recode as a reasoning quality item.

---

**L11. No pre-registration**
The design was developed after the experimental setup already existed. Pre-registration on OSF before data collection is necessary to protect the confirmatory analyses from p-hacking and HARKing. This design plan, with the additions in Section 8 (pre-specified tests, Bonferroni correction, primary outcome), is nearly sufficient as a preregistration document.
*Required action*: Submit to OSF before the first session runs. Record the planned primary outcome (follow-up Section B accuracy), the four confirmatory tests, the α correction, and the subgroup rule for Group A explorers.

---

**L12. AI output quality variance within Group B is uncontrolled**
Participants who phrase prompts poorly receive irrelevant or misleading visualizations. This creates within-group variance attributable to prompt skill rather than comprehension, reducing statistical power and complicating interpretation.
*Fix applied*: Group B prompt quality rated 1–3 by two researchers, included as a covariate in all Group B models (Section 7 and Section 8).

---

**L13. Demand characteristics from partial disclosure**
Partial disclosure ("we record your interactions") may not prevent demand effects if participants infer their condition. A Group B participant who realizes the AI controls all visualizations may strategically memorize more aggressively, attenuating the expected retention gap.
*Fix applied*: Cover story added (Section 4).

---

**L14. Incomplete ethics documentation**
The original plan omitted IRB approval, informed consent, and withdrawal procedures — all required for a study involving behavioral recording and deception.
*Fix applied*: Full ethics section added (Section 9).

---

**L15. Cluster non-independence ignored in analysis**
Participants within the same session share environmental conditions (room, time of day, instructor presence). Standard regression errors will be underestimated without accounting for within-session correlation.
*Fix applied*: Cluster-robust standard errors clustered at the session level specified for all primary analyses (Section 8).

---

### Literature Referenced in This Section

- Barnett, S. M., & Ceci, S. J. (2002). When and where do we apply what we learn? *Psychological Bulletin*, 128(4), 612–637.
- Craik, F. I. M., & Lockhart, R. S. (1972). Levels of processing. *Journal of Verbal Learning and Verbal Behavior*, 11(6), 671–684.
- Qin, Y., & Karimi, H. A. (2024). Active and passive exploration for spatial knowledge acquisition: A meta-analysis. *Quarterly Journal of Experimental Psychology*.
- Raudenbush, S. W. (2011). The implications of contamination for experimental design in education. *Journal of Educational and Behavioral Statistics*, 36(1).
- Roediger, H. L., & Karpicke, J. D. (2006). Test-enhanced learning. *Psychological Science*, 17(3), 249–255.
- Roediger, H. L., Agarwal, P. K., McDaniel, M. A., & McDermott, K. B. (2009). Test-enhanced learning in a middle school science classroom. *Journal of Educational Psychology*, 103(2), 399–414.
- Slamecka, N. J., & Graf, P. (1978). The generation effect. *Journal of Experimental Psychology: Human Learning and Memory*, 4(6), 592–604.
