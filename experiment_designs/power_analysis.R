# Power analysis for the 3-arm dragon-survey experiment
# Arms: chat_with_llm, dashboard_with_llm, dashboard_without_llm
#
# Re-run after adding the "--- followup" page to survey.qmd (all 3 arms). The
# design is now a repeated-measures one: same participants answer an in-session
# block, then (intended, ~1hr later) a follow-up block.
#
# Outcome inventory as it now stands in survey.qmd:
#   In-session scored MC items (5): q1_breeder, q2_claw_wingspan, q3_flying_speed,
#     q4_wingspan, q5_weight_speed          -> composite score 0-5
#   In-session confidence: q1_confidence (0-10, tied to Q1 only)
#   In-session open text: q1_reasoning (not scored)
#   Follow-up scored MC items (3): fu_q1_claw_wingspan, fu_q2_weight_speed,
#     fu_q3_predictor                        -> composite score 0-3
#   Follow-up confidence: fu_q1_confidence, fu_q2_confidence, fu_q3_confidence (0-10 each)
#   Follow-up process (not scored): fu_q4_process, fu_q5_confidence_shift
#
# IMPORTANT structural note: of the 3 follow-up scored items, only 2 are true
# retests of an in-session item:
#   q2_claw_wingspan  <-> fu_q1_claw_wingspan
#   q5_weight_speed   <-> fu_q2_weight_speed
# fu_q3_predictor is a *transfer* item with no in-session counterpart (by design
# - it's the cleanest diagnostic per summary_plan.md's FU-Q4 logic, since it
# can't have been "given away" by the AI in-session). q1_breeder, q3_flying_speed,
# and q4_wingspan are NOT retested at follow-up.
# This means the matched-pairs retention composite is only 0-2, not 0-3 or 0-5.

library(pwr)

alpha <- 0.05
power_target <- 0.80
k_groups <- 3

## ---- 1. One-way ANOVA (k=3): applies to any continuous composite ----
## (in-session score 0-5, follow-up score 0-3, or either confidence scale --
## Cohen's f is scale-free, so the same n table covers all of them)
anova_f <- c(small = 0.10, medium = 0.25, large = 0.40)
anova_n <- sapply(anova_f, function(f) pwr.anova.test(k = k_groups, f = f, sig.level = alpha, power = power_target)$n)
anova_results <- data.frame(
  effect_size = names(anova_f), cohens_f = anova_f,
  n_per_group = ceiling(anova_n), total_N = ceiling(anova_n) * k_groups
)

## ---- 2. Chi-square test of independence, per scored item (3 arms x correct/incorrect) ----
## Applies identically to all 8 scored items (5 in-session + 3 follow-up)
chisq_w <- c(small = 0.10, medium = 0.30, large = 0.50)
df_chisq <- (k_groups - 1) * (2 - 1)
chisq_n <- sapply(chisq_w, function(w) pwr.chisq.test(w = w, df = df_chisq, sig.level = alpha, power = power_target)$N)
chisq_results <- data.frame(
  effect_size = names(chisq_w), cohens_w = chisq_w,
  total_N = ceiling(chisq_n), n_per_group = ceiling(ceiling(chisq_n) / k_groups)
)

## ---- 3. Group x Time (retention) interaction, via matched-pairs difference score ----
## Only 2 items are truly retested (q2<->FU1, q5<->FU2), giving a 0-2 retention
## composite per participant: retained_score = correct_in_session - correct_followup
## (or, equivalently, proportion retained). Testing whether this *change* differs
## across the 3 arms is a one-way ANOVA on the difference score -- same Cohen's f
## table as section 1 applies directly (f is defined on whatever outcome you feed
## it), so sample-size needs are IDENTICAL in formula terms to Section 1.
## The catch is not the formula, it's the outcome: a 0-2 composite has very
## coarse variance (only 3 possible values), so a given *true* underlying effect
## translates to a smaller *observed* Cohen's f than the same effect would on a
## richer scale. In practice, plan for the "small" row here, not "medium".
retention_note_n_small  <- anova_results$n_per_group[anova_results$effect_size == "small"]
retention_note_n_medium <- anova_results$n_per_group[anova_results$effect_size == "medium"]

## ---- 4. Verifying summary_plan.md's cited "~90/group for a 15pp difference" ----
## Recomputed directly via a two-proportion test (the actual comparison this
## claim describes: correct-rate in one arm vs. another, at a specific timepoint)
baselines <- list(c(0.50, 0.35), c(0.60, 0.45), c(0.70, 0.55))
verify_2tailed <- sapply(baselines, function(b) pwr.2p.test(h = ES.h(b[1], b[2]), sig.level = 0.05, power = 0.80)$n)
verify_1tailed <- sapply(baselines, function(b) pwr.2p.test(h = ES.h(b[1], b[2]), sig.level = 0.05, power = 0.80, alternative = "greater")$n)
verify_results <- data.frame(
  p1 = sapply(baselines, `[`, 1), p2 = sapply(baselines, `[`, 2),
  n_per_group_2tailed = ceiling(verify_2tailed),
  n_per_group_1tailed = ceiling(verify_1tailed)
)
cohens_h_1535 <- ES.h(0.50, 0.35)

## ---- 5. Two-stage attrition ----
## Original: consent -> in-session completion (~20% assumed)
## New: in-session completion -> follow-up completion, 1 hour later, no phones/
## dashboard access in between (assume an ADDITIONAL ~10% non-return, since this
## now requires the participant to still be present/reachable after a gap)
attrition_session  <- 0.20
attrition_followup <- 0.10
combined_retention <- (1 - attrition_session) * (1 - attrition_followup)

cat("=== 1. One-way ANOVA (k=3), applies to in-session score 0-5, follow-up score 0-3, or confidence 0-10 ===\n")
print(anova_results, row.names = FALSE)

cat("\n=== 2. Chi-square, per scored item (3 arms x correct/incorrect), all 8 scored items ===\n")
print(chisq_results, row.names = FALSE)

cat("\n=== 3. Group x Time retention-interaction analysis ===\n")
cat("Only 2 of 5 in-session items are retested at follow-up (q2<->FU1, q5<->FU2).\n")
cat("Retention composite is 0-2. Recommend planning to the SMALL effect row:\n")
cat("  n/group (small, f=.10):", retention_note_n_small, "  [vs. medium f=.25:", retention_note_n_medium, "]\n")

cat("\n=== 4. Verifying summary_plan.md's '~90/group for a 15pp difference' claim ===\n")
cat("Cohen's h for a 15pp gap centered at p=.50 (e.g. .50 vs .35):", round(cohens_h_1535, 3),
    "-- this is a SMALL-to-medium effect (small=.2, medium=.5), not large.\n")
print(verify_results, row.names = FALSE)
cat("--> A genuine 15pp difference needs ~", ceiling(verify_2tailed[1]),
    "/group two-tailed (or ~", ceiling(verify_1tailed[1]), "/group one-tailed) at alpha=.05, power=.80.\n")
cat("--> summary_plan.md's cited ~90/group corresponds to roughly a 20pp gap at baseline .50, not 15pp.\n")
cat("--> Treat ~90/group as optimistic; ~170/group (two-tailed) is the defensible number for a 15pp target.\n")

cat("\n=== 5. Attrition-adjusted enrollment (two-stage: in-session + follow-up return) ===\n")
cat("In-session completion assumed:", (1 - attrition_session) * 100, "%\n")
cat("Follow-up return (additional) assumed:", (1 - attrition_followup) * 100, "%\n")
cat("Combined retention from enrollment to complete follow-up data:", round(combined_retention * 100, 1), "%\n\n")

medium_n_per_group <- ceiling(anova_results$n_per_group[anova_results$effect_size == "medium"])
target_n_enroll_medium <- ceiling(medium_n_per_group / combined_retention)
cat("Analyzable n/group needed for medium-effect ANOVA on composite scores:", medium_n_per_group, "\n")
cat("--> Recruit per arm (buffered for 2-stage attrition):", target_n_enroll_medium, "\n")
cat("--> Recruit total (3 arms):", target_n_enroll_medium * k_groups, "\n\n")

verify_n_per_group <- ceiling(verify_2tailed[1])
target_n_enroll_verify <- ceiling(verify_n_per_group / combined_retention)
cat("If the goal is specifically to detect a genuine 15pp accuracy difference between two arms:\n")
cat("Analyzable n/group needed:", verify_n_per_group, "\n")
cat("--> Recruit per arm (buffered for 2-stage attrition):", target_n_enroll_verify, "\n")
cat("--> Recruit total (3 arms):", target_n_enroll_verify * k_groups, "\n")
