# Stimulus Design Report: Dragons vs. Coffee Farms

**Study:** Manual-control dashboard vs. LLM chat dashboard; effects on answer accuracy, time-to-answer, one-hour recall, and detection of a Simpson's paradox.

---

## 1. What the stimulus has to do

The dataset and question set are not neutral scaffolding — several of the study's objectives depend on properties of the stimulus itself:

| Objective | Property the stimulus must have |
|---|---|
| Paradox detection as an automation-complacency probe | A decision question with one defensible answer, reachable only by conditioning on the grouping variable |
| Uncued spontaneous detection | The grouping variable is never named in the focal question, and the pooled default is what both arms see first |
| One-hour recall as the offloading DV | Answers that can only be remembered, not reconstructed from world knowledge |
| Behavioural trace (deviation from pooled default) | Group structure discoverable but not visually obvious |
| Clean accuracy scoring | Non-focal items unambiguous at every level of aggregation |

The dragon stimulus satisfied most of these by accident of being fictional. The move to an economic domain was motivated by framing — the paper should read as a study of business decision-making — so the question is how much of that accidental protection survives.

## 2. Head-to-head comparison

| Dimension | Dragons | Soda (rejected) | Coffee (current) |
|---|---|---|---|
| Decision variable | Wingspan (biological) | Shelf-space share (allocated by the firm) | Altitude (geographic) |
| Reverse causality | None | **Severe** — retailers allocate space to what already sells | None |
| Prior knowledge on X→Y | None | Moderate (shelf elasticity) | Low, but non-zero among coffee-literate people |
| Pooled-misled answer believable? | Yes | **No** — "less shelf space sells more" is absurd, so people escape without detecting | Yes |
| Recall answers reconstructable? | No | **Yes** — supercenters obviously sell most | No |
| Group separation on the focal X | 67–83% overlap | 36–66% | 44–71% |
| Pooled correlation | −0.20 (weak, looks flat) | −0.48 | −0.41 |
| Within-group correlations | +0.71 / +0.62 / +0.56 | Same | Same |
| Business framing | None | Strong | Strong |
| New nuisance covariate | — | Retail familiarity | Coffee familiarity |

The coffee version recovers every property soda lost while keeping the framing gain. Altitude cannot be endogenous; varietal is a credible common cause of both where a plant is grown and its baseline cup quality; and "buy lower land, it sells for more" is a believable-looking conclusion, so the trap still bites.

One improvement over the dragons: the pooled reversal is now −0.41 rather than −0.20. At −0.20 the pooled scatter looked close to flat, which risked participants reading "no relationship" rather than the misleading negative one. The stronger slope was bought with per-varietal offsets that preserve the within-group correlations exactly, at the cost of somewhat more visible clustering (44–71% overlap, still well short of visually obvious).

## 3. Critical analysis of the coffee design

**The focal item is still a single near-binary observation.** Four options, 25% guessing baseline, one item per participant. This is unchanged from the dragon design and it is the largest statistical constraint on the study. The paradox item should be pre-registered as a focal confirmatory item, not treated as a continuous DV with a tidy effect size.

**Priors are reduced, not eliminated, and they point the wrong way.** "High-grown coffee is better" is a real and reasonably widespread belief, and it coincides with the paradox-aware answer. A coffee-literate participant can therefore answer Q1 correctly with zero detection. The familiarity item at the end supports a pre-registered sensitivity analysis excluding self-reported "very familiar" respondents. A more aggressive option is to invert the paradox — negate altitude so the correct answer becomes *lower* — which would make priors push toward the wrong answer and turn a correct response into strong evidence of detection. The cost is a dataset that contradicts real agronomy, which knowledgeable participants may reject outright. Not recommended, but it is the available lever if the familiarity leak proves large in piloting.

**Recall may be noisier than with dragons.** Bourbon, Caturra, and Typica are unfamiliar labels with low semantic distinctiveness; Forest, Mountain, and Sea are vivid and easy to encode. Since one-hour recall is the primary offloading DV, weaker encoding in *both* arms compresses the range and could mask a real interface difference. Recall is tested in multiple-choice form, which mitigates this — recognition is far easier than free recall — but a pilot check on absolute recall rates is worth doing before committing.

**Q2 does not test what it claims to.** "Among farms of the same size, which varietal sits higher?" is meant as a control-for-a-third-variable probe, but Typica is higher both marginally (1542 m vs. 1325 m) and adjusted for farm size (+251 m). A participant who ignores farm size entirely and reads the bar chart gets it right. This weakness is inherited unchanged from the dragon design. Making it a genuine conditioning test would require the marginal and adjusted comparisons to disagree — i.e. a second reversal — which conflicts with the decision to have exactly one trap. As it stands, Q2 is best described in the write-up as an additional accuracy item rather than a reasoning probe.

**There is latent structure nobody is asked about.** Farm size vs. price is −0.54 pooled but roughly zero within varietal, and tree age behaves similarly. A manual participant exploring freely may stumble into varietal structure through a variable unrelated to the focal question. That inflates detection specifically in the manual arm and entangles the detection measure with time spent exploring — which is also a DV. This is a real risk to the causal interpretation of any manual advantage, and it is cheap to remove by regenerating those two columns with consistent signs at both levels of aggregation.

**The dominant confound is not the domain.** In the chat arm, whether the paradox is caught is largely a property of the scripted assistant. Coffee does not change this. The AI's chart output for the focal question must be deterministic and must show the pooled view; a live LLM would make the headline result a property of model behaviour on the day rather than of participant reasoning.

## 4. Bottom line

The coffee stimulus meets the study's objectives at least as well as the dragons on every dimension except recall-item memorability, and it delivers the business framing the paper needs. The remaining weaknesses are ranked:

1. Regenerate farm size and tree age so they carry no latent reversal (removes an arm-asymmetric contamination path).
2. Pilot one-hour recall rates for the varietal names; substitute more distinctive labels if absolute recall is low.
3. Pre-register the familiarity covariate and the sensitivity analysis that drops highly familiar respondents.
4. Describe Q2 as an accuracy item, not a conditioning probe.
5. Script the chat arm's chart for the focal question deterministically.
