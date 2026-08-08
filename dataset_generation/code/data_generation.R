source(here::here('dataset_generation','code', 'setup.R'))
library(GGally)


set.seed(42)

penguins <- read_csv(here('dataset_generation', "data", "penguins.csv"), show_col_types = FALSE)

# --- Mapping tables ----------------------------------------------------------

species_map <- c(
  Adelie    = "Forest Dragon",
  Chinstrap = "Mountain Dragon",
  Gentoo    = "Sea Dragon"
)

region_map <- c(
  Torgersen = "Northern Peaks",
  Biscoe    = "Coastal Cliffs",
  Dream     = "Ancient Forest"
)

# --- Patterns baked into the transformation ----------------------------------
#
# Forest Dragon   (Adelie):    compact and light, smallest wingspan, fastest
# Mountain Dragon (Chinstrap): long sharp claws (high length:thickness ratio),
#                              light, mid wingspan, moderate speed
# Sea Dragon      (Gentoo):    largest wingspan, much heaviest, slowest
#
# Within each type, males are larger than females.
#
# Flying speed is built as: a large per-species baseline, plus a strong
# WITHIN-species wingspan effect, minus a per-species weight drag. The two
# question-relevant patterns this produces are both reversals:
#
#   wingspan vs speed -- strongly POSITIVE within each dragon type, but
#     NEGATIVE across all dragons pooled, because the broad-winged Sea
#     Dragons are the slow ones. (Simpson's paradox.)
#
#   weight vs speed   -- strongly NEGATIVE across all dragons pooled, but
#     essentially FLAT within each dragon type. The per-species drag
#     coefficients are calibrated against each species' own wingspan-weight
#     slope, so within a type a heavier dragon's extra wing area cancels its
#     extra drag; the pooled negative slope is entirely a species effect.

n <- nrow(penguins |> filter(!is.na(bill_length_mm)))

# Baseline speed by type: fast, light, small-winged Forest Dragons at the top.
species_speed_offset <- c(
  "Forest Dragon"   =  60,   # distinctly fastest
  "Mountain Dragon" =  43,
  "Sea Dragon"      =   28
)

# Weight drag per type, tuned to each type's own wingspan-on-weight slope so
# that weight has no within-species association with speed.
species_weight_drag <- c(
  "Forest Dragon"   = 0.20,
  "Mountain Dragon" = 0.36,
  "Sea Dragon"      = 0.31
)

dragons <- penguins |>
  filter(!is.na(bill_length_mm)) |>
  mutate(
    dragon_type = species_map[species],
    region      = region_map[island],

    # Claws — non-round multipliers break the obvious linear link to bill dims.
    # Mountain Dragons have proportionally longer, thinner claws.
    claw_length_cm    = round(bill_length_mm * 0.547 + rnorm(n(), 0, 0.6), 1),
    claw_thickness_cm = round(bill_depth_mm  * 0.313 + rnorm(n(), 0, 0.3), 1),

    # Wingspan — scaled from flipper; Sea Dragons are widest.
    wingspan_m = round(flipper_length_mm * 0.0218 + rnorm(n(), 0, 0.08), 2),

    # Weight — scaled from body mass with added scatter.
    weight_kg = round(body_mass_g * 0.0637 + rnorm(n(), 0, 2.5), 1),

    sex = sex
  ) |>
  # Centre the two speed drivers within dragon type, so their coefficients act
  # purely within a species and the between-species pattern is set by the
  # baseline offsets alone.
  mutate(
    wingspan_c = wingspan_m - mean(wingspan_m),
    weight_c   = weight_kg  - mean(weight_kg),
    .by = dragon_type
  ) |>
  mutate(
    flying_speed_kmh = round(
      90 +
        species_speed_offset[dragon_type] +
        wingspan_c * 90 -
        weight_c   * species_weight_drag[dragon_type] +
        rnorm(n(), 0, 5),
      1
    )
  ) |>
  select(
    dragon_type, region, sex,
    claw_length_cm, claw_thickness_cm,
    wingspan_m, weight_kg, flying_speed_kmh
  ) |>
  # Shuffle row order so the species-block structure of penguins isn't visible.
  slice_sample(prop = 1)

write_csv(dragons, here("data", "dragons.csv"))

# --- Quick sanity check ------------------------------------------------------

dragons |>
  summarise(
    across(
      c(claw_length_cm, claw_thickness_cm, wingspan_m, weight_kg, flying_speed_kmh),
      list(mean = mean, sd = sd),
      .names = "{.col}__{.fn}"
    ),
    .by = dragon_type
  ) |>
  print(width = Inf)

# The two reversals the survey questions turn on. Expected:
#   pooled wingspan ~ speed  negative,  within-species  strongly positive
#   pooled weight   ~ speed  strongly negative,  within-species  ~zero
cat("\nPooled across all dragons:\n")
cat(sprintf("  cor(wingspan, speed) = %+.3f\n",
            cor(dragons$wingspan_m, dragons$flying_speed_kmh)))
cat(sprintf("  cor(weight,   speed) = %+.3f\n",
            cor(dragons$weight_kg, dragons$flying_speed_kmh)))

cat("\nWithin each dragon type:\n")
dragons |>
  summarise(
    cor_wingspan_speed = cor(wingspan_m, flying_speed_kmh),
    cor_weight_speed   = cor(weight_kg,  flying_speed_kmh),
    .by = dragon_type
  ) |>
  print()


ggpairs(dragons)


dragons |>
ggplot(aes(x = dragon_type, y = flying_speed_kmh)) +
  geom_bar(stat= 'identity')

