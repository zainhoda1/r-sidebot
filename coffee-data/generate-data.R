source(here::here('dragon_dataset_generation', 'code', 'setup.R'))
library(GGally)

set.seed(42)

# Change this one line if the coffee data lives in its own project folder.
base_dir <- 'dragon_dataset_generation'

# --- What's different here vs coffee_data_generation.R ----------------------
#
# The original script derives coffee attributes from the penguins dataset
# (3 species -> 3 varietals). This version is fully synthetic -- no source
# dataset -- and produces FIVE varietals instead of three, ordered from
# lowest-grown/cheapest to highest-grown/dearest:
#
#   Bourbon < Caturra < Typica < Catuai < Gesha
#
# The same single-reversal Simpson's paradox is reproduced: pooled across all
# farms, price appears to RISE with altitude, but within every one of the
# five varietals it FALLS with altitude. That requires the same two
# ingredients as the original:
#
#   1. A negative within-varietal slope of price on (centred) altitude.
#   2. Varietal baseline altitude and varietal baseline price that both rise
#      together across the five varietals, strongly enough that the
#      between-varietal trend dominates the pooled regression and flips the
#      sign relative to the within-varietal slope.
#
# Every other relationship (yield vs price, farm size vs price, tree age vs
# price, farm size vs age) is built to carry the SAME sign pooled and within,
# so there remains exactly ONE reversal in the dataset.

n_per_varietal <- 120

varietals <- c("Bourbon", "Caturra", "Typica", "Catuai", "Gesha")

regions <- c("North", "South", "Central")
certifications <- c("Organic", "Conventional")

# --- Varietal baselines -------------------------------------------------
#
# Altitude and price baselines both increase across the five varietals.
# Retuned (Sept 2026) to make the reversal much stronger in both directions:
# pooled cor(altitude, price) ~ +0.67 (up from +0.23), and every varietal's
# within-group cor(altitude, price) sits around -0.36 to -0.49 (up from
# roughly -0.19 to -0.35). This came from widening varietal_price_offset
# (~4x the previous gaps) together with a steeper altitude_price_gain and a
# lower price_noise_sd -- widening the price offsets alone barely moves the
# pooled correlation, because a steeper negative within-varietal slope adds
# negative covariance that fights the between-varietal signal; the offsets
# have to widen enough to swamp that penalty.
#
# Uncoloured, the scatter still reads as one cloud (widest gap along the
# scatter's principal axis is ~0.12 sd, well under the single-cloud
# threshold of 0.5), but the price separation between adjacent varietals is
# now much larger (~0.8-1.2 within-sd, vs ~0.2-0.3 before) -- so the pooled
# trend is visibly steeper and the within-varietal downward lines are
# visibly steeper too, at some cost to how subtle the paradox is before
# colouring. If it needs to be reined back in, price_noise_sd is the softest
# lever; altitude_price_gain and varietal_price_offset are what carry the
# strength of each half of the reversal.
varietal_altitude_base <- c(
  Bourbon = 1080,
  Caturra = 1230,
  Typica  = 1360,
  Catuai  = 1470,
  Gesha   = 1600
)

varietal_price_offset <- c(
  Bourbon = 0.00,
  Caturra = 2.48,
  Typica  = 4.64,
  Catuai  = 6.48,
  Gesha   = 8.08
)

# Baseline yield by varietal, ordered inversely to price so yield vs price is
# negative both pooled and within.
varietal_yield_base <- c(
  Bourbon = 1430,
  Caturra = 1350,
  Typica  = 1260,
  Catuai  = 1170,
  Gesha   = 1030
)

# Dollars per kg gained per metre of altitude WITHIN a varietal. Negative:
# the reversal runs low-grown-is-better. Strengthened from -0.0075 to make
# the within-varietal downward slope more pronounced.
altitude_price_gain <- -0.012

# Dollars per kg per hectare of farm size WITHIN a varietal. Positive, kept
# the same sign pooled and within.
farm_price_gain <- 0.55

# Residual price scatter, and the kg/ha lost per dollar of within-varietal
# price premium. Noise sd lowered from 0.42 to 0.30 -- less scatter around
# each varietal's line sharpens the within-varietal correlation further.
price_noise_sd   <- 0.30
yield_price_gain <- -90

coffee <- tibble(
  varietal = factor(rep(varietals, each = n_per_varietal), levels = varietals)
) |>
  mutate(
    growing_region = sample(regions, n(), replace = TRUE),
    certification  = sample(certifications, n(), replace = TRUE, prob = c(0.45, 0.55)),

    # Farm size -- overlapping ranges across varietals (this is the Q2
    # control variable, so it must not separate cleanly by varietal).
    farm_size_ha = round(rnorm(n(), mean = 13, sd = 3.2), 1),
    farm_size_ha = pmax(farm_size_ha, 2),

    # Tree age -- derived from farm size, keeping the practice item
    # (size vs age) positive at both levels.
    tree_age_years = round(4 + 0.85 * farm_size_ha + rnorm(n(), 0, 1.4), 1),

    # Altitude -- varietal baseline plus noise.
    altitude_m = round(
      varietal_altitude_base[as.character(varietal)] + rnorm(n(), 0, 75),
      0
    )
  ) |>
  # Centre the price drivers within varietal, so their coefficients act
  # purely within a varietal and the between-varietal pattern is set by the
  # baseline offsets alone.
  mutate(
    altitude_c = altitude_m   - mean(altitude_m),
    farm_c     = farm_size_ha - mean(farm_size_ha),
    .by = varietal
  ) |>
  mutate(
    price_per_kg_usd = round(
      5.60 +
        varietal_price_offset[as.character(varietal)] +
        altitude_c * altitude_price_gain +
        farm_c     * farm_price_gain +
        rnorm(n(), 0, price_noise_sd),
      2
    )
  ) |>
  # Yield is derived from centred price so its sign is pinned at BOTH
  # levels: negative within varietal by construction, negative pooled via
  # the baselines.
  mutate(
    price_c = price_per_kg_usd - mean(price_per_kg_usd),
    .by = varietal
  ) |>
  mutate(
    yield_kg_per_ha = round(
      varietal_yield_base[as.character(varietal)] +
        price_c * yield_price_gain +
        rnorm(n(), 0, 70),
      0
    )
  ) |>
  select(
    varietal, growing_region, certification,
    farm_size_ha, tree_age_years,
    altitude_m, yield_kg_per_ha, price_per_kg_usd
  ) |>
  # Shuffle row order so the varietal-block structure isn't visible.
  slice_sample(prop = 1)

write_csv(coffee, here(base_dir, "data", "coffee.csv"))

# --- Quick sanity check ------------------------------------------------------

coffee |>
  summarise(
    across(
      c(farm_size_ha, tree_age_years, altitude_m, yield_kg_per_ha, price_per_kg_usd),
      list(mean = mean, sd = sd),
      .names = "{.col}__{.fn}"
    ),
    .by = varietal
  ) |>
  print(width = Inf)

# The reversal the survey turns on. Expected:
#   pooled altitude ~ price  POSITIVE, within-varietal NEGATIVE for all five
cat("\nPooled across all farms:\n")
cat(sprintf("  cor(altitude, price)    = %+.3f\n",
            cor(coffee$altitude_m, coffee$price_per_kg_usd)))
cat(sprintf("  slope(price ~ altitude) = %+.5f\n",
            coef(lm(price_per_kg_usd ~ altitude_m, coffee))[2]))

cat("\nWithin each varietal:\n")
coffee |>
  summarise(
    slope_altitude_price = coef(lm(price_per_kg_usd ~ altitude_m))[2],
    cor_altitude_price   = cor(altitude_m,      price_per_kg_usd),
    .by = varietal
  ) |>
  print()

# Same-sign checks: everything EXCEPT altitude must agree pooled and within,
# so that exactly one reversal exists in the dataset.
cat("\nSame-sign checks (pooled vs within should match in sign):\n")
cat(sprintf("  pooled cor(yield, price)     = %+.2f\n",
            cor(coffee$yield_kg_per_ha, coffee$price_per_kg_usd)))
cat(sprintf("  pooled cor(farm size, price) = %+.2f\n",
            cor(coffee$farm_size_ha, coffee$price_per_kg_usd)))
cat(sprintf("  pooled cor(tree age, price)  = %+.2f\n",
            cor(coffee$tree_age_years, coffee$price_per_kg_usd)))
cat(sprintf("  pooled cor(farm size, age)   = %+.2f   [practice item]\n",
            cor(coffee$farm_size_ha, coffee$tree_age_years)))
coffee |>
  summarise(
    cor_yield_price = cor(yield_kg_per_ha,  price_per_kg_usd),
    cor_farm_price  = cor(farm_size_ha,     price_per_kg_usd),
    cor_age_price   = cor(tree_age_years,   price_per_kg_usd),
    cor_farm_age    = cor(farm_size_ha,     tree_age_years),
    .by = varietal
  ) |>
  print()

# Single-cloud check: uncoloured, altitude vs price must look like ONE group.
#
#   * how far apart adjacent varietal means sit, measured in within-varietal
#     sds -- under ~1.5 on an axis means the clouds overlap rather than split;
#   * the widest empty gap between neighbouring points projected onto the
#     cloud's principal axis, in sds -- well under 0.5 for a single cloud.
cat("\nSingle-cloud diagnostics (pooled scatter should show no split):\n")
coffee |>
  summarise(
    mean_a = mean(altitude_m),       sd_a = sd(altitude_m),
    mean_p = mean(price_per_kg_usd), sd_p = sd(price_per_kg_usd),
    .by = varietal
  ) |>
  arrange(mean_a) |>
  summarise(
    altitude_sep = paste(sprintf("%.2f", diff(mean_a) / mean(sd_a)), collapse = ", "),
    price_sep    = paste(sprintf("%.2f", diff(mean_p) / mean(sd_p)), collapse = ", ")
  ) |>
  with(cat(sprintf(
    "  adjacent varietal-mean separation (within-sd units): altitude %s | price %s\n",
    altitude_sep, price_sep
  )))

local({
  pc <- prcomp(scale(cbind(coffee$altitude_m, coffee$price_per_kg_usd)))$x[, 1]
  cat(sprintf("  widest gap along principal axis = %.2f sd\n",
              max(diff(sort(pc))) / sd(pc)))
})

# --- Answer key --------------------------------------------------------------
#
#   Practice  farm size vs tree age            -> Positively related
#   Q1        breed/buy for price              -> LOWER altitude
#   Q2        same farm size, higher altitude  -> Gesha
#   Q3        highest average yield per ha     -> Bourbon
#   Q4        highest average altitude         -> Gesha
#   Q5        yield vs price                   -> Negatively related

#ggpairs(coffee)

# Uncoloured: should read as a single cloud drifting upward.
coffee %>%
  ggplot(aes(x = altitude_m, y = price_per_kg_usd)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "dashed")

# Coloured: the same cloud splits into five downward-sloping varietals.
coffee %>%
  ggplot(aes(x = altitude_m, y = price_per_kg_usd, color = varietal)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              colour = "black", linetype = "dashed")

