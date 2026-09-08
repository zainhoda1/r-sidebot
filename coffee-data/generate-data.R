library(tidyverse)

set.seed(42)

n_per_varietal <- 120
varietals <- c("Bourbon", "Typica", "Gesha")
regions <- c("North", "South", "Central")
certifications <- c("Organic", "Conventional")

# --- Varietal baselines -------------------------------------------------

varietal_altitude_base <- c(
  Bourbon = 1232,
  Typica = 1330,
  Gesha = 1428
)

varietal_price_offset <- c(
  Bourbon = 0.00,
  Typica = 2.05,
  Gesha = 4.10
)

# Baseline yield by varietal, ordered inversely to price so yield vs price is
# negative both pooled and within.
varietal_yield_base <- c(
  Bourbon = 1430,
  Typica = 1260,
  Gesha = 1030
)

# Dollars per kg gained per metre of altitude WITHIN a varietal. Negative:
# the reversal runs low-grown-is-better.
altitude_price_gain <- -0.018

# Dollars per kg per acre of farm size WITHIN a varietal. Positive.
farm_price_gain <- 0.25

# Residual price scatter, and the kg/acre lost per dollar of within-varietal
# price premium.
price_noise_sd <- 0.25
yield_price_gain <- -90

coffee <- tibble(
  varietal = factor(rep(varietals, each = n_per_varietal), levels = varietals)
) |>
  mutate(
    growing_region = sample(regions, n(), replace = TRUE),
    certification = sample(
      certifications,
      n(),
      replace = TRUE,
      prob = c(0.45, 0.55)
    ),

    # Farm size -- overlapping ranges across varietals (this is the Q2
    # control variable, so it must not separate cleanly by varietal).
    farm_size_acres = round(rnorm(n(), mean = 13, sd = 3.2), 1),
    farm_size_acres = pmax(farm_size_acres, 2),

    # Tree age -- derived from farm size, keeping the practice item
    # (size vs age) positive at both levels.
    tree_age_years = round(4 + 0.85 * farm_size_acres + rnorm(n(), 0, 1.4), 1),

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
    altitude_c = altitude_m - mean(altitude_m),
    farm_c = farm_size_acres - mean(farm_size_acres),
    .by = varietal
  ) |>
  mutate(
    price_per_kg_usd = round(
      5.60 +
        varietal_price_offset[as.character(varietal)] +
        altitude_c * altitude_price_gain +
        farm_c * farm_price_gain +
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
    yield_kg_per_acre = round(
      varietal_yield_base[as.character(varietal)] +
        price_c * yield_price_gain +
        rnorm(n(), 0, 70),
      0
    )
  ) |>
  select(
    varietal,
    growing_region,
    certification,
    farm_size_acres,
    tree_age_years,
    altitude_m,
    yield_kg_per_acre,
    price_per_kg_usd
  ) |>
  # Shuffle row order so the varietal-block structure isn't visible.
  slice_sample(prop = 1)

write_csv(coffee, file.path("coffee-data", "coffee.csv"))

# --- Answer key --------------------------------------------------------------
#
#   Practice  farm size vs tree age            -> Positively related
#   Q1        breed/buy for price              -> LOWER altitude
#   Q2        same farm size, higher altitude  -> Gesha
#   Q3        highest average yield per acre  -> Bourbon
#   Q4        highest average altitude         -> Gesha
#   Q5        yield vs price                   -> Negatively related

# --- Answer-key checks -------------------------------------------------------
# One chart per item above, saved at the same 6:4 ratio.

# Practice: farm size vs tree age -> positively related.
plot_practice <- coffee %>%
  ggplot(aes(x = farm_size_acres, y = tree_age_years)) +
  geom_point(alpha = 0.6)

# Q1: within each varietal, price falls as altitude rises -> buy LOWER altitude.
# Uncoloured: should read as a single cloud.
plot_uncoloured <- coffee %>%
  ggplot(aes(x = altitude_m, y = price_per_kg_usd)) +
  geom_point(alpha = 0.6)

# Coloured: the same cloud splits into three downward-sloping varietals.
plot_coloured <- coffee %>%
  ggplot(aes(x = altitude_m, y = price_per_kg_usd, color = varietal)) +
  geom_point() +
  theme(legend.position = 'none')

# Faceted: each varietal on its own panel.
plot_q1 <- coffee %>%
  ggplot(aes(x = altitude_m, y = price_per_kg_usd)) +
  geom_point(alpha = 0.6) +
  facet_wrap(vars(varietal))

# Q2: at any given farm size, Gesha sits highest on altitude.
plot_q2 <- coffee %>%
  ggplot(aes(x = farm_size_acres, y = altitude_m, color = varietal)) +
  geom_point(alpha = 0.6)

# Q3: highest average yield per acre -> Bourbon.
plot_q3 <- coffee %>%
  ggplot(aes(x = varietal, y = yield_kg_per_acre)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "red")

# Q4: highest average altitude -> Gesha.
plot_q4 <- coffee %>%
  ggplot(aes(x = varietal, y = altitude_m)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "red")

# Q5: yield vs price -> negatively related, pooled and within varietal.
plot_q5 <- coffee %>%
  ggplot(aes(x = price_per_kg_usd, y = yield_kg_per_acre)) +
  geom_point(aes(color = varietal), alpha = 0.6)

check_plots <- list(
  practice = plot_practice,
  "q1-uncoloured" = plot_uncoloured,
  "q1-coloured" = plot_coloured,
  q1 = plot_q1,
  q2 = plot_q2,
  q3 = plot_q3,
  q4 = plot_q4,
  q5 = plot_q5
)

for (nm in names(check_plots)) {
  ggsave(
    file.path("coffee-data", "checks", paste0("check-", nm, ".png")),
    check_plots[[nm]],
    width = 6,
    height = 4,
    dpi = 300
  )
}
