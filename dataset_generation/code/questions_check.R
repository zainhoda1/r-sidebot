library(GGally)
library(here)


dragons <- read_csv(here("dragon_dataset_generation", "data", "dragons.csv"), show_col_types = FALSE)

dragons_opposite <- read_csv(here("dragon_dataset_generation","data", "dragons_opposite.csv"),
                             show_col_types = FALSE)



ggpairs(dragons)

ggpairs(dragons_opposite)


dragons |>
ggplot(aes(x = dragon_type, y = flying_speed_kmh)) +
  geom_bar(stat= 'identity')
