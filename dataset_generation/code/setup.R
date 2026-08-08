# Load libraries

pkgs <- c(
  "fastDummies",
  "here",
  "lubridate",
  "tidyverse",
  "dotenv",
  "janitor",
  "psych",
  "nFactors",
  "rlang",
  "openxlsx",
  # "xlsx",
  "zipcodeR",
  "viridis",
  "maps",
  "ggridges",
  "purrr",
  "gt",
  "scales",
  "fixest",
  "cowplot",
  "GGally",
  "shiny",
  "bslib",
  "thematic"
)

installed <- pkgs %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(pkgs[!installed])
}

invisible(lapply(pkgs, library, character.only = TRUE))

`%notin%` <- Negate(`%in%`)

# Change dplyr settings so I can view all columns
options(dplyr.width = Inf)

