# ──────────────────────────────────────────────────────────
# global.R — Data loading & shared objects
# Stable Home Dashboard
# ──────────────────────────────────────────────────────────

library(tidyverse)
library(shiny)
library(bslib)
library(plotly)

# ── Shared constants ─────────────────────────────────────
location_colors <- c(
  "Geneva"  = "#E74C3C",
  "Ontario" = "#2ECC71",
  "NYS"     = "#3498DB"
)

location_levels <- c("Geneva", "Ontario", "NYS")

# ── Poverty (families below poverty line) ────────────────
poverty_raw <- read_csv(
  here::here("data", "raw_data", "stable home", "PovertyFamilies.csv"),
  col_types = cols(.default = col_character())
)

poverty_df <- poverty_raw %>%
  filter(Community %in% location_levels) %>%
  mutate(
    Location = factor(Community, levels = location_levels),
    Year     = as.integer(Year),
    PCT      = as.numeric(PCT)
  ) %>%
  select(Location, Year, PCT) %>%
  arrange(Location, Year)

poverty_year_min <- min(poverty_df$Year, na.rm = TRUE)
poverty_year_max <- max(poverty_df$Year, na.rm = TRUE)

# ── Health insurance coverage ─────────────────────────────
insurance_raw <- read_csv(
  here::here("data", "raw_data", "stable home", "HealthInsCoverage.csv"),
  col_types = cols(.default = col_character())
)

insurance_df <- insurance_raw %>%
  mutate(
    Community = recode(Community, "City of Geneva" = "Geneva")
  ) %>%
  filter(Community %in% location_levels) %>%
  mutate(
    Location = factor(Community, levels = location_levels),
    Year     = as.integer(Year),
    PCT      = as.numeric(PCT)
  ) %>%
  select(Location, Year, PCT) %>%
  arrange(Location, Year)

insurance_year_min <- min(insurance_df$Year, na.rm = TRUE)
insurance_year_max <- max(insurance_df$Year, na.rm = TRUE)

# ── Family structure (households with children under 6) ──
family_raw <- read_csv(
  here::here("data", "raw_data", "stable home", "FamilyStructureUnder6A.csv"),
  col_types = cols(.default = col_character())
)

family_df <- family_raw %>%
  filter(
    Community %in% location_levels,
    Type %in% c("Two parents", "Single mother", "Single father")
  ) %>%
  mutate(
    Location = factor(Community, levels = location_levels),
    Year     = as.integer(Year),
    PCT      = round(as.numeric(Proportion) * 100, 1),
    Type     = factor(Type, levels = c("Two parents", "Single mother", "Single father"))
  ) %>%
  select(Location, Year, Type, PCT) %>%
  arrange(Location, Year, Type)

family_year_min <- min(family_df$Year, na.rm = TRUE)
family_year_max <- max(family_df$Year, na.rm = TRUE)

# ── Income ───────────────────────────────────────────────
income <- read_csv(
  here::here("data", "raw_data", "stable home", "Income.csv"),
  col_types = cols(.default = col_character())
) |>
  mutate(
    Year   = as.integer(Year),
    Income = as.numeric(gsub(",", "", Income))
  ) |>
  select(-Yr, -Source) |>
  rename(Location = "Community")

# ── Population ───────────────────────────────────────────
population <- read_csv(
  here::here("data", "raw_data", "stable home", "Population.csv"),
  col_types = cols(.default = col_character())
) |>
  mutate(
    Year       = as.integer(Year),
    Population = as.numeric(gsub(",", "", Population))
  ) |>
  select(-Source)

# ── Merge and compute indices (base year = 2014) ────────
pop_income <- income |>
  left_join(population, by = c("Year", "Location"))

pop_income_index <- pop_income |>
  filter(Year >= 2014) |>
  mutate(Location = recode(Location, "City of Geneva" = "Geneva")) |>
  group_by(Location) |>
  mutate(
    base_income      = Income[Year == 2014],
    income_index     = (Income / base_income) * 100,
    base_population  = Population[Year == 2014],
    population_index = (Population / base_population) * 100
  ) |>
  ungroup()

pi_year_min <- min(pop_income_index$Year, na.rm = TRUE)
pi_year_max <- max(pop_income_index$Year, na.rm = TRUE)

# ── SNAP / TANF ──────────────────────────────────────────
snap_raw <- read_csv(
  here::here("data", "raw_data", "stable home", "SnapOCDoSS.csv"),
  col_types = cols(.default = col_character())
)

snap_data <- snap_raw %>%
  select(-matches("^X"), -matches("^\\.\\.\\.")) %>%
  mutate(across(
    c(AllHouseholds, `Households w Child Under 18`,
      TANF, SNAP, SNAPPCT, TANFPCT),
    ~ suppressWarnings(as.numeric(.))
  )) %>%
  mutate(
    Community = str_trim(Community),
    Year = as.integer(Year)
  ) %>%
  filter(!is.na(Year))

snap_communities <- sort(unique(snap_data$Community))
snap_year_min    <- min(as.integer(snap_data$Year), na.rm = TRUE)
snap_year_max    <- max(as.integer(snap_data$Year), na.rm = TRUE)

community_colors <- c(
  "Geneva"            = "#E74C3C",
  "Ontario"           = "#2ECC71",
  "Geneva Town"       = "#9B59B6",
  "Ontario wo Geneva" = "#3498DB"
)
