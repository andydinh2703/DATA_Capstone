# ──────────────────────────────────────────────────────────
# global.R — Data loading & shared objects
# Geneva Kids Health Dashboard
# ──────────────────────────────────────────────────────────

library(tidyverse)
library(shiny)
library(bslib)
library(plotly)

# ── Color palette ───────────────────────────────────────
location_colors <- c(
  "Geneva"  = "#E74C3C",
  "Ontario" = "#2ECC71",
  "NYS"     = "#3498DB"
)

location_levels <- c("Geneva", "Ontario", "NYS")

# ── Load Low Birth Weight data ──────────────────────────
lbw_raw <- read_csv(
  here::here("data", "raw_data", "healthy_beginnings", "LowBirthWeight.csv"),
  col_types = cols(.default = col_character())
)

lbw <- lbw_raw %>%
  mutate(
    Total = as.numeric(gsub(",", "", Total)),
    Low   = as.numeric(gsub(",", "", Low)),
    Year  = as.integer(Year),
    Rate  = round(Low / Total * 100, 1),
    Location = factor(Location, levels = location_levels)
  ) %>%
  select(Location, Year, Total, Low, Rate) %>%
  arrange(Location, Year)

# ── Load Infant Mortality data ──────────────────────────
im_raw <- read_csv(
  here::here("data", "raw_data", "healthy_beginnings", "Infant Mortality Data.csv"),
  col_types = cols(.default = col_character())
)

im <- im_raw %>%
  mutate(
    Total  = as.numeric(gsub(",", "", Total)),
    Deaths = as.numeric(Deaths),
    Rate   = as.numeric(Rate),
    Year   = as.integer(Year),
    Location = factor(Location, levels = location_levels)
  ) %>%
  filter(Age == "Infant") %>%
  select(Location, Year, Total, Deaths, Rate) %>%
  arrange(Location, Year)

# ── Year range ──────────────────────────────────────────
year_min <- min(c(lbw$Year, im$Year), na.rm = TRUE)
year_max <- max(c(lbw$Year, im$Year), na.rm = TRUE)

# ── Load Low Birth Weight by County (for heatmap) ──────
lbw_county_raw <- read_csv(
  here::here("data", "raw_data", "healthy_beginnings", "lowbirthweight_bycounty.csv"),
  col_types = cols(.default = col_character())
)

lbw_county <- lbw_county_raw |>
  rename(
    birth_weight = "Birth Weight in Grams",
    live_births  = "Number of Live Births"
  ) |>
  mutate(
    Year        = as.integer(Year),
    live_births = as.numeric(gsub(",", "", live_births)),
    low_birth_weight = case_when(
      birth_weight %in% c("3500+", "3000-3499", "2500-2999") ~ "normal",
      birth_weight %in% c("2000-2499", "1500-1999", "1000-1499", "Under 1000") ~ "low"
    )
  ) |>
  filter(birth_weight != "Total") |>
  group_by(Year, County, low_birth_weight) |>
  summarise(live_births = sum(live_births), .groups = "drop") |>
  group_by(Year, County) |>
  mutate(
    total_births = sum(live_births),
    proportion   = live_births / total_births,
    percentage   = proportion * 100
  ) |>
  ungroup() |>
  filter(low_birth_weight == "low") |>
  mutate(County = ifelse(County == "St Lawrence", "St. Lawrence", County))

# ── Join with NY county geometries ──────────────────────
ny_counties <- tigris::counties(state = "NY", cb = TRUE, progress_bar = FALSE)
ny_counties <- ny_counties |> rename(County = "NAME")

birth_rate_with_counties <- lbw_county |>
  left_join(ny_counties, by = "County") |>
  sf::st_as_sf() |>
  sf::st_transform(4326)

lbw_county_year_min <- min(birth_rate_with_counties$Year, na.rm = TRUE)
lbw_county_year_max <- max(birth_rate_with_counties$Year, na.rm = TRUE)
