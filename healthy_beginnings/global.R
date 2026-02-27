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
  here::here("data", "LowBirthWeight.csv"),
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
  here::here("data", "Infant Mortality Data.csv"),
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
