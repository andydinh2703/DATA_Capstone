# ──────────────────────────────────────────────────────────
# global.R — Data loading & shared objects
# Ready Mind Dashboard
# ──────────────────────────────────────────────────────────

# ── School District Proficiency ──────────────────────────
proficiency_raw <- read_csv(
  here::here("data", "raw_data", "ready_mind", "data-BhuEI.csv"),
  col_types = cols(.default = col_character())
)

proficiency_df <- proficiency_raw %>%
  rename(
    ela_rank  = `ELA rank`,
    ela_pct   = `% proficient ELA`,
    math_rank = `Math rank`,
    math_pct  = `% proficient math`
  ) %>%
  mutate(
    ela_pct   = as.numeric(gsub("%", "", ela_pct)),
    math_pct  = suppressWarnings(as.numeric(gsub("%", "", math_pct))),
    ela_rank  = suppressWarnings(as.integer(ela_rank)),
    math_rank = suppressWarnings(as.integer(math_rank)),
    District  = str_trim(District),
    County    = str_trim(County)
  )

# ── County-level averages ─────────────────────────────────
proficiency_county_avg <- proficiency_df %>%
  group_by(County) %>%
  summarise(
    ela_avg     = round(mean(ela_pct,  na.rm = TRUE), 1),
    math_avg    = round(mean(math_pct, na.rm = TRUE), 1),
    n_districts = n(),
    .groups     = "drop"
  )

# ── Join with NY county geometries ───────────────────────
proficiency_ny_counties <- tigris::counties(
  state = "NY", cb = TRUE, progress_bar = FALSE
) %>%
  rename(County = "NAME") %>%
  left_join(proficiency_county_avg, by = "County") %>%
  sf::st_as_sf()

proficiency_counties <- sort(unique(proficiency_df$County))
