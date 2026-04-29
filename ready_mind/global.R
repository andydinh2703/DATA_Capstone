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
    County    = str_trim(ifelse(County == "Saint Lawrence", "St. Lawrence", County))
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
  sf::st_as_sf() %>%
  sf::st_transform(4326)

proficiency_counties <- sort(unique(proficiency_df$County))

# ── Geneva Grade-Level Proficiency ───────────────────────
geneva_grade_raw <- read_csv(
  here::here("data", "raw_data", "ready_mind", "NYS_proficiency_3_4_8.csv"),
  col_types = cols(.default = col_character())
)

geneva_grade_df <- geneva_grade_raw %>%
  mutate(
    Year  = as.integer(Year),
    Grade = as.integer(Grade),
    PCT   = as.numeric(PCT)
  ) %>%
  # For grade 8 math from 2015+: Combined covers both NYSTP and Regents students;
  # drop the component rows so we don't double-count.
  filter(!(Grade == 8 & Subject %in% c("MATH NYSTP", "MATH Regents") & Year >= 2015)) %>%
  mutate(
    Subject = case_when(
      Subject %in% c("MATH", "MATH NYSTP", "MATH Combined") ~ "Math",
      Subject == "ELA" ~ "ELA",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Subject), !is.na(PCT)) %>%
  select(Year, Grade, Subject, PCT)

# ── English Language Learners ─────────────────────────────
ell_raw <- read_csv(
  here::here("data", "raw_data", "ready_mind", "englishLL.csv"),
  col_types = cols(.default = col_character())
)

ell <- ell_raw %>%
  filter(!is.na(Count)) %>%
  mutate(
    year_start = as.integer(substr(Year, 1, 4)),
    Count      = as.integer(gsub(",", "", Count)),
    Enrollment = suppressWarnings(as.integer(gsub(",", "", Enrollment))),
    Proportion = as.numeric(Proportion)
  )
