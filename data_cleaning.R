
# ============================================================
# Data Cleaning: South Dakota Traffic Fatalities
# Years: 2023 and 2024
#
# Purpose:
#   1. Combine 2023 and 2024 FARS accident data
#   2. Filter accidents to South Dakota
#   3. Read county population data
#   4. Download South Dakota county boundaries
#   5. Calculate county-level crash and fatality rates
#   6. Save cleaned datasets for the Shiny application
# ============================================================


# ------------------------------------------------------------
# 1. Load libraries
# ------------------------------------------------------------

library(dplyr)
library(readxl)
library(tigris)
library(sf)
library(tidyr)


# ------------------------------------------------------------
# 2. Read accident datasets
# ------------------------------------------------------------

accident_2023 <- read.csv("accident_2023.csv")

accident_2024 <- read.csv("accident_2024.csv")


# ------------------------------------------------------------
# 3. Combine datasets and filter South Dakota
# ------------------------------------------------------------

accidents <- bind_rows(
  accident_2023,
  accident_2024
)

# South Dakota FIPS state code = 46

sd_accidents <- accidents %>%
  filter(as.integer(STATE) == 46) %>%
  mutate(
    YEAR = as.integer(YEAR),
    FATALS = as.numeric(FATALS),
    VE_TOTAL = as.numeric(VE_TOTAL),
    PERSONS = as.numeric(PERSONS)
  )


# ------------------------------------------------------------
# 4. Clean county identification codes
# ------------------------------------------------------------

# FARS county codes are three digits.
# Combine state and county codes to create GEOID.

sd_accidents <- sd_accidents %>%
  mutate(
    COUNTY_CODE = suppressWarnings(as.integer(COUNTY)),
    COUNTY = if_else(
      !is.na(COUNTY_CODE),
      sprintf("%03d", COUNTY_CODE),
      NA_character_
    ),
    GEOID = if_else(
      !is.na(COUNTY) & COUNTY_CODE > 0,
      paste0("46", COUNTY),
      NA_character_
    )
  )


# ------------------------------------------------------------
# 5. Read county population data
# ------------------------------------------------------------

county_population_raw <- read_excel(
  "sd_counties_pop.xlsx",
  sheet = "Sheet1"
)

# Separate statewide population from county populations

state_population <- county_population_raw %>%
  filter(Location == "South Dakota") %>%
  select(
    POP_2023 = `2023_pop`,
    POP_2024 = `2024_pop`
  )

# Keep the 66 South Dakota counties

county_population <- county_population_raw %>%
  filter(Location != "South Dakota") %>%
  mutate(
    COUNTYNAME = sub("^\\.", "", Location),
    COUNTYNAME = sub(" County, South Dakota$", "", COUNTYNAME)
  ) %>%
  select(
    COUNTYNAME,
    POP_2023 = `2023_pop`,
    POP_2024 = `2024_pop`
  )

# ------------------------------------------------------------
# 6. Download South Dakota county boundaries
# ------------------------------------------------------------

options(tigris_use_cache = TRUE)

sd_counties <- counties(
  state = "SD",
  cb = TRUE,
  year = 2024
) %>%
  select(
    GEOID,
    NAME,
    geometry
  )

# Add population information to county boundaries

sd_counties <- sd_counties %>%
  left_join(
    county_population,
    by = c("NAME" = "COUNTYNAME")
  )


# ------------------------------------------------------------
# 7. Create county-level crash summaries
# ------------------------------------------------------------

# Count fatal crashes and total fatalities by county and year

county_crashes <- sd_accidents %>%
  filter(!is.na(GEOID)) %>%
  group_by(YEAR, GEOID) %>%
  summarise(
    FATAL_CRASHES = n(),
    TOTAL_FATALITIES = sum(FATALS, na.rm = TRUE),
    AVG_FATALITIES = mean(FATALS, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 8. Include all counties and both years
# ------------------------------------------------------------

# This ensures counties with zero fatal crashes
# are still included in our analysis.

sd_county_summary <- crossing(
  GEOID = sd_counties$GEOID,
  YEAR = c(2023, 2024)
) %>%
  left_join(
    county_crashes,
    by = c("GEOID", "YEAR")
  ) %>%
  left_join(
    sd_counties %>%
      st_drop_geometry() %>%
      select(
        GEOID,
        COUNTYNAME = NAME,
        POP_2023,
        POP_2024
      ),
    by = "GEOID"
  ) %>%
  mutate(
    FATAL_CRASHES = replace_na(FATAL_CRASHES, 0L),
    TOTAL_FATALITIES = replace_na(TOTAL_FATALITIES, 0),
    POPULATION = if_else(
      YEAR == 2023,
      as.numeric(POP_2023),
      as.numeric(POP_2024)
    ),
    CRASH_RATE = FATAL_CRASHES / POPULATION * 100000,
    FATALITY_RATE = TOTAL_FATALITIES / POPULATION * 100000
  ) %>%
  select(
    YEAR,
    GEOID,
    COUNTYNAME,
    FATAL_CRASHES,
    TOTAL_FATALITIES,
    AVG_FATALITIES,
    POPULATION,
    CRASH_RATE,
    FATALITY_RATE
  )


# ------------------------------------------------------------
# 9. Calculate statewide statistics
# ------------------------------------------------------------

sd_summary <- sd_accidents %>%
  group_by(YEAR) %>%
  summarise(
    FATAL_CRASHES = n(),
    TOTAL_FATALITIES = sum(FATALS, na.rm = TRUE),
    AVG_FATALITIES = mean(FATALS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    POPULATION = c(
      state_population$POP_2023,
      state_population$POP_2024
    )[match(YEAR, c(2023, 2024))],
    CRASH_RATE = FATAL_CRASHES / POPULATION * 100000,
    FATALITY_RATE = TOTAL_FATALITIES / POPULATION * 100000
  )


# ------------------------------------------------------------
# 10. Save cleaned datasets
# ------------------------------------------------------------

saveRDS(
  sd_accidents,
  "sd_accidents.rds"
)

saveRDS(
  sd_counties,
  "sd_counties.rds"
)

saveRDS(
  sd_county_summary,
  "sd_county_summary.rds"
)

saveRDS(
  sd_summary,
  "sd_summary.rds"
)

