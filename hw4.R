
# QUESTION 3

library(tidyverse)

population <- read_csv("county_pop_arcos.csv", show_col_types = FALSE)
annual <- read_csv("county_annual.csv", show_col_types = FALSE)
land <- read_csv("land_area.csv", show_col_types = FALSE)

# fixinf Montgomery County FIPS code
annual <- annual %>%
  mutate(
    countyfips = if_else(
      BUYER_STATE == "AR" & BUYER_COUNTY == "MONTGOMERY",
      "05097",
      as.character(countyfips)
    )
  )

# Remove rows with missing county info
annual <- annual %>%
  filter(!is.na(BUYER_COUNTY))

# Keep needed land variables and rename FIPS variable
land_area <- land %>%
  select(Areaname, STCOU, LND110210D) %>%
  rename(countyfips = STCOU)

# Merge population and land area
county_info <- population %>%
  left_join(
    land_area,
    by = "countyfips"
  )

# Merge annual opioid data with county information by county and year
opioid <- annual %>%
  left_join(
    county_info,
    by = c("countyfips", "year")
  )

# Make sure variables are numeric
opioid <- opioid %>%
  mutate(
    population = as.numeric(population),
    LND110210D = as.numeric(LND110210D),
    DOSAGE_UNIT = as.numeric(DOSAGE_UNIT)
  )

# Calculate population density
opioid <- opioid %>%
  mutate(
    density = population / LND110210D
  )

head(opioid)
dim(opioid)