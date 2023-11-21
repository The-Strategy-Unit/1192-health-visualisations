# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2020
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(nomisr)

# specific format for Nomis
la_names_nomis <- paste(la_names, collapse = ",")

# Local Authority population --------------------------------------------

# Find Nomis codes for the geographical areas listed previously
# Multiple lines are returned with repeated data so only first is returned (matches the API method)
dat_la <- nomisr::nomis_codelist(
  id = "NM_2002_1",
  concept = "geography",
  search = la_names_nomis
) |>
  dplyr::slice_head(
    n = 1,
    by = label.en
  )

la_r <- nomisr::nomis_get_data(
  id = "NM_2002_1",
  geography = dat_la$id,
  time = "latest",
  tidy = TRUE
) |>
  filter(
    measures_name == "Value",
    !c_age <= 18, # Exclude the grouped ages
    !c_age >= 201,
    !c_age == c(191, 200),
    !c_age_name == "All Ages"
  )


all_geographies <- la_r %>%
  select(
    period = date_name,
    area_code = geography_code,
    area_name = geography_name,
    gender = gender_name,
    age = c_age_name,
    count = obs_value,
    geography
  ) %>%
  mutate(
    period = ymd(str_c(period, "06-30", sep = "-")),
    gender = fct_recode(gender, "Females" = "Female", "Males" = "Male"),
    age = as.integer(str_trim(str_replace_all(age, "Age.|\\+", "")))
  ) %>%
  spread(age, count) %>%
  gather(age, n, -period, -area_code, -area_name, -geography, -gender) %>%
  mutate(age = as.integer(age))


# Ethnicity data ----------------------------------------------------------


dat_la_eth <- nomisr::nomis_codelist(
  id = "NM_608_1",
  concept = "geography",
  search = la_names_nomis
) |>
  dplyr::arrange(label.en) |> 
  dplyr::slice_head(
    n = 1,
    by = label.en
  )

la_r_eth <- nomisr::nomis_get_data(
  id = "NM_608_1",
  geography = dat_la_eth$id,
  time = "latest",
  tidy = TRUE
) |> 
  filter(
    measures_name == "Value",
    rural_urban_name == "Total",
    cell %in% c(100, 200, 300, 400, 500),
    measures == 20100
  ) |> 
  select(date_name,
         geography_name,
         geography_code,
         rural_urban_name,
         cell_name,
         measures_name,
         obs_value,
         obs_status_name)
