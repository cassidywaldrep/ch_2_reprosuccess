# trying to figure out the differences in Ilsa's numbers and my numbers


# first looking at brood rearing 

cass <- tbl(conn, "mall_broodrearing_classified_IncompleteDataBirdsIncluded_ILSA") %>%
  collect() %>%
  rename(status_early_late_cass = status_early_late, 
         inc_duration.y_cass = inc_duration.y) %>%
  dplyr::select(birdid_year, Absx_mean, status_early_late_cass, inc_duration.y_cass)

cass %>%
  dplyr::select(birdid_year, status_early_late) %>%
  distinct() %>%
  group_by(status_early_late) %>%
  summarize(count = n())

missing_birds <- read_csv("results/ILSA_MALL_2022to2025_EarlyAndLateBroodRearingClassified_AllBirds_3Apr2026.csv") %>%
  rename(status_early_late_ilsa = status_early_late, 
         inc_duration.y_ilsa = inc_duration.y) %>%
  dplyr::select(birdid_year, Absx_mean, status_early_late_ilsa, inc_duration.y_ilsa ) %>%
  right_join(cass, by = "birdid_year") %>%
  filter(is.na(status_early_late_ilsa) == TRUE) %>%
  pull(birdid_year)

# Cassidy has 6 extra birds in the brood rearing analysis - were they included in nesting??
missing_birds


## looking at incubation data

ilsa_n <- read_csv("results/ILSA_MALL_2022to2025_StartEndDatesOfIncubationLayingPlusNestLocation_AllBirdsIncluded_30Mar2026.csv") %>%
  dplyr::select(birdid_year, status_laying, status_incubation) %>%
  rename(status_laying_ilsa = status_laying, 
         status_incubation_ilsa = status_incubation) %>%
  unique()

cass_n <- read_csv("results/MALL_2022to2025_StartEndDatesOfIncubationLayingPlusNestLocation_AllBirdsIncluded_24Jun2026.csv") %>%
  dplyr::select(birdid_year, status_laying, status_incubation) %>%
  rename(status_laying_cass = status_laying, 
         status_incubation_cass = status_incubation) %>%
  unique()

missing_nest <- cass_n %>%
  left_join(ilsa_n, by = "birdid_year") %>%
  filter(is.na(status_incubation_ilsa) == TRUE) %>%
  pull(birdid_year)


egg <- tbl(conn, "mall_egglaying_classified_last6daysAnd1inc_ILSA") %>%
  collect()

egg <- tbl(conn, "mall_incubation_classified_ILSA") %>%
  collect()

egg %>%
  dplyr::select(birdid_year, breeding_outcome) %>%
  distinct() %>%
  group_by(breeding_outcome) %>%
  summarize(count = n())
