# To create functions to prepare data: e.g. aggregate observations, delete or add variables, etc. But NOT directly
# to explore and clean data!



### Open the table:
tits <- readr::read_csv2(here::here("mydata", "ppl_dijon_tits_data_20192021.csv"), col_names = TRUE, na = "NA",
                         col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                 site = readr::col_factor(),
                                                 year = readr::col_factor(),
                                                 date = readr::col_factor(),
                                                 laying_date = readr::col_factor(),
                                                 incubation_date = readr::col_factor(),
                                                 hatching_date = readr::col_factor(),
                                                 clutch_size = readr::col_integer(),
                                                 brood_size = readr::col_integer(),
                                                 fledgling_nb = readr::col_integer(),
                                                 success = readr::col_factor(),
                                                 father_id = readr::col_factor(),
                                                 mother_id = readr::col_factor(),
                                                 id_bird = readr::col_factor(),
                                                 id_ring = readr::col_factor(),
                                                 species = readr::col_factor(),
                                                 sex = readr::col_factor(),
                                                 age = readr::col_factor(),
                                                 adult_mass = readr::col_double(),
                                                 adult_tarsus_l = readr::col_double(),
                                                 adult_wing_l = readr::col_double(),
                                                 adult_aggress = readr::col_factor(),
                                                 nestling_mass_j13 = readr::col_double(),
                                                 nestling_mass_j14 = readr::col_double(),
                                                 nestling_tarsus_l = readr::col_double(),
                                                 nestling_wing_l = readr::col_double()))

summary(tits)
summary(tits$id_ring)



# I NEED TO DELETE obs where "age" is NA (n=55) and "success" = NA (n=6-7)

tits %>% dplyr::filter(species == "CC") -> CC
tits %>% dplyr::filter(species == "PM") -> PM
summary(CC)
summary(PM)

CC %>% dplyr::filter(age != "nestling" & age != "NA") -> adult_cc
PM %>% dplyr::filter(age != "nestling" & age != "NA") -> adult_pm
summary(adult_cc)
summary(adult_pm)



### Number of nestboxes per...
# Species:
CC %>% group_by(id_nestbox) %>%
  summarize(nb_obs = n()) # So 85 nestboxes with data for the 3 years (23 of which have data for several years)
PM %>% group_by(id_nestbox) %>%
  summarize(nb_obs = n()) # So 132 nestboxes with data for the 3 years (41 of which have data for several years)
adult_pm %>% group_by(id_nestbox) %>%
  summarize(nb_obs = n()) # So 93 nestboxes with adult data for the 3 years (41 of which have data for several years)

# Year:
CC %>% group_by(year, id_nestbox) %>%
  summarize(nb_obs = n()) -> cc_eff_year_box
PM %>% group_by(year, id_nestbox) %>%
  summarize(nb_obs = n()) -> pm_eff_year_box
adult_pm %>% group_by(year, id_nestbox) %>%
  summarize(nb_obs = n()) -> adultpm_eff_year_box

