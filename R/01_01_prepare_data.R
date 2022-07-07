# ---------------------------------------- #
##### Functions to prepare my datasets #####
# ---------------------------------------- #

# The functions of this R file are meant to prepare datasets: e.g. aggregate observations,
# delete or add variables, etc. But NOT to directly explore and clean data for analyses!

# rtits <- ppl.tits::import_raw_tits_data()
#
#
# # A function to simplify and aggregate raw-tits data:
#
# rtits %>% dplyr::select(-site, -nestling_mass_j14) %>%
#   dplyr::filter(success != 'NA', age != 'NA') -> xxx
#
# summary(xxx)












# tits %>% dplyr::filter(species == "CC") -> CC
# tits %>% dplyr::filter(species == "PM") -> PM
# summary(CC)
# summary(PM)
#
# CC %>% dplyr::filter(age != "nestling" & age != "NA") -> adult_cc
# PM %>% dplyr::filter(age != "nestling" & age != "NA") -> adult_pm
# summary(adult_cc)
# summary(adult_pm)
#
#
#
# ### Number of nestboxes per...
# # Species:
# CC %>% group_by(id_nestbox) %>%
#   summarize(nb_obs = n()) # So 85 nestboxes with data for the 3 years (23 of which have data for several years)
# PM %>% group_by(id_nestbox) %>%
#   summarize(nb_obs = n()) # So 132 nestboxes with data for the 3 years (41 of which have data for several years)
# adult_pm %>% group_by(id_nestbox) %>%
#   summarize(nb_obs = n()) # So 93 nestboxes with adult data for the 3 years (41 of which have data for several years)
#
# # Year:
# CC %>% group_by(year, id_nestbox) %>%
#   summarize(nb_obs = n()) -> cc_eff_year_box
# PM %>% group_by(year, id_nestbox) %>%
#   summarize(nb_obs = n()) -> pm_eff_year_box
# adult_pm %>% group_by(year, id_nestbox) %>%
#   summarize(nb_obs = n()) -> adultpm_eff_year_box

