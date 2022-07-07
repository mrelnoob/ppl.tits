# ---------------------------------------- #
##### Functions to prepare my datasets #####
# ---------------------------------------- #

# The functions of this R file are meant to prepare datasets: e.g. aggregate observations,
# delete or add variables, etc. But NOT to directly explore and clean data for analyses!



### ______________________________________
#' Aggregate tits data by nestbox and year
#'
#' @description The `aggreg_by_nest` function aggregates the raw tits data to create a table
#' summarizing tits' breeding success for each nestbox each year where breeding occurred. As such,
#' it only keeps information on nestlings/juveniles and disregards the lines about adult tits.
#'
#' @return A tibble with 15 variables.
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' nestbox_repro <- aggreg_by_nest()
#' }
aggreg_by_nest <- function(){
  rtits <- ppl.tits::import_raw_tits_data()

  rtits %>% dplyr::filter(age == "nestling") %>%
    dplyr::group_by(id_nestbox, year) %>%
    dplyr::summarise(date = dplyr::first(date),
                     laying_date = dplyr::first(laying_date),
                     incubation_date = dplyr::first(incubation_date),
                     hatching_date = dplyr::first(hatching_date),
                     clutch_size = max(clutch_size),
                     brood_size = max(brood_size),
                     fledgling_nb = max(fledgling_nb),
                     father_id = dplyr::first(father_id),
                     mother_id = dplyr::first(mother_id),
                     species = dplyr::first(species),
                     mass = mean(nestling_mass_j13),
                     tarsus_length = mean(nestling_tarsus_l),
                     wing_length = mean(nestling_wing_l)) -> xxx

  return(xxx)
}









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

