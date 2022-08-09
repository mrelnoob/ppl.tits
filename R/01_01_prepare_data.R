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
#' @return A tibble with 16 variables.
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
                     success_manipulated = dplyr::first(success_manipulated),
                     father_id = dplyr::first(father_id),
                     mother_id = dplyr::first(mother_id),
                     species = dplyr::first(species),
                     mass = mean(nestling_mass),
                     tarsus_length = mean(nestling_tarsus_l),
                     wing_length = mean(nestling_wing_l)) -> xxx

  age <- id_nestbox <- year <- laying_date <- incubation_date <- hatching_date <- clutch_size <- NULL
  brood_size <- fledgling_nb <- success_manipulated <- father_id <- mother_id <- species <- nestling_mass <- NULL
  nestling_tarsus_l <- nestling_wing_l <- NULL

  return(xxx)
}





### ______________________________________
#' Export tits nestling aggregated dataset
#'
#' @description The `export_nestling_aggreg` function exports the dataset (.csv) created by the
#' `aggreg_by_nest` function, that is the tits nestling data aggregated by nestbox and year.
#'
#' @return A CSV table and its path.
#'
#' @export
#' @importFrom readr write_csv2
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#' export_nestling_aggreg()
#' }
export_nestling_aggreg <- function(){
  aabb <- ppl.tits::aggreg_by_nest()
  readr::write_csv2(x = aabb, file = here::here("output", "tables", "tits_nestling_data.csv"))
  return(here::here("output", "tables", "tits_nestling_data.csv"))
}






# ###### TEMP AGREGation (ONGOING package development)
# ### @ImportFrom to be added:
# readr read_csv2
# readr cols
# readr col_factor
# readr col_double
# here here
# tidyr separate
# dplyr mutate
# dplyr across
#
# # Attribute ID to nest_years:
# loco <- readr::read_csv2(here::here("mydata", "paired_boxtemp.csv"), col_names = TRUE, na = "NA",
#                         col_types = readr::cols(id_nestbox = readr::col_factor(),
#                                                 year = readr::col_factor(),
#                                                 lcz1 = readr::col_factor(),
#                                                 dist_lcz = readr::col_double(),
#                                                 temp_id_lcz = readr::col_factor(),
#                                                 dist_pp = readr::col_double(),
#                                                 temp_id_pp = readr::col_factor(),
#                                                 temp_id_final = readr::col_factor()))
# tits <- ppl.tits::aggreg_by_nest()
#
# tits$temp_station_id <- loco$temp_id_final
#
# # Format TEMP data:
# temp_raw <- readr::read_csv2(here::here("mydata", "temp_data_20192021.csv"), col_names = TRUE, na = "NA",
#                              col_types = readr::cols(time = readr::col_factor()))
# temp_raw %>%
#   tidyr::separate(time, c('date', 'hour'), sep = " ") %>%
#   tidyr::separate(date, c('day', 'month', 'year'), sep = "/") %>%
#   dplyr::mutate(dplyr::across(where(is.character), factor)) %>%
#   dplyr::mutate(dplyr::across(where(is.logical), numeric)) -> temp
# summary(temp)
# # s70 a un PB!
