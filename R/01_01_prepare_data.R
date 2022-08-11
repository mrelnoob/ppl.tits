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
# tidyr unite
# dplyr select
# dplyr mutate
# dplyr across
# dplyr case_when
# dplyr group_by
# dplyr summarise
# dplyr relocate
# stats median
#
#
# ##### Attribute ID to nest_years and improve formating:
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
# tits$temp_station_id <- loco$temp_id_final
#
# tits %>%
#   tidyr::separate(laying_date, c('day2', 'month2', 'year2'), sep = "/") %>%
#   tidyr::unite(laying_date, c(year2, month2, day2), sep = "/", na.rm = TRUE) %>%
#   tidyr::separate(date, c('day3', 'month3', 'year3'), sep = "/") %>%
#   tidyr::unite(flight_date, c(year3, month3, day3), sep = "/") -> tits
# tits$flight_date <- as.Date(x = c(tits$flight_date), optional = TRUE)
# tits$laying_date <- as.Date(x = c(tits$laying_date), optional = TRUE)
# summary(tits)
#
# ### Creation of the "breeding_window" random factor
# tits %>% dplyr::group_by(year) %>%
#   dplyr::summarise(mid_date = stats::median(laying_date, na.rm = TRUE)) -> median_laydate
#
# # Imputation of the laying_date missing values (n=9)
# tits %>% dplyr::group_by(year) %>%
#   dplyr::summarise(mean_repro_duration = mean(flight_date, na.rm = TRUE) - mean(laying_date, na.rm = TRUE)) -> mrd
#
# tits %>% dplyr::mutate(laying_date = dplyr::case_when(
#   year == "2019" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[1]),
#   year == "2019" & is.na(laying_date) == FALSE ~ laying_date,
#   year == "2020" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[2]),
#   year == "2020" & is.na(laying_date) == FALSE ~ laying_date,
#   year == "2021" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[3]),
#   year == "2021" & is.na(laying_date) == FALSE ~ laying_date,
#   year == "2022" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[4]),
#   year == "2022" & is.na(laying_date) == FALSE ~ laying_date)) -> tits
#
# # Random factor generation
#
# tits %>% dplyr::mutate(breeding_window = dplyr::case_when(
#   year == "2019" & laying_date >= as.Date(median_laydate$mid_date[1]) ~ paste(year, "late", sep = "_"),
#   year == "2019" & laying_date < as.Date(median_laydate$mid_date[1]) ~ paste(year, "early", sep = "_"),
#   year == "2020" & laying_date >= as.Date(median_laydate$mid_date[2]) ~ paste(year, "late", sep = "_"),
#   year == "2020" & laying_date < as.Date(median_laydate$mid_date[2]) ~ paste(year, "early", sep = "_"),
#   year == "2021" & laying_date >= as.Date(median_laydate$mid_date[3]) ~ paste(year, "late", sep = "_"),
#   year == "2021" & laying_date < as.Date(median_laydate$mid_date[3]) ~ paste(year, "early", sep = "_"),
#   year == "2022" & laying_date >= as.Date(median_laydate$mid_date[4]) ~ paste(year, "late", sep = "_"),
#   year == "2022" & laying_date < as.Date(median_laydate$mid_date[4]) ~ paste(year, "early", sep = "_"))) %>%
#   dplyr::mutate(dplyr::across(where(is.character), factor)) %>%
#   dplyr::relocate(breeding_window, .after = year) %>%
#   dplyr::relocate(laying_date, .after = breeding_window) %>%
#   dplyr::relocate(flight_date, .after = hatching_date) %>%
#   dplyr::select(-incubation_date, -hatching_date) -> tits
# summary(tits)
# rm(loco, mrd, median_laydate)
#
#
#
# ##### Format TEMP data:
# # Short tits data version to work with
# tits %>% dplyr::select(id_nestbox, laying_date, flight_date) -> tt
#
#
# # Import of temp data
# temp <- readr::read_csv2(here::here("mydata", "temp_data_20192021.csv"), col_names = TRUE,
#                              col_types = readr::cols(time = readr::col_factor()))
# temp %>%
#   tidyr::separate(time, c('date', 'hour'), sep = " ") %>%
#   tidyr::separate(date, c('year', 'month', 'day'), sep = "-") %>%
#   dplyr::select(-s69) %>%
#   dplyr::mutate(dplyr::across(where(is.character), factor)) %>%
#   dplyr::filter(month == "12" | month == "01" | month == "02" | month == "03" |
#                   month == "04" | month == "05" | month == "06") -> temp
# summary(temp)
#
#
# temp %>% dplyr::group_by(year, month, day) %>%
#   dplyr::summarise(dplyr::across(where(is.numeric), .fns =
#                                    list(min_t = min,
#                                         max_t = max))) -> daily_t_range # Globally, this code computes
# # the daily temperature min and max. In details:
# #     - I grouped the hourly temperature values (readings) for each day of the month and each month of the year.
# #     - I then SUMMARISED the daily MIN and MAX temperature values ACROSS all NUMERIC columns while assigning
# #       suffixes to each temperature station for the MIN and MAX values, respectively (doubling the number of
# #       columns).
# # NOTE: however, I could not figure out how to disregard missing values so there are more NAs than expected (if
# # there was any NA hourly reading for a given day, the daily value will also be NA).
#
# # NOW, I need to recreate a date column and compute averages, sum DD etc.
#
# # I need to aggregate TEMP data as a function of the tits laying dates!!! How???
# # + search for how to handle time
# # series (and compute day-degrees, etc. ???)!!!!!!
# # Voir aussi https://www.r-bloggers.com/2014/02/using-dates-and-times-in-r/
