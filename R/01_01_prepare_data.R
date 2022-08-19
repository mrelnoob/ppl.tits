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






# ############### TEMP AGREGation (ONGOING package development)
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
# stringr str_detect
# degday dd_calc
#
# library(ppl.tits)
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
#   tidyr::unite(laying_date, c(year2, month2, day2), sep = "-", na.rm = TRUE) %>%
#   tidyr::separate(date, c('day3', 'month3', 'year3'), sep = "/") %>%
#   tidyr::unite(flight_date, c(year3, month3, day3), sep = "-") %>%
#   dplyr::mutate(laying_date = as.Date(x = laying_date, optional = TRUE),
#                 flight_date = as.Date(x = flight_date, optional = TRUE)) -> tits
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
# tits %>% dplyr::select(id_nestbox, laying_date, flight_date, temp_station_id) -> tt
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
#                   month == "04" | month == "05" | month == "06") %>%
#   tidyr::unite(date, c(year, month, day), sep = "-", remove = FALSE) %>%
#   dplyr::mutate(date = as.Date(date)) -> temp
# summary(temp)
#
#
# temp %>% dplyr::group_by(date) %>%
#   dplyr::summarise(dplyr::across(where(is.numeric), .fns =
#                                    list(min_t = min,
#                                         max_t = max))) -> daily_t_range
# # Globally, this code computes the daily temperature min and max. More precisely:
# #     - I grouped the hourly temperature values (readings) for each day of the month and each month of the year (=date).
# #     - I then SUMMARISED the daily MIN and MAX temperature values ACROSS all NUMERIC columns while assigning
# #       suffixes to each temperature station for the MIN and MAX values, respectively (doubling the number of
# #       columns).
# # NOTE: however, I could not figure out how to disregard missing values so there are more NAs than expected (if
# # there was any NA hourly reading for a given day, the daily value will also be NA).
# temp %>% dplyr::group_by(date) %>%
#   dplyr::summarise(dplyr::across(where(is.numeric), .fns =
#                                    list(min_t = min))) -> daily_t_min
# temp %>% dplyr::group_by(date) %>%
#   dplyr::summarise(dplyr::across(where(is.numeric), .fns =
#                                    list(max_t = max))) -> daily_t_max
#
#
#
# #### Computing averages, cumulative DD etc. (UNFINISHED)
# head(daily_t_range)
# degday::dd_calc(daily_min = daily_t_range$s01_min_t, daily_max = daily_t_range$s01_max_t, thresh_low = 0, thresh_up = 100,
#                 method = "sng_sine", cumulative = TRUE, no_neg = TRUE, interpolate_na = TRUE) %>% head() # Works!
#
# # daily_t_range %>% # My own way...
# #   dplyr::group_by(date) %>%
# #   dplyr::summarise(s01 = mean(c(s01_min_t, s01_max_t))) -> ooo
# # print(cumsum(ooo[,2]), n = 20) # Works too but it's not base 0°C!
#
# ### Function to compute daily mean for all stations:
# daily_t_mean <- daily_t_range[,1]
# dname <- "date"
#
# for(k in 1:70){
#   station_name <- colnames(temp[,6:ncol(temp)])
#   k_name <- station_name[k]
#
#   jjj <- cbind(daily_t_range[,1], daily_t_min[,1+k], daily_t_max[,1+k])
#   colnames(jjj) <- c("date", "min_t", "max_t")
#
#   jjj %>%
#     dplyr::group_by(date) %>%
#     dplyr::summarise(mean_t = mean(c(min_t, max_t))) -> kkk
#   daily_t_mean[,1+k] <- kkk[,2]
#
#   dname[1+k] <- paste(k_name, "mean_t", sep = "_")
#   colnames(daily_t_mean) <- dname
#   rm(jjj, kkk, k_name, station_name)
# }
# rm(dname, k)
#
#
#
#
#
#
#
# ### Function to compute (UNFINISHED)§§§§§
# cumdd_30d <- NULL
# i <- 111 # Toy i!
# station_name <- colnames(daily_t_mean)
# for(i in 1:nrow(tits)){
#
#   date_i <- tits[i, 4]
#   station_i <- as.character(as.matrix(tits[i, 16])) # Bloody R doesn't want to extract the factor's value directly!
#
#   # Daily temperature minimal values for the 30 days preceding the i-th laying date:
#   daily_t_min %>%
#     dplyr::filter(date < date_i) %>%
#     dplyr::filter(date > date_i-31) -> mw30_min
#
#   # Daily temperature maximal values for the 30 days preceding the i-th laying date:
#   daily_t_max %>%
#     dplyr::filter(date < date_i) %>%
#     dplyr::filter(date > date_i-31) -> mw30_max
#
#   # Extracting values from the right station:
#   nn <- which(stringr::str_detect(string = station_name, pattern = station_i))
#   ttt <- cbind(mw30_min[,nn], mw30_max[,nn])
#   colnames(ttt) <- c("min_t", "max_t")
#
#   # Computing cumDD and handling NAs:
#   if (sum(is.na(ttt$min_t)) >= 29) {
#     cumdd_30d[i] <- "NA"
#   } else if (sum(is.na(ttt$max_t)) >= 29) {
#     cumdd_30d[i] <- "NA"
#   } else if (all(is.na(ttt$min_t))) {
#     cumdd_30d[i] <- "NA"
#   } else if (all(is.na(ttt$max_t))) {
#     cumdd_30d[i] <- "NA"
#   } else {
#     degday::dd_calc(daily_min = ttt$min_t, daily_max = ttt$max_t, thresh_low = 0, thresh_up = 100,
#                     method = "sng_sine", cumulative = TRUE, no_neg = TRUE,
#                     interpolate_na = TRUE) -> cumdd # This function doesn't works if the temperature vectors
#     # are all NA's or if there is only one value that is NOT NA (because it cannot interpolate missing
#     # values in such a case).
#
#     cumdd_30d[i] <- cumdd[30]
#   }
# }
# tits$cumdd_30 <- as.numeric(cumdd_30d)
# summary(tits)
# ### Et ça marche!!! Plus qu'à finir les autres fonctions/variables!§§§§§§§§§§
#
#
#
#
# tits %>%
#   dplyr::filter(year == "2021") %>%
#   summary()
#
# s66_2021 <- daily_t_range %>%
#   dplyr::filter(date > "2021-02-01") %>%
#   dplyr::select(date, s66_min_t, s66_max_t)
#
# # I should create a function with a loop!
# #  --> Pour chaque ligne, j'extrais la date de ponte et la station d'appariement.
# #  --> Je filtre les relevés pour ne garder que les 30 derniers jours par rapport à date_ponte
# #  --> Puis je calcule la somme DD pour la station d'appariement via dd_calc()
# # Puis idem pour les autres variables de température.
# # Au cas où, je peux aussi déjà calculer les somme DD + autres variables mais pour les mois de mars de chaque année (en
# # fonction des réponses d'Yves et Bruno).
# # + Calculer sum_DD mais pour le mois de mars de chaque année uniquement (puis l'associer à chaque nichoir-année évidemment)!
# # + Quelle variable spatialement continue pour le modèle prédictif???
