# ---------------------------------------- #
##### Functions to prepare my datasets #####
# ---------------------------------------- #

# The functions of this R file are meant to prepare datasets: e.g. aggregate observations,
# delete or add variables, etc. But NOT to directly explore and clean data for analyses!

utils::globalVariables("where") # This is necessary for now as tidyselect::where is not
# an exported function!


### ______________________________________
#' Aggregate tits data by nestbox and year
#'
#' @description The `aggreg_by_nest` function aggregates the raw tits data to create a table
#' summarizing tits' breeding success for each nestbox each year where breeding occurred. As such,
#' it only keeps information on nestlings/juveniles and disregards the lines about adult tits.
#' @param myrawdata The raw tits data object (tibble).
#'
#' @return A tibble with 16 variables.
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' nestbox_repro <- aggreg_by_nest(myrawdata = rawtits) # This example cannot work because the
#' # proposed object does not actually exist!
#' }
aggreg_by_nest <- function(myrawdata){
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
#' `aggreg_by_nest` function, that is the tits nestling data aggregated by nestbox and year. The
#' input dataset should therefore be the \strong{raw dataset}, not an object created by the
#' aggregating function!
#' @param myrawdata The raw tits data object (tibble).
#'
#' @return A CSV table and its path.
#'
#' @export
#' @importFrom readr write_csv2
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#' export_nestling_aggreg(myrawdata = raw_tits)
#' }
export_nestling_aggreg <- function(myrawdata){
  aabb <- ppl.tits::aggreg_by_nest(myrawdata = myrawdata)
  readr::write_csv2(x = aabb, file = here::here("output", "tables", "tits_nestling_data.csv"))
  return(here::here("output", "tables", "tits_nestling_data.csv"))
}





### ____________________________________________________________________
#' Update and export the tits dataset with temperature related variables
#'
#' @description The function is the first of a series of functions meant to update and complete
#' the tits dataset with new variables. The `tdata_update_temp` function modifies the "aggregated
#' tits dataset" in several ways:
#' * First, it assigns to each observation (nestbox for a given year) the temperature station it is
#' paired with (giving the new column "\emph{temp_station_id}").
#' * Second, it reorganizes the date columns (deleting 2 and updating the others).
#' * Third, it creates a new `factor` variable called "\emph{breeding window}" that indicates whether
#' the reproduction event occurred at the beginning or at the end of the breeding season.
#' * Fourth, it computes five \strong{temperature related} variables. These variables are
#' \emph{cumdd_30} (the cumulative day-degrees for the 30 days prior to the laying date);
#' \emph{min_t_before} and \emph{min_t_between} (which are the minimum temperature recorded during the
#' 30 days prior to the laying date and between the laying and the flight date, respectively);
#' \emph{mean_winter_t} and \emph{sd_winter_t}) (which are the mean recorded temperature during the
#' four month of winter across 2019-2022 and its standard deviation, respectively).
#'
#' @param myboxtemp_data The nestbox-temperature stations pairing dataset (.csv).
#' @param mytits_data The tits nestling aggregated dataset (.csv). Cf.
#' \code{\link[ppl.tits:export_nestling_aggreg]{boxplot}}.
#'
#' @return A tibble with an improved version of the tits dataset.
#' @export
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_double
#' @importFrom here here
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr relocate
#' @importFrom stats median
#' @importFrom stats sd
#' @importFrom stringr str_detect
#' @importFrom degday dd_calc
#'
#' @examples
#' \dontrun{
#' mydata <- tdata_update_temp()
#' }
tdata_update_temp <- function(myboxtemp_data = here::here("mydata", "paired_boxtemp.csv"),
                              mytits_data = here::here("output", "tables", "tits_nestling_data.csv")){
  ##### Attribute ID to nest_years and improve formatting
  # _____________________________________________________

  ### Assigning temp_stations to each observation and improving dates format____#
  loco <- readr::read_csv2(myboxtemp_data, col_names = TRUE, na = "NA",
                           col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                   year = readr::col_factor(),
                                                   lcz1 = readr::col_factor(),
                                                   dist_lcz = readr::col_double(),
                                                   temp_id_lcz = readr::col_factor(),
                                                   dist_pp = readr::col_double(),
                                                   temp_id_pp = readr::col_factor(),
                                                   temp_id_final = readr::col_factor()))
  tits <- readr::read_csv2(mytits_data, col_names = TRUE,
                           col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                   year = readr::col_factor(),
                                                   date = readr::col_factor(),
                                                   laying_date = readr::col_factor(),
                                                   incubation_date = readr::col_factor(),
                                                   hatching_date = readr::col_factor(),
                                                   clutch_size = readr::col_integer(),
                                                   brood_size = readr::col_integer(),
                                                   fledgling_nb = readr::col_integer(),
                                                   success_manipulated = readr::col_factor(),
                                                   father_id = readr::col_factor(),
                                                   mother_id = readr::col_factor(),
                                                   species = readr::col_factor())) # NOTE: I have
  # to generate "tits" by reading the exported aggregated tits dataset in order for {targets} to be
  # able to follow modifications. Otherwise, I could just have used ppl.tits::aggreg_by_nest().
  tits$temp_station_id <- loco$temp_id_final

  tits %>%
    tidyr::separate(laying_date, c('day2', 'month2', 'year2'), sep = "/") %>%
    tidyr::unite(laying_date, c(year2, month2, day2), sep = "-", na.rm = TRUE) %>%
    tidyr::separate(date, c('day3', 'month3', 'year3'), sep = "/") %>%
    tidyr::unite(flight_date, c(year3, month3, day3), sep = "-") %>%
    dplyr::mutate(laying_date = as.Date(x = laying_date, optional = TRUE),
                  flight_date = as.Date(x = flight_date, optional = TRUE)) -> tits



  ### Creation of the "breeding_window" random factor___________________________#
  tits %>% dplyr::group_by(year) %>%
    dplyr::summarise(mid_date = stats::median(laying_date, na.rm = TRUE)) -> median_laydate

  # Imputation of the laying_date missing values (n=9)
  tits %>% dplyr::group_by(year) %>%
    dplyr::summarise(mean_repro_duration = mean(
      flight_date, na.rm = TRUE) - mean(laying_date, na.rm = TRUE)) -> mrd

  tits %>% dplyr::mutate(laying_date = dplyr::case_when(
    year == "2019" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[1]),
    year == "2019" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2020" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[2]),
    year == "2020" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2021" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[3]),
    year == "2021" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2022" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[4]),
    year == "2022" & is.na(laying_date) == FALSE ~ laying_date)) -> tits

  # Random factor generation
  tits %>% dplyr::mutate(breeding_window = dplyr::case_when(
    year == "2019" & laying_date >= as.Date(median_laydate$mid_date[1]) ~ paste(year, "late",
                                                                                sep = "_"),
    year == "2019" & laying_date < as.Date(median_laydate$mid_date[1]) ~ paste(year, "early",
                                                                               sep = "_"),
    year == "2020" & laying_date >= as.Date(median_laydate$mid_date[2]) ~ paste(year, "late",
                                                                                sep = "_"),
    year == "2020" & laying_date < as.Date(median_laydate$mid_date[2]) ~ paste(year, "early",
                                                                               sep = "_"),
    year == "2021" & laying_date >= as.Date(median_laydate$mid_date[3]) ~ paste(year, "late",
                                                                                sep = "_"),
    year == "2021" & laying_date < as.Date(median_laydate$mid_date[3]) ~ paste(year, "early",
                                                                               sep = "_"),
    year == "2022" & laying_date >= as.Date(median_laydate$mid_date[4]) ~ paste(year, "late",
                                                                                sep = "_"),
    year == "2022" & laying_date < as.Date(median_laydate$mid_date[4]) ~ paste(year, "early",
                                                                               sep = "_"))) %>%
    dplyr::mutate(dplyr::across(where(is.character), factor)) %>%
    dplyr::relocate(breeding_window, .after = year) %>%
    dplyr::relocate(laying_date, .after = breeding_window) %>%
    dplyr::relocate(flight_date, .after = hatching_date) %>%
    dplyr::select(-incubation_date, -hatching_date) -> tits





  ##### Format TEMPERATURE data and compute daily mean temperatures and range
  # _________________________________________________________________________

  ### Import of TEMP data and creation of the subtables___________________________#
  temp <- readr::read_csv2(here::here("mydata", "temp_data_20192021.csv"), col_names = TRUE,
                           col_types = readr::cols(time = readr::col_factor()))
  temp %>%
    tidyr::separate(time, c('date', 'hour'), sep = " ") %>%
    tidyr::separate(date, c('year', 'month', 'day'), sep = "-") %>%
    dplyr::select(-s69) %>% # Empty column
    dplyr::mutate(dplyr::across(where(is.character), factor)) %>%
    dplyr::filter(month == "12" | month == "01" | month == "02" | month == "03" |
                    month == "04" | month == "05" | month == "06") %>%
    tidyr::unite(date, c(year, month, day), sep = "-", remove = FALSE) %>%
    dplyr::mutate(date = as.Date(date)) -> temp

  temp %>% dplyr::group_by(date) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), .fns =
                                     list(min_t = min,
                                          max_t = max))) -> daily_t_range
  # Globally, this code computes the daily temperature min and max. More precisely:
  #     - I grouped the hourly temperature values (readings) for each day of the month and each
  #       month of the year (=date).
  #     - I then SUMMARISED the daily MIN and MAX temperature values ACROSS all NUMERIC columns
  #       while assigning
  #       suffixes to each temperature station for the MIN and MAX values, respectively (doubling
  #       the number of columns).
  # NOTE: however, I could not figure out how to disregard missing values so there are more NAs
  # than expected (if there was any NA hourly reading for a given day, the daily value will also be NA).
  temp %>% dplyr::group_by(date) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), .fns =
                                     list(min_t = min))) -> daily_t_min
  temp %>% dplyr::group_by(date) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), .fns =
                                     list(max_t = max))) -> daily_t_max

  # Function to compute daily mean for all stations:
  daily_t_mean <- daily_t_range[,1]
  station_name <- c("date", colnames(temp[,6:ncol(temp)]))

  dname <- "date"

  for(k in 1:70){

    k_name <- station_name[1+k]

    jjj <- cbind(daily_t_range[,1], daily_t_min[,1+k], daily_t_max[,1+k])
    colnames(jjj) <- c("date", "min_t", "max_t")

    jjj %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(mean_t = mean(c(min_t, max_t))) -> kkk
    daily_t_mean[,1+k] <- kkk[,2]

    dname[1+k] <- paste(k_name, "mean_t", sep = "_")
    colnames(daily_t_mean) <- dname
  }





  ##### Computing averages, cumulative day-degrees, minimum temperatures, etc.
  # __________________________________________________________________________

  # ______
  ### List of the variables that will be created___#
  cumdd_30d <- NULL
  min_t_before <- NULL
  min_t_between <- NULL
  mean_winter_t <- NULL
  sd_winter_t <- NULL # For simplicity, I'll reorganize them later.

  # Sub-table for MEAN_WINTER_T and SD_WINTER_T (mean winter temperature across 2019-2022):
  daily_t_mean %>%
    tidyr::separate(date, c('year', 'month', 'day'), sep = "-") %>%
    dplyr::filter(month == "01" | month == "02" | month == "03" |
                    month == "12") -> mean_t_winter
  # Note: I place this here because, contrarily to the other sub-tables, this one does
  # not require a "i" (it doesn't depend on a specific date) so it can be computed just once!


  # _______
  ### Heavy "for-loop" to compute each variable for each line (1...i...n)___#
  for(i in 1:nrow(tits)){

    # ____________
    # ____________
    date_i <- tits[i, 4]
    fdate_i <- tits[i, 5]
    year_i <- tits[i, 2]
    station_i <- as.character(as.matrix(tits[i, 16])) # Bloody R doesn't
    # want to extract the factor's value directly!
    nn <- which(stringr::str_detect(string = station_name, pattern = station_i))


    # ____________
    # ____________
    ### Sub-tables for CUMDD_30D and MIN_T_BEFORE (it's actually the same table)
    # Daily temperature minimal values for the 30 days preceding the i-th laying date:
    daily_t_min %>%
      dplyr::filter(date < date_i) %>%
      dplyr::filter(date > date_i-31) -> mw30_min

    # Daily temperature maximal values for the 30 days preceding the i-th laying date:
    daily_t_max %>%
      dplyr::filter(date < date_i) %>%
      dplyr::filter(date > date_i-31) -> mw30_max

    # ____________
    ### Sub-tables for MIN_T_BETWEEN
    # Daily temperature minimal values between the i-th laying and flight dates:
    daily_t_min %>%
      dplyr::filter(date > date_i) %>%
      dplyr::filter(date < fdate_i) -> mwbetween_min


    # ____________
    # ____________
    ### Extracting values from the right station
    ttt30 <- cbind(mw30_min[, nn], mw30_max[, nn])
    tttbet <- mwbetween_min[, nn]
    tttwinter <- mean_t_winter[, 2+nn]
    colnames(ttt30) <- c("mw30_min_t", "mw30_max_t")
    colnames(tttbet) <- "mwbet_min_t"
    colnames(tttwinter) <- "winter_mean_t"

    # _________
    # Computing CUMDD_30D and handling NAs:
    if (sum(is.na(ttt30$mw30_min_t)) >= 29) {
      cumdd_30d[i] <- "NA"
    } else if (sum(is.na(ttt30$mw30_max_t)) >= 29) {
      cumdd_30d[i] <- "NA"
    } else if (all(is.na(ttt30$mw30_min_t))) {
      cumdd_30d[i] <- "NA"
    } else if (all(is.na(ttt30$mw30_max_t))) {
      cumdd_30d[i] <- "NA"
    } else {
      degday::dd_calc(daily_min = ttt30$mw30_min_t, daily_max = ttt30$mw30_max_t, thresh_low = 0,
                      thresh_up = 100, method = "sng_sine", cumulative = TRUE, no_neg = TRUE,
                      interpolate_na = TRUE) -> cumdd # This function doesn't works if the
      # temperature vectors are all NA's or if there is only one value that is NOT NA (because it
      # cannot interpolate missing values in such a case).
      cumdd_30d[i] <- cumdd[30]
    }

    # _________
    # Computing MIN_T_BEFORE, MIN_T_BETWEEN and handling NAs:
    if (sum(is.na(ttt30$mw30_min_t)) >= 15) { # This time, I consider NA if half of the data are
      # missing because missing data imputation would probably be more reliable that inferring min_t
      # from just 1 or 2 values.
      min_t_before[i] <- "NA"
    } else if (all(is.na(ttt30$mw30_min_t))) {
      min_t_before[i] <- "NA"
    } else {
      min_t_before[i] <- min(ttt30$mw30_min_t)
    }

    if (sum(is.na(tttbet$mwbet_min_t)) >= 15) {
      min_t_between[i] <- "NA"
    } else if (all(is.na(tttbet$mwbet_min_t))) {
      min_t_between[i] <- "NA"
    } else {
      min_t_between[i] <- min(tttbet$mwbet_min_t)
    }

    # _________
    # Computing MEAN_WINTER_T, SD_WINTER_T and handling NAs:
    mean_winter_t[i] <- mean(tttwinter$winter_mean_t, na.rm = TRUE)
    sd_winter_t[i] <- stats::sd(tttwinter$winter_mean_t, na.rm = TRUE)

  }

  tits$cumdd_30 <- as.numeric(cumdd_30d)
  tits$min_t_before <- as.numeric(min_t_before)
  tits$min_t_between <- as.numeric(min_t_between)
  tits$mean_winter_t <- as.numeric(mean_winter_t)
  tits$sd_winter_t <- as.numeric(sd_winter_t)


  # To dismiss notes regarding "visible binding for global variables" during the CMD Check:
  laying_date <- year2 <- month2 <- day2 <- flight_date <- year3 <- month3 <- day3 <-
    year <- breeding_window <- hatching_date <- incubation_date <- time <- s69 <- month <-
    day <- min_t <- max_t <- NULL


  ##### To export the updated table
  # _______________________________
  return(tits)
  # _______________________________

}







# ###################### Join the datasets (tits and predictors)§§§§§§§§§§§§§§§§§§§§§§§§§
# targets tar_read
# readr read.csv2
# readr cols
# readr col_factor
# readr col_integer
# here here
# dplyr mutate
# dplyr across
# dplyr left_join
# dplyr relocate
# dplyr select
#
# library(ppl.tits)
#
#
# # Import
# tits <- targets::tar_read(titsdata_temp) # Comment l'importer autrement (sans refaire tourner le script)?????????????
# #  En exportant la table à l'issue de la fonction précédante? Ou pas, juste préciser dans l'aide de la fonction de
# # ne pas utiliser les fonctions du package telles quelles, mais de faire tar_make ???
#
# tpred <- readr::read_csv2(here::here("mydata", "tits_predictors.csv"), col_names = TRUE, na = "NA",
#                          col_types = readr::cols(id_nestbox = readr::col_factor(),
#                                                  lsource_vs150_m = readr::col_integer(),
#                                                  age_class = readr::col_factor(
#                                                    ordered = TRUE,
#                                                    levels = c("0", "1", "2"),
#                                                    include_na = TRUE),
#                                                  site = readr::col_factor(),
#                                                  strata_div = readr::col_factor(
#                                                    ordered = TRUE,
#                                                    levels = c("0", "1", "2", "3", "4"),
#                                                    include_na = TRUE))) # I don't know why,
# # but I cannot make readr understand that some variables are actually numeric! So I have
# # to add new lines:
# tpred %>%
#   dplyr::mutate(dplyr::across(where(is.character), as.numeric)) -> tpred
# summary(tpred)
#
#
# ntits <- dplyr::left_join(tits, tpred, by = "id_nestbox")
# ntits %>% dplyr::mutate(woody_area = vegetation_area - herbaceous_area,
#                         open_area = 70353 - c(vegetation_area + built_area)) %>% # 70353 is the
#   # total surface area of the 150m buffers computed by PostGIS!
#   dplyr::select(-temp_station_id) %>%
#   dplyr::relocate(site, .after = id_nestbox) %>%
#   dplyr::relocate(lsource_vs150_m, .after = lflux_10_iq) %>%
#   dplyr::relocate(lsource_vs150_sd, .after = lsource_vs150_m) %>%
#   dplyr::relocate(soft_manag_area, .after = herbaceous_area) %>%
#   dplyr::relocate(woody_area, .after = vegetation_area) %>%
#   dplyr::relocate(open_area, .before = built_area) %>%
#   dplyr::relocate(age_class, .after = trafic) %>%
#   dplyr::relocate(strata_div, .after = age_class) -> ntits
#
# summary(ntits)
#
#
#
#
# # + other IV (open space, woody area) --> 2ème UPDATE function!
# # + other IV (mothers_cond, fathers_cond) --> 3ème UPDATE function!
# # + synthesis IV (light poll, temp etc.) --> 4ème UPDATE function? Or exploration (with imputation, etc.)???
#
#

