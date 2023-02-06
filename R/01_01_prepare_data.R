# ---------------------------------------- #
##### Functions to prepare my datasets #####
# ---------------------------------------- #

# The functions of this R file are meant to prepare datasets: e.g. aggregate observations,
# delete or add variables, etc. But NOT to directly explore and clean data for analyses!

utils::globalVariables("where") # This is necessary for now as tidyselect::where is not
# an exported function!


### ____________________________________________________________________
#' Update and export the tits dataset with temperature related variables
#'
#' @description (THEORETICALLY MEANT FOR INTERNAL USE ONLY, use with caution!) \cr
#' The function is the \strong{first} of a series of functions meant to update and complete
#' the tits dataset with its \emph{independent variables} (i.e. predictors and covariates).
#' The `tdata_upD_temp` function modifies the \emph{aggregated tits dataset} in several ways:
#' * First, it assigns to each observation (nestbox for a given year) the temperature station it is
#' paired with (giving the new column "\emph{temp_station_id}").
#' * Second, it reorganizes the date columns (deleting 2 and updating the others).
#' * Third, it creates a new `factor` variable called "\emph{breeding window}" that indicates whether
#' the reproduction event occurred at the beginning or at the end of the breeding season.
#' * Fourth, it computes seven \strong{temperature related} variables. These variables are
#' \emph{cumdd_30} (the cumulative day-degrees for the 30 days prior to the laying date); \emph{cumdd_60}
#' (the cumulative day-degrees for the 60 days prior to the laying date); \emph{cumdd_between} (the
#' cumulative day-degrees for the period between the laying and the flight dates);
#' \emph{min_t_before} and \emph{min_t_between} (which are the minimum temperature recorded during the
#' 30 days prior to the laying date and between the laying and the flight dates, respectively);
#' \emph{mean_winter_t} and \emph{sd_winter_t}) (which are the mean recorded temperature during the
#' four month of winter across 2019-2022 and its standard deviation, respectively).
#'
#' @param myboxtemp_data The path to the nestbox-temperature stations pairing dataset (.csv).
#' @param mytits_data The path to the tits nestling aggregated dataset (.csv). Cf.
#' \code{\link[ppl.tits:export_nestling_aggreg]{export_nestling_aggreg}}.
#' @param mytemp_data The path to the temperature records dataset (.csv).
#'
#' @return A .csv file of the updated version of the tits dataset, and its path.
#' @export
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_double
#' @importFrom readr write_csv2
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
#' mydata <- tdata_upD_temp()
#' }
tdata_upD_temp <- function(myboxtemp_data = here::here("input_raw_data", "paired_boxtemp.csv"),
                           mytemp_data = here::here("input_raw_data", "temp_data_20192022.csv"),
                           mytits_data = here::here("output", "tables", "tits_nestling_data.csv")){
  ##### Attribute ID to nest_years and improve formatting
  # _____________________________________________________

  ### Setting the seed for random number generations____________________________#
  set.seed(93)

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

  # Correcting wrong dates (including :
  tits[which(tits$id_nestbox == "DIJ-102" & tits$year == "2021"), "laying_date"] <- as.Date("2021-04-04")
  tits[which(tits$id_nestbox == "DIJ-200" & tits$year == "2022"), "laying_date"] <- as.Date("2022-04-04")



  ### Creation of the "breeding_window" random factor___________________________#
  tits %>% dplyr::group_by(year) %>%
    dplyr::summarise(mid_date = stats::median(laying_date, na.rm = TRUE)) -> median_laydate # NOTE:
  # This object as well as the following (mrd) are not chronological! They're based on the order of
  # appearance of years in "tits": so the order is (currently) 2020, 2021, 2019 and 2022. If we
  # import data from additional years (that could change the order), I will have to carefully
  # check that my code works as expected! Because of this error, I call these objects
  # (median_laydate and mrd) not in non-chronological order in the next code lines!

  # Imputation of the laying_date missing values (n=9) ann correcting wrong "flight_date" for clutches
  # that failed:
  tits %>% dplyr::group_by(year) %>%
    dplyr::summarise(mean_repro_duration = mean(
      flight_date, na.rm = TRUE) - mean(laying_date, na.rm = TRUE)) -> mrd

  tits %>% dplyr::mutate(laying_date = dplyr::case_when(
    year == "2019" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[3]),
    year == "2019" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2020" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[1]),
    year == "2020" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2021" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[2]),
    year == "2021" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2022" & is.na(laying_date) == TRUE ~ as.Date(flight_date - mrd$mean_repro_duration[4]),
    year == "2022" & is.na(laying_date) == FALSE ~ laying_date)) -> tits

  tits %>% dplyr::mutate(
    flight_date = dplyr::case_when(
      brood_size != 0 ~ as.Date(flight_date),
      brood_size == 0 ~ as.Date(laying_date + 30))) -> tits

  # Random factor generation
  tits %>% dplyr::mutate(breeding_window = dplyr::case_when(
    year == "2019" & laying_date >= as.Date(median_laydate$mid_date[3]) ~ paste("late", year,
                                                                                sep = "_"),
    year == "2019" & laying_date < as.Date(median_laydate$mid_date[3]) ~ paste("early", year,
                                                                               sep = "_"),
    year == "2020" & laying_date >= as.Date(median_laydate$mid_date[1]) ~ paste("late", year,
                                                                                sep = "_"),
    year == "2020" & laying_date < as.Date(median_laydate$mid_date[1]) ~ paste("early", year,
                                                                               sep = "_"),
    year == "2021" & laying_date >= as.Date(median_laydate$mid_date[2]) ~ paste("late", year,
                                                                                sep = "_"),
    year == "2021" & laying_date < as.Date(median_laydate$mid_date[2]) ~ paste("early", year,
                                                                               sep = "_"),
    year == "2022" & laying_date >= as.Date(median_laydate$mid_date[4]) ~ paste("late", year,
                                                                                sep = "_"),
    year == "2022" & laying_date < as.Date(median_laydate$mid_date[4]) ~ paste("early", year,
                                                                               sep = "_"))) %>%
    dplyr::mutate(dplyr::across(where(is.character), factor)) %>%
    dplyr::relocate(breeding_window, .after = year) %>%
    dplyr::relocate(laying_date, .after = breeding_window) %>%
    dplyr::relocate(flight_date, .after = hatching_date) %>%
    dplyr::select(-incubation_date, -hatching_date) -> tits





  ##### Format TEMPERATURE data and compute daily mean temperatures and range
  # _________________________________________________________________________

  ### Import of TEMP data and creation of the subtables_________________________#
  temp <- readr::read_csv2(mytemp_data, col_names = TRUE,
                           col_types = readr::cols(time = readr::col_factor()))
  temp %>%
    tidyr::separate(time, c('date', 'hour'), sep = " ") %>%
    tidyr::separate(date, c('day', 'month', 'year'), sep = "/") %>%
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
  cumdd_60d <- NULL
  cumdd_between <- NULL
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
    date_i <- tits[i, 4] # Laying date.
    fdate_i <- tits[i, 5] # Fledging date.
    year_i <- tits[i, 2]
    station_i <- as.character(as.matrix(tits[i, 16])) # Temperature station ID. Note: bloody R
    # doesn't want to extract the factor's value directly!
    nn <- which(stringr::str_detect(string = station_name, pattern = station_i)) # To extract
    # the position (in the 'station_name' string) of the i-th temperature station.


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
    ### Sub-tables for CUMDD_60D
    # Daily temperature minimal values for the 60 days preceding the i-th laying date:
    daily_t_min %>%
      dplyr::filter(date < date_i) %>%
      dplyr::filter(date > date_i-61) -> mw60_min

    # Daily temperature maximal values for the 60 days preceding the i-th laying date:
    daily_t_max %>%
      dplyr::filter(date < date_i) %>%
      dplyr::filter(date > date_i-61) -> mw60_max

    # ____________
    ### Sub-tables for CUMDD_BETWEEN and MIN_T_BETWEEN (it's actually the same table)
    # Daily temperature minimal values between the i-th laying and flight dates:
    daily_t_min %>%
      dplyr::filter(date > date_i) %>%
      dplyr::filter(date < fdate_i) -> mwbetween_min

    # Daily temperature maximal values between the i-th laying and flight dates:
    daily_t_max %>%
      dplyr::filter(date > date_i) %>%
      dplyr::filter(date < fdate_i) -> mwbetween_max


    # ____________
    # ____________
    ### Extracting values from the right station
    ttt30 <- cbind(mw30_min[, nn], mw30_max[, nn])
    ttt60 <- cbind(mw60_min[, nn], mw60_max[, nn])
    tttbet <- cbind(mwbetween_min[, nn], mwbetween_max[, nn])
    tttwinter <- mean_t_winter[, 2+nn] # "2+" because this table has 2 more front columns.
    colnames(ttt30) <- c("mw30_min_t", "mw30_max_t")
    colnames(ttt60) <- c("mw60_min_t", "mw60_max_t")
    colnames(tttbet) <- c("mwbet_min_t", "mwbet_max_t")
    colnames(tttwinter) <- "winter_mean_t"

    # _________
    # Computing CUMDD_30D and handling NAs:
    if (sum(is.na(ttt30$mw30_min_t)) >= length(ttt30$mw30_min_t)*0.9) { # I consider "NA" if there
      # are more than 90% of observations missing because imputing missing values would be too random
      # otherwise.
      cumdd_30d[i] <- "NA"
    } else if (sum(is.na(ttt30$mw30_max_t)) >= length(ttt30$mw30_max_t)*0.9) {
      cumdd_30d[i] <- "NA"
    } else {
      degday::dd_calc(daily_min = ttt30$mw30_min_t, daily_max = ttt30$mw30_max_t, thresh_low = 0,
                      thresh_up = 100, method = "sng_sine", cumulative = TRUE, no_neg = TRUE,
                      interpolate_na = TRUE) -> cumdd # This function doesn't work if the
      # temperature vectors are all NA's or if there is only one value that is NOT NA (because it
      # cannot interpolate missing values in such a case).
      cumdd_30d[i] <- cumdd[30]
    }

    # _________
    # Computing CUMDD_60D and handling NAs:
    if (sum(is.na(ttt60$mw60_min_t)) >= length(ttt60$mw60_min_t)*0.9) {
      cumdd_60d[i] <- "NA"
    } else if (sum(is.na(ttt60$mw60_max_t)) >= length(ttt60$mw60_max_t)*0.9) {
      cumdd_60d[i] <- "NA"
    } else {
      degday::dd_calc(daily_min = ttt60$mw60_min_t, daily_max = ttt60$mw60_max_t, thresh_low = 0,
                      thresh_up = 100, method = "sng_sine", cumulative = TRUE, no_neg = TRUE,
                      interpolate_na = TRUE) -> cumdd # This function doesn't work if the
      # temperature vectors are all NA's or if there is only one value that is NOT NA (because it
      # cannot interpolate missing values in such a case).
      cumdd_60d[i] <- cumdd[60]
    }

    # _________
    # Computing CUMDD_BETWEEN and handling NAs:
    if (sum(is.na(tttbet$mwbet_min_t)) >= length(tttbet$mwbet_min_t)*0.9) {
      cumdd_between[i] <- "NA"
    } else if (sum(is.na(tttbet$mwbet_max_t)) >= length(tttbet$mwbet_max_t)*0.9) {
      cumdd_between[i] <- "NA"
    } else {
      degday::dd_calc(daily_min = tttbet$mwbet_min_t, daily_max = tttbet$mwbet_max_t, thresh_low = 0,
                      thresh_up = 100, method = "sng_sine", cumulative = TRUE, no_neg = TRUE,
                      interpolate_na = TRUE) -> cumdd # This function doesn't work if the
      # temperature vectors are all NA's or if there is only one value that is NOT NA (because it
      # cannot interpolate missing values in such a case).
      cumdd_between[i] <- cumdd[length(cumdd)]
    }

    # _________
    # Computing MIN_T_BEFORE, MIN_T_BETWEEN and handling NAs:
    if (sum(is.na(ttt30$mw30_min_t)) >= length(ttt30$mw30_min_t)*0.67) { # This time, I consider NA
      # if more than 2/3 of observations are missing because later missing data imputation would probably
      # be more reliable if the proportion of missing "min_t" observations is not too high!
      min_t_before[i] <- "NA"
    } else if (all(is.na(ttt30$mw30_min_t))) {
      min_t_before[i] <- "NA"
    } else {
      min_t_before[i] <- min(ttt30$mw30_min_t, na.rm = TRUE)
    }

    if (sum(is.na(tttbet$mwbet_min_t)) >= length(tttbet$mwbet_min_t)*0.67) {
      min_t_between[i] <- "NA"
    } else if (all(is.na(tttbet$mwbet_min_t))) {
      min_t_between[i] <- "NA"
    } else {
      min_t_between[i] <- min(tttbet$mwbet_min_t, na.rm = TRUE)
    }

    # _________
    # Computing MEAN_WINTER_T, SD_WINTER_T and handling NAs:
    mean_winter_t[i] <- mean(tttwinter$winter_mean_t, na.rm = TRUE)
    sd_winter_t[i] <- stats::sd(tttwinter$winter_mean_t, na.rm = TRUE)

  }

  tits$cumdd_30 <- as.numeric(cumdd_30d)
  tits$cumdd_60 <- as.numeric(cumdd_60d)
  tits$cumdd_between <- as.numeric(cumdd_between)
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
  readr::write_csv2(x = tits, file = here::here("output", "tables", "ndata_temp.csv"))
  return(here::here("output", "tables", "ndata_temp.csv"))
  # _______________________________

}





### ________________________________________________________________________________
#' Update and export the tits dataset with all its "raw" independent variables (IVs)
#'
#' @description (THEORETICALLY MEANT FOR INTERNAL USE ONLY, use with caution!) \cr
#' The function is the \strong{second} of a series of functions meant to update and
#' complete the tits dataset with its independent variables (i.e. predictors and covariates).
#' The `tdata_upD_rawiv` function loads and modifies the dataset generated by the previous
#' update function (\code{\link[ppl.tits:tdata_upD_temp]{tdata_upD_temp}}) in several ways:
#' * First, it performs a left join between both input datasets (i.e. `my_tdata` and
#' `my_iv_data` - see below).
#' * Second, it computes two new variables: namely \emph{woody_area} and \emph{open_area} from
#' already existing columns.
#' * Third, it reorganises the dataset for improved clarity.
#' When it's done processing, the function exports the updated dataset as a new .csv file.
#'
#' @param my_tdata The path to the dataset generated
#' by \code{\link[ppl.tits:tdata_upD_temp]{tdata_upD_temp}}. Note that this argument is not
#' necessarily required but is strongly advised.
#' Therefore, if you want to reproduce this work, you should not call this function unless you
#' have previously built the {targets} pipeline described in _targets.R (e.g. by using
#' targets::tar_make())! If you DO NOT KNOW what I'm talking about, then you should
#' read the README file (https://github.com/mrelnoob/ppl.tits) and PROBABLY NOT modify the
#' function's arguments (see examples section).
#' @param my_iv_data The path to the dataset (.csv) generated by PostGIS (cf. ppl_log.docx***) and
#' containing the "raw" independent variables (IV = predictors and covariates) meant to describe the local
#' environment around each nestbox (= zonal statistics). It is called "raw" as opposed to the
#' more "advanced" IV that will be computed by the next functions to generate the "clean" and "complete"
#' tits dataset (`ntits_clean` and `ntits_complete`, respectively).
#'
#' @return A list containing 1) the updated dataset (a tibble accessible with
#' `tdata_upD_rawiv()$dataset`), and 2) the path to the exported .csv file of the updated
#' dataset (accessible with `tdata_upD_rawiv()$path`).
#' @export
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_integer
#' @importFrom readr write_csv2
#' @importFrom here here
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr left_join
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' mydata <- tdata_upD_rawiv()$dataset
#' path_to_csv <- tdata_upD_rawiv()$path
#' }
tdata_upD_rawiv <- function(my_tdata = here::here("output", "tables", "ndata_temp.csv"),
                               my_iv_data = here::here("input_raw_data", "tits_predictors.csv")){

  ##### Import data files
  # _____________________
  tits <- readr::read_csv2(my_tdata, col_names = TRUE,
                           col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                   year = readr::col_factor(),
                                                   breeding_window = readr::col_factor(),
                                                   laying_date = readr::col_date(),
                                                   flight_date = readr::col_date(),
                                                   clutch_size = readr::col_integer(),
                                                   brood_size = readr::col_integer(),
                                                   fledgling_nb = readr::col_integer(),
                                                   success_manipulated = readr::col_factor(),
                                                   father_id = readr::col_factor(),
                                                   mother_id = readr::col_factor(),
                                                   species = readr::col_factor())) # NOTE: I have
  # to generate "tits" by reading the exported "ndata_temp" dataset in order for {targets} to be
  # able to follow modifications. Otherwise, I could just have used ppl.tits::tdata_upD_temp().


  tpred <- readr::read_csv2(my_iv_data, col_names = TRUE, na = "NA",
                            col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                    lsource_vs150_m = readr::col_integer(),
                                                    age_class = readr::col_factor(
                                                      ordered = TRUE,
                                                      levels = c("0", "1", "2"),
                                                      include_na = TRUE),
                                                    site = readr::col_factor(),
                                                    strata_div = readr::col_factor(
                                                      ordered = TRUE,
                                                      levels = c("0", "1", "2", "3", "4"),
                                                      include_na = TRUE)))





  ##### Actual data join, new predictors computation and data reorganisation
  # ________________________________________________________________________
  ntits <- dplyr::left_join(tits, tpred, by = "id_nestbox")
  ntits %>% dplyr::mutate(woody_area = vegetation_area - herbaceous_area,
                          open_area = dplyr::case_when(
                            dist == 50 ~ 7850 - c(vegetation_area + built_area + water_area),
                            dist == 100 ~ 31400 - c(vegetation_area + built_area + water_area),
                            dist == 150 ~ 70650 - c(vegetation_area + built_area + water_area),
                            dist == 200 ~ 125600 - c(vegetation_area + built_area + water_area))) %>% # The
                            # values are the total surface area of the buffers computed by PostGIS as a
                            # function of their radius!
    dplyr::select(-temp_station_id) %>%
    dplyr::relocate(site, .after = id_nestbox) %>%
    dplyr::relocate(dist, .after = year) %>%
    dplyr::relocate(lsource_vs150_m, .after = lflux_10_iq) %>%
    dplyr::relocate(lsource_vs150_iq, .after = lsource_vs150_m) %>%
    dplyr::relocate(soft_manag_area, .after = herbaceous_area) %>%
    dplyr::relocate(built_area, .after = soft_manag_area) %>%
    dplyr::relocate(build_volume, .after = built_area) %>%
    dplyr::relocate(build_sd, .after = build_volume) %>%
    dplyr::relocate(woody_area, .before = vegetation_area) %>%
    dplyr::relocate(open_area, .after = build_sd) %>%
    dplyr::relocate(water_area, .after = open_area) %>%
    dplyr::relocate(age_class, .after = trafic) %>%
    dplyr::relocate(strata_div, .after = age_class) -> ntits
  ntits[which(ntits$id_nestbox == "DIJ-188B" & ntits$year == "2022"), "strata_div"] <- "4" # I obviously forgot
  # to assign a strata_div value for this nestbox, so here it is (NOTE: as I assign it here, it
  # means I did not update it in the GIS layers nor in the database)!

  # To dismiss notes regarding "visible binding for global variables" during the CMD Check:
  id_nestbox <- year <- breeding_window <- laying_date <- flight_date <- clutch_size <- brood_size <-
    fledgling_nb <- success_manipulated <- father_id <- mother_id <- species <-
    vegetation_area <- herbaceous_area <- built_area <- temp_station_id <- site <-
    lsource_vs150_m <- lflux_10_iq <- lsource_vs150_iq <- soft_manag_area <- woody_area <-
    open_area <- age_class <- trafic <- strata_div <- water_area <- dist <- build_volume <- build_sd <- NULL



  ##### To export the updated table
  # _______________________________
  readr::write_csv2(x = ntits, file = here::here("output", "tables", "ndata_rawiv.csv"))

  output <- list(dataset = ntits,
                 path = here::here("output", "tables", "ndata_rawiv.csv"))
  return(output)
  # _______________________________
}






### _________________________________________________________________________________
#' Update and export the tits dataset completed with the parental condition variables
#'
#' @description (THEORETICALLY MEANT FOR INTERNAL USE ONLY, use with caution!) \cr
#' The function is the \strong{third} of a series of functions meant to update and
#' complete the tits dataset with its independent variables (i.e. predictors and covariates).
#' The `tdata_upD_parcond` function loads and modifies the dataset generated by the previous
#' update function (\code{\link[ppl.tits:tdata_upD_rawiv]{tdata_upD_rawiv}}) by computing
#' the two proxy variables meant to represent the tits parental condition: that is \emph{father_cond}
#' and \emph{mother_cond}. NOTE, however, that only \emph{Parus major} (Great tits) will have values
#' for these variables as our adult sample size for \emph{Cyanistes caeruleus} (Blue tits) is too small
#' to allow proper missing value imputations and approximation of the parental condition.
#' When it's done processing, the function exports the updated dataset as a new .csv file.
#' @param myrawdata The path to the raw tits data object (`raw_tits_data.rda`). Cf.
#' \code{\link[ppl.tits:import_raw_tits_data]{import_raw_tits_data}}.
#' @param my_tdata The path to the dataset generated
#' by \code{\link[ppl.tits:tdata_upD_rawiv]{tdata_upD_rawiv}}. Note that this argument is not
#' necessarily required but is strongly advised.
#' Therefore, if you want to reproduce this work, you should not call this function unless you
#' have previously built the {targets} pipeline described in _targets.R (e.g. by using
#' targets::tar_make())! If you DO NOT KNOW what I'm talking about, then you should
#' read the README file (https://github.com/mrelnoob/ppl.tits) and PROBABLY NOT modify the
#' function's arguments (see examples section).
#'
#' @return A list containing 1) the updated dataset (a tibble accessible with
#' `tdata_upD_parcond()$dataset`), and 2) the path to the exported .csv file of the updated
#' dataset (accessible with `tdata_upD_parcond()$path`).
#' @export
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_integer
#' @importFrom readr col_date
#' @importFrom readr write_csv2
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr relocate
#' @importFrom dplyr filter
#' @importFrom dplyr across
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr first
#' @importFrom dplyr left_join
#' @importFrom missMDA imputePCA
#' @importFrom FactoMineR PCA
#'
#' @examples
#' \dontrun{
#' mydata <- tdata_upD_parcond(myrawdata = raw_tits, my_tdata = tdata_rawiv)$dataset
#' path_to_csv <- tdata_upD_parcond(myrawdata = raw_tits, my_tdata = tdata_rawiv)$path
#' # NOTE: this code won't work unless the {targets} pipeline has been built first!
#' }
tdata_upD_parcond <- function(myrawdata = here::here("data", "raw_tits_data.rda"),
                              my_tdata = here::here("output", "tables", "ndata_rawiv.csv")){

  ##### Data preparation
  # ____________________

  ### Setting the seed for random number generations____________________________#
  set.seed(43)

  ### Import data files_________________________________________________________#
  rtits_original <- ppl.tits::read_from_path(mypath = myrawdata)
  ntits_original <- readr::read_csv2(my_tdata, col_names = TRUE,
                           col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                   site = readr::col_factor(),
                                                   year = readr::col_factor(),
                                                   breeding_window = readr::col_factor(),
                                                   laying_date = readr::col_date(),
                                                   flight_date = readr::col_date(),
                                                   clutch_size = readr::col_integer(),
                                                   brood_size = readr::col_integer(),
                                                   fledgling_nb = readr::col_integer(),
                                                   success_manipulated = readr::col_factor(),
                                                   father_id = readr::col_factor(),
                                                   mother_id = readr::col_factor(),
                                                   species = readr::col_factor(),
                                                   age_class = readr::col_factor(
                                                     ordered = TRUE,
                                                     levels = c("0", "1", "2"),
                                                     include_na = TRUE
                                                   ),
                                                   strata_div = readr::col_factor(
                                                     ordered = TRUE,
                                                     levels = c("0", "1", "2", "3", "4"),
                                                     include_na = TRUE))) # NOTE: I have
  # to generate "ntits_original" by reading the exported "ndata_rawiv" dataset in order for {targets}
  # to be able to follow modifications. Otherwise, I could just have used ppl.tits::tdata_upD_rawiv().

  # Creating a new ID for adults based on their id_ring:
  rtits_original %>% dplyr::select(-date, -laying_date, -incubation_date, -success, -success_manipulated,
                                   -hatching_date, -clutch_size, -brood_size, -fledgling_nb,
                                   -nestling_mass, -nestling_tarsus_l, -nestling_wing_l) -> rtits
  # NOTE: some rows share the same id_ring. For some it is normal (it's adult
  # birds that reproduced several times) while it must be mistakes for others (e.g. "8877008"
  # or "V010500").



  ### Creation of sub-datasets____________________________________________________#
  # Tits parents light working datasets (and subsetting per species and sex):
  rtits %>% dplyr::filter(age != "nestling") -> atits

  atits %>% dplyr::filter(species == "PM") %>%
    dplyr::group_by(id_ring) %>%
    dplyr::summarise(sex = dplyr::first(sex),
                     age = dplyr::first(age),
                     adult_mass = mean(adult_mass),
                     adult_tarsus_l = mean(adult_tarsus_l),
                     adult_wing_l = mean(adult_wing_l)) %>%
    dplyr::mutate(
      age = dplyr::case_when(
        as.character(age) == "one" ~ 1,
        as.character(age) == "more_than_one" ~ 2,
        as.character(age) == "two" ~ 2,
        as.character(age) == "more_than_two" ~ 3)) -> apm

  apm %>% dplyr::filter(sex == "female") -> apm_f
  apm %>% dplyr::filter(sex == "male") -> apm_m
  # NOTE: I do not create a dataset for CC as there are only 12 adults (7 females, 4 males, 1 unknown),
  # too low a number for approximating the parental condition of nestlings and impute missing values.
  # Consequently, CC nestlings (blue tits) won't be modelled using this variable.





  ##### Creating parental condition proxies for tits nestlings (only for PM)
  # ________________________________________________________________________

  ### Synthesizing the morphometric variables of the parents______________________#
  # Normed-PCA for the males (known fathers):
  imput_apm_m <- missMDA::imputePCA(apm_m[,4:6], ncp = 2) # Missing values imputation (n = 2).
  res.pca <- FactoMineR::PCA(X = imput_apm_m$completeObs, scale.unit = TRUE, graph = FALSE)
  # factoextra::fviz_pca_var(res.pca, col.var = "contrib",
  # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # To
  # visualise the correlation circle for this PCA, with variables coloration according to their
  # contribution to the first 2 principal components.
  # As the first axis (PC) of my PCA satisfactorily synthesizes a fair amount of variance of my 3
  # variables (54.9%), I will use my observations coordinates on this axis as a synthetic variable:
  apm_m$father_cond <- res.pca$ind$coord[,1]
  # High values indicate large tits while smaller values represent smaller tits.

  # Normed-PCA for the females (known mothers):
  imput_apm_f <- missMDA::imputePCA(apm_f[,4:6], ncp = 2) # Missing values imputation (n = 19).
  res.pca <- FactoMineR::PCA(X = imput_apm_f$completeObs, scale.unit = TRUE, graph = FALSE)
  # For visualisation, please reuse above code (cf. males section).
  # As the first axis (PC) of my PCA satisfactorily synthesizes a fair amount of variance of my 3
  # variables (52.6%), I will use my observations coordinates on this axis as a synthetic variable:
  apm_f$mother_cond <- res.pca$ind$coord[,1]



  ### Joining newly created variables with the nestling_dataset___________________#
  apm_f %>% dplyr::rename(mother_id = id_ring) %>%
    dplyr::select(mother_id, mother_cond) -> apm_f
  ntits <- dplyr::left_join(ntits_original, apm_f, by = "mother_id")
  # Note that CC females are lacking.

  apm_m %>% dplyr::rename(father_id = id_ring) %>%
    dplyr::select(father_id, father_cond) -> apm_m
  ntits <- dplyr::left_join(ntits, apm_m, by = "father_id")
  # Note that CC males are lacking.

  # Initially, I wanted to impute missing values for these two variables here, but upon reflection,
  # I figured it would be better if I do that in my exploration phase along with other exploratory
  # steps (e.g. other missing data imputations, checks for outliers, etc.).
  # So I only needs to rearrange my columns now:
  ntits %>%
    dplyr::relocate(father_cond, .after = father_id) %>%
    dplyr::relocate(mother_cond, .after = mother_id) -> xxx

  # To dismiss notes regarding "visible binding for global variables" during the CMD Check:
  adult_mass <- adult_tarsus_l <- adult_wing_l <- age <- brood_size <- clutch_size <-
  father_cond <- father_id <- fledgling_nb <- hatching_date <- id_ring <-
  incubation_date <- laying_date <- mother_cond <- mother_id <-
  nestling_mass <- nestling_tarsus_l <- nestling_wing_l <- sex <- species <- success <-
  success_manipulated <- NULL



  ##### To export the updated table
  # _______________________________
  readr::write_csv2(x = xxx, file = here::here("output", "tables", "ndata_parcond.csv"))

  output <- list(dataset = xxx,
                 path = here::here("output", "tables", "ndata_parcond.csv"))
  return(output)
  # _______________________________
}





### _______________________________________________________________________
#' Update and export the tits dataset completed with imputed missing values
#'
#' @description (THEORETICALLY MEANT FOR INTERNAL USE ONLY, use with caution!) \cr
#' The function is the \strong{fourth} and last of a series of functions meant to
#' update and complete the tits dataset with its independent variables (IV - i.e. predictors and
#' covariates).
#' The `tdata_upD_final` function loads and modifies the dataset generated by the previous
#' update function (\code{\link[ppl.tits:tdata_upD_parcond]{tdata_upD_parcond}}) by correcting a few
#' last errors in the data and by imputing missing values using \strong{Random Forest} regressions. For
#' consistency, as the global dataset contains IV extracted from four sizes of buffer around nestboxes,
#' missing data imputations have been performed on each sub-dataset (e.g. 100m buffer-based data,
#' 150m buffer-based data, etc.), independently. \cr
#' For more details regarding the imputation process and associated errors, please refer to the
#' \emph{Exploratory data analyses and preparation report}.
#' When it's done processing, the function exports the updated dataset
#' (`ndata_clean`) as a new .csv file.
#'
#' @param my_tdata The path to the dataset generated
#' by \code{\link[ppl.tits:tdata_upD_parcond]{tdata_upD_parcond}}. Note that this argument is not
#' necessarily required but is strongly advised.
#' Therefore, if you want to reproduce this work, you should not call this function unless you
#' have previously built the {targets} pipeline described in _targets.R (e.g. by using
#' targets::tar_make())! If you DO NOT KNOW what I'm talking about, then you should
#' read the README file (https://github.com/mrelnoob/ppl.tits) and PROBABLY NOT modify the
#' function's arguments (see examples section).
#'
#' @return A list containing five elements: 1) the updated "clean" dataset (a tibble accessible with
#' `tdata_upD_final()$clean_dataset`); 2) the dataset before imputations but containing all data
#' corrections applied by the function (a tibble accessible with
#' `tdata_upD_final()$priorimp_dataset`); 3 and 4) tables showing the out-of-bag imputation error
#' for both species and for each of the four "buffer radius" (accessible with `tdata_upD_final()$impute_error`
#' followed by the desired species code (e.g. "_pm"; cf. examples)); and 5) the path to the exported
#' .csv file of the updated "clean" dataset (`ndata_clean.csv`; accessible with `tdata_upD_final()$path`).
#'
#' @export
#' @importFrom here here
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_integer
#' @importFrom readr col_date
#' @importFrom readr write_csv2
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr across
#' @importFrom assertthat is.date
#' @importFrom missForest missForest
#'
#' @examples
#' \dontrun{
#' # To obtain the data and imputation error regarding the 50m buffer data:
#' mydata <- tdata_upD_final()$clean_dataset
#' mydata %>% dplyr::filter(dist == 50) -> mydata_50
#' mydata <- tdata_upD_final()$priorimp_dataset # May be useful to compare the results of missing
#' # data imputation.
#' myerror_pm <- tdata_upD_final()$impute_error_pm
#' myerror_cc <- tdata_upD_final()$impute_error_cc
#' path_to_csv <- tdata_upD_final()$path
#' }
tdata_upD_final <- function(my_tdata = here::here("output", "tables", "ndata_parcond.csv")){

  ##### Data preparation
  # ____________________
  tits <- readr::read_csv2(my_tdata, col_names = TRUE,
                           col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                   site = readr::col_factor(),
                                                   year = readr::col_factor(),
                                                   breeding_window = readr::col_factor(),
                                                   laying_date = readr::col_date(),
                                                   flight_date = readr::col_date(),
                                                   clutch_size = readr::col_integer(),
                                                   brood_size = readr::col_integer(),
                                                   fledgling_nb = readr::col_integer(),
                                                   success_manipulated = readr::col_factor(),
                                                   father_id = readr::col_factor(),
                                                   mother_id = readr::col_factor(),
                                                   species = readr::col_factor(),
                                                   age_class = readr::col_factor(
                                                     ordered = TRUE,
                                                     levels = c("0", "1", "2"),
                                                     include_na = TRUE
                                                   ),
                                                   strata_div = readr::col_factor(
                                                     ordered = TRUE,
                                                     levels = c("0", "1", "2", "3", "4"),
                                                     include_na = TRUE))) # NOTE: I have
  # to generate "tits" by reading the exported "ndata_parcond" dataset in order for {targets}
  # to be able to follow modifications.
  # Otherwise, I could just have used ppl.tits::tdata_upD_parcond()$dataset



  ### Last data corrections_______________________________________________________#
  # Convert "NA" into real NAs:
  tits %>% dplyr::mutate(
    father_id = gsub(x = father_id, pattern = "NA", replacement = NA),
    mother_id = gsub(x = mother_id, pattern = "NA", replacement = NA)) %>%
    dplyr::mutate(
      father_id = as.factor(father_id),
      mother_id = as.factor(mother_id)) -> tits # Actually, it now turns out that I don't need this
  # code chunk as my NAs are not coded as "NA" anymore, but I keep this code chunk for posterity, in
  # case of future need!

  # Delete invalid observations:
  tits %>% dplyr::filter((id_nestbox != "DIJ-244" | year != "2022")) %>% # Here, as my command is the
    # inverse of a selection (it's the equivalent of using: -(my_conditions); except that 'filter'
    # does not allow the use of "-"), I have to invert my operator as well, hence the use of "|"
    # instead of "&". Similarly, if I wanted to do that in base R (with
    # something like tits[cond1 == XX...], I should similarly use the function "which()" and not the
    # minus sign alone, otherwise it will delete the first line, not the one I want to delete):
    # e.g. tits[-which(tits$id_nestbox == "DIJ-244" & tits$year == "2022"),]. It's stupid I know, but
    # trust me, I lost hours to understand that!
    # For this line, the nestbox was destroyed!
    dplyr::filter((id_nestbox != "DIJ-023B" | year != "2022")) %>% # Brood predated.
    dplyr::filter((id_nestbox != "DIJ-073" | year != "2022")) %>% # Nestbox destroyed.
    dplyr::filter((id_nestbox != "DIJ-246" | year != "2022")) %>% # Brood predated.
    dplyr::filter((id_nestbox != "DIJ-338" | year != "2022")) %>% # Brood predated.
    dplyr::filter(success_manipulated == "0") %>%
    dplyr::select(-success_manipulated)-> tits

  # Reorder variables:
  tits %>% dplyr::relocate(coord_x, .after = site) %>%
    dplyr::relocate(coord_y, .after = coord_x) %>%
    dplyr::relocate(species, .after = dist) %>%
    dplyr::relocate(mass, .after = fledgling_nb) %>%
    dplyr::relocate(tarsus_length, .after = mass) %>%
    dplyr::relocate(wing_length, .after = tarsus_length) -> tits





  ##### Missing data imputation
  # ___________________________

  # First, I have to divide the dataset into 4 sub-datasets according to their "dist" value (i.e. 50,
  # 100, 150, or 200).
  sub_datasets <- list()
  sub_errtab_pm <- list()
  sub_errtab_cc <- list()

  for (distance in c(50,100,150,200)) {

    if (distance == 50) {
      i <- 1
    }else if (distance == 100){
      i <- 2
    }else if (distance == 150){
      i <- 3
    }else if (distance == 200){
      i <- 4}
    tits %>% dplyr::filter(dist == distance) -> tits_sub

    ### For PM (Parus major)________________________________________________________#
    # Data preparation:
    tits_sub %>% dplyr::filter(species == "PM") -> pm

    pm %>% dplyr::mutate(dplyr::across(where(assertthat::is.date), factor)) %>%
      dplyr::select(-id_nestbox, -site, -species, -laying_date, -flight_date, -father_id, -mother_id,
                    -mass, -tarsus_length, -wing_length) %>%
      as.data.frame() -> pm_mis # missForest only accepts data.frames or matrices (not tibbles).
    # Variables must also be only numeric or factors (no character, date, etc.)!
    # NOTE: I also delete the morphometric variables because they are meant to be response variables
    # (Y) but are not necessary MAR or MCAR (missing completely at random), so they should not be
    # imputed!

    # Actual data imputation using Random Forests:
    set.seed(85)
    imput <- missForest::missForest(xmis = pm_mis, maxiter = 300, ntree = 300, variablewise = TRUE)
    pm_imp <- imput$ximp

    # To create a summary table for the OOB errors (for each imputed variable):
    pm_imp_error <- data.frame(cbind(
      sapply(pm_mis, function(y) sum(length(which(is.na(y))))), # Number of imputed values (i.e. NAs).
      sqrt(imput$OOBerror)), # When 'variablewise' is set to TRUE, missForest() returns MSE (Mean
      # Squared Error) values instead of NRMSE (Normalized Root Mean Square Error). Therefore, I use
      # the square root of the out-of-bag (OOB) values to convert MSE into RMSE.
      row.names = colnames(pm_mis)) # To get the name of the variables
    pm_imp_error %>% dplyr::rename(Nb_imputed_values = 'X1', Oob_RMSE = 'X2') %>%
      dplyr::filter(Nb_imputed_values > 0) -> sub_errtab_pm[[i]]

    pm_new <- pm
    pm_new$brood_size <- pm_imp$brood_size
    pm_new$father_cond <- pm_imp$father_cond
    pm_new$mother_cond <- pm_imp$mother_cond
    pm_new$cumdd_30 <- pm_imp$cumdd_30
    pm_new$cumdd_60 <- pm_imp$cumdd_60
    pm_new$cumdd_between <- pm_imp$cumdd_between
    pm_new$min_t_before <- pm_imp$min_t_before
    pm_new$min_t_between <- pm_imp$min_t_between





    ### For CC (Cyanistes caeruleus)________________________________________________#
    # Data preparation:
    tits_sub %>% dplyr::filter(species == "CC") -> cc

    cc %>% dplyr::mutate(dplyr::across(where(assertthat::is.date), factor)) %>%
      dplyr::select(-id_nestbox, -site, -species, -laying_date, -flight_date, -father_id, -father_cond,
                    -mother_id, -mother_cond, -mass, -tarsus_length, -wing_length) %>%
      as.data.frame() -> cc_mis

    # Actual data imputation using Random Forests:
    set.seed(24)
    imput <- missForest::missForest(xmis = cc_mis, maxiter = 300, ntree = 300, variablewise = TRUE)
    cc_imp <- imput$ximp

    # To create a summary table for the OOB errors (for each imputed variable):
    cc_imp_error <- data.frame(cbind(
      sapply(cc_mis, function(y) sum(length(which(is.na(y))))),
      sqrt(imput$OOBerror)),
      row.names = colnames(cc_mis))
    cc_imp_error %>% dplyr::rename(Nb_imputed_values = 'X1', Oob_RMSE = 'X2') %>%
      dplyr::filter(Nb_imputed_values > 0)-> sub_errtab_cc[[i]]

    cc_new <- cc
    cc_new$brood_size <- cc_imp$brood_size
    cc_new$cumdd_30 <- cc_imp$cumdd_30
    cc_new$cumdd_60 <- cc_imp$cumdd_60
    cc_new$cumdd_between <- cc_imp$cumdd_between
    cc_new$min_t_before <- cc_imp$min_t_before
    cc_new$min_t_between <- cc_imp$min_t_between





    ##### Reunite datasets
    # ____________________
    tits_imp <- rbind(pm_new, cc_new)
    tits_imp %>% dplyr::arrange(as.character(id_nestbox), as.character(year)) -> xxx

    sub_datasets[[i]] <- xxx
  }





  ##### Extract, reunite and rearrange imputed "dist"-based sub-datasets
  # ____________________________________________________________________
  df_50 <- sub_datasets[[1]]
  df_100 <- sub_datasets[[2]]
  df_150 <- sub_datasets[[3]]
  df_200 <- sub_datasets[[4]]
  tits_imp <- rbind(df_50, df_100, df_150, df_200)
  tits_imp %>% dplyr::arrange(as.character(id_nestbox), as.character(year), dist) -> xxx

  impute_error_pm_50 <- sub_errtab_pm[[1]]
  impute_error_pm_100 <- sub_errtab_pm[[2]]
  impute_error_pm_150 <- sub_errtab_pm[[3]]
  impute_error_pm_200 <- sub_errtab_pm[[4]]
  impute_error_pm <- impute_error_pm_50
  impute_error_pm$Oob_RMSE_100m <- impute_error_pm_100$Oob_RMSE
  impute_error_pm$Oob_RMSE_150m <- impute_error_pm_150$Oob_RMSE
  impute_error_pm$Oob_RMSE_200m <- impute_error_pm_200$Oob_RMSE
  impute_error_pm %>% dplyr::rename(Oob_RMSE_50m = Oob_RMSE) -> impute_error_pm

  impute_error_cc_50 <- sub_errtab_cc[[1]]
  impute_error_cc_100 <- sub_errtab_cc[[2]]
  impute_error_cc_150 <- sub_errtab_cc[[3]]
  impute_error_cc_200 <- sub_errtab_cc[[4]]
  impute_error_cc <- impute_error_cc_50
  impute_error_cc$Oob_RMSE_100m <- impute_error_cc_100$Oob_RMSE
  impute_error_cc$Oob_RMSE_150m <- impute_error_cc_150$Oob_RMSE
  impute_error_cc$Oob_RMSE_200m <- impute_error_cc_200$Oob_RMSE
  impute_error_cc %>% dplyr::rename(Oob_RMSE_50m = Oob_RMSE) -> impute_error_cc

  # To dismiss notes regarding "visible binding for global variables" during the CMD Check:
  id_nestbox <- year <- Nb_imputed_values <- coord_x <- coord_y <- father_cond <- father_id <-
    fledgling_nb <- flight_date <- laying_date <- mass <- mother_cond <- mother_id <- site <-
    species <- success_manipulated <- tarsus_length <- wing_length <- Oob_RMSE <-
    built_area <- open_area <- trafic <- vegetation_area <- woody_area <- dist <-
    age_class <- herbaceous_area <- soft_manag_area <- strata_div <- woodyveg_sd <- NULL





  ##### To export the updated table
  # _______________________________
  readr::write_csv2(x = xxx, file = here::here("output", "tables", "ndata_clean.csv"))

  output <- list(clean_dataset = xxx,
                 priorimp_dataset = tits,
                 impute_error_pm = impute_error_pm,
                 impute_error_cc = impute_error_cc,
                 path = here::here("output", "tables", "ndata_clean.csv"))
  return(output)
  # _______________________________
}
