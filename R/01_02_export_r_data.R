# --------------------------------------------------------------------- #
##### Functions to export datasets for the {ppl.tits} package users #####
# --------------------------------------------------------------------- #

### ________________________________________
#' Read stored RData objects from their path
#'
#' @description (THEORETICALLY MEANT FOR INTERNAL USE ONLY, use with caution!).
#' @param mypath Path to the name of the stored data (created by
#' \code{\link[base:save]{save}}).
#'
#' @return The stored data (exactly like `pkg::stored_data` would).
#' @export
#'
#' @examples
#' \dontrun{
#' # read_from_path(mypath = path_to_package_data)
#' }
read_from_path <- function(mypath){

  envir <- environment()
  data_name <- load(file = mypath, envir = envir)
  get(data_name)

  # To be fair, I stole this function from: https://github.com/ropensci/targets/discussions/598
}





### ____________________________________________________________________________
#' Export the 'tits nestling' aggregated dataset (i.e. `tits_nestling_data.csv`)
#'
#' @description (THEORETICALLY MEANT FOR INTERNAL USE ONLY, use with caution!) \cr
#' The `export_nestling_aggreg()` function aggregates the raw tits data to create a table
#' summarizing tits' breeding success for each nestbox each year where breeding occurred. As such,
#' it only keeps information on nestlings/juveniles and disregards the lines about adult tits. Then, the
#' function exports the created dataset (.csv) and returns its path. \strong{Actually}, the function
#' also creates the folders in which the created dataset will be stored.
#'
#' @param myrawdata The path to the raw tits data object (`raw_tits_data.rda`). Cf.
#' \code{\link[ppl.tits:import_raw_tits_data]{import_raw_tits_data}}.
#'
#' @return The path to the aggregated nestling tits dataset: `tits_nestling_data.csv`.
#'
#' @export
#' @importFrom readr write_csv2
#' @importFrom here here
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr first
#'
#' @examples
#' \dontrun{
#' export_nestling_aggreg(myrawdata = raw_tits)
#' }

### I need an internal object, no???
export_nestling_aggreg <- function(myrawdata){

  ##### Data preparation
  # ____________________
  rtits <- ppl.tits::read_from_path(mypath = myrawdata)

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

  # To dismiss R CMD checks warnings for unbound variables:
  age <- id_nestbox <- year <- laying_date <- incubation_date <- hatching_date <- clutch_size <- NULL
  brood_size <- fledgling_nb <- success_manipulated <- father_id <- mother_id <- species <- NULL
  nestling_mass <- nestling_tarsus_l <- nestling_wing_l <- NULL





  ##### Output folders creation
  # ___________________________

  dir.create("output")
  dir.create("output/tables")


  ##### Table export and output
  # ___________________________
  readr::write_csv2(x = xxx, file = here::here("output", "tables", "tits_nestling_data.csv"))
  return(here::here("output", "tables", "tits_nestling_data.csv"))
}





### ______________________________________________________
#' Exports the `ntits_clean` dataset to the "data/" folder
#'
#' @description FOR INTERNAL USE ONLY. DO NOT RUN.
#' @param my_tdata The path to "ndata_parcond.csv" because, internally, this function calls the
#'
#'
#' @return The path to the dataset generated
#' by \code{\link[ppl.tits:tdata_upD_parcond]{tdata_upD_parcond}} because, internally, this function calls
#' \code{\link[ppl.tits:tdata_upD_final]{tdata_upD_final}} and thus requires the same input dataset.
#' @export
#' @importFrom usethis use_data
#'
#' @examples
#' \dontrun{
#' # DO NOT USE!
#' export_ndata_clean()
#' }
export_ndata_clean <- function(my_tdata = here::here("output", "tables", "ndata_parcond.csv")){

  # To create the R object I want to export and made available for the package users:
  ntits_clean <- ppl.tits::tdata_upD_final(my_tdata = my_tdata)$clean_dataset

  # To export the created dataset:
  usethis::use_data(ntits_clean, overwrite = TRUE) # Creates/updates data/ntits_clean.rda and, the
  # first time it is run, modifies the DESCRIPTION file to add `LazyData: true` and `Depends:
  # R (>= 2.10)`!

  return(here::here("data", "ntits_clean.rda"))
  # IMPORTANT NOTE:
  # An alternative function might be found here: https://github.com/ropensci/targets/discussions/588
}


