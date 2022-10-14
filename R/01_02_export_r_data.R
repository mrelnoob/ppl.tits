# --------------------------------------------------------------------- #
##### Functions to export datasets for the {ppl.tits} package users #####
# --------------------------------------------------------------------- #

### ______________________________________________________
#' Exports the `ntits_clean` dataset to the "data/" folder
#'
#' @description FOR INTERNAL USE ONLY. DO NOT RUN.
#' @param my_clean_data The path to "ndata_clean.csv".
#'
#' @return The path to "data/ndata_clean.rda".
#' @export
#' @importFrom here here
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_integer
#' @importFrom readr col_date
#' @importFrom usethis use_data
#'
#' @examples
#' \dontrun{
#' # DO NOT USE!
#' export_ndata_clean()
#' }
export_ndata_clean <- function(my_clean_data = here::here("output", "tables", "ndata_clean.csv")){

  # To create the R object I want to export and made available for the package users:
  ntits_clean <- readr::read_csv2(my_clean_data, col_names = TRUE,
                                  col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                          site = readr::col_factor(),
                                                          year = readr::col_factor(),
                                                          species = readr::col_factor(),
                                                          breeding_window = readr::col_factor(),
                                                          laying_date = readr::col_date(),
                                                          flight_date = readr::col_date(),
                                                          clutch_size = readr::col_integer(),
                                                          fledgling_nb = readr::col_integer(),
                                                          father_id = readr::col_factor(),
                                                          mother_id = readr::col_factor(),
                                                          age_class = readr::col_factor(
                                                            ordered = TRUE,
                                                            levels = c("0", "1", "2"),
                                                            include_na = TRUE
                                                          ),
                                                          strata_div = readr::col_factor(
                                                            ordered = TRUE,
                                                            levels = c("0", "1", "2", "3", "4"),
                                                            include_na = TRUE)))

  usethis::use_data(ntits_clean, overwrite = TRUE) # Creates/updates data/ntits_clean.rda and, the
  # first time it is run, modifies the DESCRIPTION file to add `LazyData: true` and `Depends:
  # R (>= 2.10)`!

  return(here::here("data", "ntits_clean.rda"))
  # IMPORTANT NOTE:
  # An alternative function might be found here: https://github.com/ropensci/targets/discussions/588
}


