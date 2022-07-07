
#' Import the raw tits dataset
#'
#' Imports ONLY the raw dataset for the "tits" part of the PubPrivLands project, and drops
#' lines with no information on 'age' or 'success' as well as two useless columns. \cr To avoid errors,
#' if new data are added using this function, they should be formatted according to the first table
#' I tailored this function for (i.e. same columns, no special characters, no spaces, etc.; the only
#' tolerated differences may be different rows and cell values)!
#'
#' @return A tibble (i.e. a kind of improved data.frame). For further information on tibbles, please refer to
#' the `tidyverse` or \link[readr]{readr} documentation.
#' @export
#'
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_integer
#' @importFrom readr col_double
#' @importFrom here here
#' @importFrom dplyr select
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' mydata <- import_raw_tits_data()
#' }
import_raw_tits_data <- function(){

  aaa <- readr::read_csv2(here::here("mydata", "ppl_dijon_tits_data_20192021.csv"), col_names = TRUE, na = "NA",
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

  aaa %>% dplyr::select(-site, -nestling_mass_j14) %>%
    dplyr::filter(success != 'NA', age != 'NA') -> xxx

  return(xxx)

}

