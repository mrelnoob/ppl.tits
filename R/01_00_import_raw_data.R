
#' Import the raw tits dataset
#'
#' Imports ONLY the raw dataset for the "tits" part of the PubPrivLands project, and drops
#' lines with no information on 'success' as well as one useless column. It also correct a few mistakes
#' and deletes two late reproduction events that would otherwise bias the analyses. \cr To avoid errors,
#' if new data are added using this function, they should be formatted
#' according to the first table I tailored this function for (i.e. same columns, no special characters, no
#' spaces, etc.; the only tolerated differences may be different rows and cell values)!
#'
#' @param mypath The absolute path to the raw .csv file.
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
#' mydata <- import_raw_tits_data(mypath = "C:/path/to/my/data.csv") # This example cannot work,
#' # it needs an existing path!
#' }
import_raw_tits_data <- function(mypath = here::here("mydata", "ppl_dijon_tits_data.csv")){

  ### Raw data import___________________________________________________________#
  aaa <- readr::read_csv2(file = mypath, col_names = TRUE, na = "NA",
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
                                                 success_manipulated = readr::col_factor(),
                                                 father_id = readr::col_factor(),
                                                 mother_id = readr::col_factor(),
                                                 id_bird = readr::col_factor(),
                                                 id_ring = readr::col_factor(),
                                                 species = readr::col_factor(),
                                                 sex = readr::col_factor(),
                                                 age = readr::col_factor(
                                                   ordered = TRUE,
                                                   levels = c("nestling", "one", "more_than_one",
                                                              "two", "more_than_two"),
                                                   include_na = TRUE),
                                                 adult_mass = readr::col_double(),
                                                 adult_tarsus_l = readr::col_double(),
                                                 adult_wing_l = readr::col_double(),
                                                 adult_aggress = readr::col_factor(
                                                   ordered = TRUE,
                                                   levels = c("0,0", "0,5", "1,0", "1,5", "2,0",
                                                              "2,5", "3,0"),
                                                   include_na = TRUE),
                                                 nestling_mass = readr::col_double(),
                                                 nestling_tarsus_l = readr::col_double(),
                                                 nestling_wing_l = readr::col_double()))


  ### Data corrections__________________________________________________________#
  # To delete 'site' and only keep observations with a success evaluation:
  aaa %>% dplyr::select(-site) %>%
    dplyr::filter(success != 'NA') -> xxx

  # To correct the erroneous "bird_id" (i.e. 'BRFAIPM1120'):
  xxx$id_bird <- as.character(xxx$id_bird) # As R cannot tolerate adding new levels to a factor
  # variable, I have to first convert the variable as a character one!
  xxx[xxx$id_bird == "BRFAIPM1120" &
        xxx$id_ring == "8877045", "id_bird"] <- "THPAGPM095X"
  xxx$id_bird <- as.factor(xxx$id_bird)

  # TO correct the 'laying_date' of the female tits with 'id_bird' = CLPECPM0005:
  xxx$laying_date <- as.character(xxx$laying_date) # Idem.
  xxx[xxx$id_bird == "CLPECPM0005", "laying_date"] <- "09/04/2019"
  xxx$laying_date <- as.factor(xxx$laying_date)

  # TO correct wrong father_id values for DIJ-272_2021 and DIJ-134_2022:
  xxx$father_id <- as.character(xxx$father_id) # Idem.
  xxx[xxx$father_id == "8876210" &
        xxx$year == "2021", "father_id"] <- "8876510"

  xxx[xxx$father_id == "8877594" &
        xxx$year == "2022", "father_id"] <- "8876594"
  xxx$father_id <- as.factor(xxx$father_id)

  # TO correct wrong id_ring values for the females of DIJ-133_2022 and DIJ-179_2020:
  xxx$id_ring <- as.character(xxx$id_ring) # Idem.
  xxx[xxx$id_ring == "8877950" &
        xxx$year == "2022", "id_ring"] <- "8877590"

  xxx[xxx$id_ring == "8381599" &
        xxx$year == "2020", "id_ring"] <- "8581599"
  xxx$id_ring <- as.factor(xxx$id_ring)

  # To correct the gender/sex of the presumed father of DIJ-212_2022:
  xxx[xxx$id_ring == "8877107", "sex"] <- "male" # Here, as i'm not trying to add a new level to
  # the 'sex' factor, I do not need to convert 'the 'sex' into a character variable.

  # To delete late reproduction events (i.e. for DIJ-175 in 2019 and DIJ-044 in 2022):
  xxx %>% dplyr::filter(date != "28/06/2022") %>%
    dplyr::filter(laying_date != "09/05/2019") -> xxx

  # To convert "age" and "adult_aggress" into numeric variables:
  xxx %>% dplyr::mutate(
    age = dplyr::case_when(
      as.character(age) == "one" ~ 1,
      as.character(age) == "more_than_one" ~ 2,
      as.character(age) == "two" ~ 2,
      as.character(age) == "more_than_two" ~ 3),
    adult_aggress = as.numeric(gsub(x = adult_aggress, pattern = ",", replacement = "."))) -> xxx # As
  # "adult_aggress" contained commas instead of decimal points, I had to use the 'gsub()' function to
  # convert them (it's a base function close to 'grep()').
  summary(xxx)



  ### To avoid warnings during the R CMD check__________________________________#
  site <- success <- date <- laying_date <- NULL # This is necessary for all not globally
  # defined variables, that is variables that are called without "$" (such as calls from
  # {dplyr}; e.g. if your function uses something like dplyr::filter(myvar == "something"),
  # then the variable called "myvar" should either be globally defined or set to NULL, otherwise
  # you'll get warnings when you'll build your package).

  return(xxx)

}

