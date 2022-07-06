# Function to import raw data

### Read the table:
tits <- readr::read_csv2(here::here("mydata", "ppl_dijon_tits_data_20192021.csv"), col_names = TRUE, na = "NA",
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

summary(tits)
