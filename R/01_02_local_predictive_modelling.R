# --------------------------------------------------------- #
##### Functions to create the "local" predictive models #####
# --------------------------------------------------------- #

# The functions of this R file are meant to prepare and build the so-called "local models" of our
# analysis workflow. These predictive models, based only on local environmental factors (hence their
# name), are intended to approximate the "quality" of habitat patches in our study area and thus to
# assess the "capacity" of these patches in the subsequent Graphab connectivity models.

utils::globalVariables("where") # This is necessary for now as tidyselect::where is not
# an exported function!



###### Reduced dataset for Random Forest regressions ######
library(magrittr)
tits <- targets::tar_read(final_tdata)
summary(tits)
colnames(tits)

tits %>% dplyr::mutate(repro_success = fledgling_nb/clutch_size) %>%
  dplyr::select(repro_success, species, coord_x, coord_y, mean_winter_t, sd_winter_t, lsource_vs150_m,
                lsource_vs150_iq, noise_m, noise_iq, trafic, woodyveg_volume, woodyveg_sd,
                vegetation_area, woody_area, herbaceous_area, soft_manag_area, open_area,
                built_area) -> stits
summary(stits)

########## FOR PM ###########
stits %>% dplyr::filter(species == "PM") -> pm

pm_null.rf <- randomForest::randomForest(repro_success ~ ., data = pm, importance = TRUE, nPerm = 2,
                                         oob.prox = TRUE)?????



# argument nperm!!!
##### Pour comparer mon model NULL avec mon full RF, je dois faire une CV, non???????? NON car je peux
# simplement utiliser l'importance des variables pour voir si woody_cover est le prédicteur dominant!
