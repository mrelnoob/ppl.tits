# ------------------------------------------------ #
##### Structure of my data processing workflow #####
# ------------------------------------------------ #

#library(targets) # I don't need it if i use "::"!

### I should also source my custom functions but I don't want to, because that's one of the
# advantages of building an R package. However, it will prevent {targets} from tracking changes
# to my functions. To avoid that, I need to set-up a few global options with:
targets::tar_option_set(packages = "ppl.tits",
                        imports = "ppl.tits")
### Also note that my targets should call other targets in order for the pipeline to work
# properly and follow changes.

list(
  ### All targets related to input files #
  # ______________________________________
  targets::tar_target(raw_data_file, here::here("mydata", "ppl_dijon_tits_data.csv"),
             format = "file"),
  targets::tar_target(boxtemp_file, here::here("mydata", "paired_boxtemp.csv"),
             format = "file"),
  targets::tar_target(predictor_file, here::here("mydata", "tits_predictors.csv"),
             format = "file"),


  ### All targets related to data-processing #
  # __________________________________________
  # Read the raw data and return a data.frame:
  targets::tar_target(raw_tits, ppl.tits::import_raw_tits_data(mypath = raw_data_file)),
  # Produces the first tits data update (formatting and inclusion of the breeding_window and
  # temperature-related variables):
  targets::tar_target(tdata_temp, ppl.tits::tdata_upD_temp(
    myboxtemp_data = boxtemp_file, mytits_data = nestling_agg_data)),


  ### All targets related to output files #
  # _______________________________________
  targets::tar_target(nestling_agg_data, ppl.tits::export_nestling_aggreg(myrawdata = raw_tits), format = "file")

  )

# Il faut que je lise "https://books.ropensci.org/targets/" si je veux vraiment m'y mettre, car ça ne va pas. Quand j'aurais le
# temps. Voir aussi : https://stackoverflow.com/questions/68683153/preferred-approach-for-making-the-targets-r-package-detect-changes-to-functi
# qui traite du développement de fonctions/packages en utilisant TRAGETS§§§
