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
# properly and follow changes. HOWEVER, as I am importing {ppl.tits}, I should NOT call my
# custom functions like this: ppl.tits::my_function(). I should call them directly as if I had
# used library(ppl.tits)!
### Note also that I cannot call my targets in the functions of my package (in their .R files). As
# a consequence, when a function/target creates a new version of a dataset, it should be exported as
# a .csv file: the target thus returns the path to the .csv file, which can be used in the subsequent
# functions to import the said dataset using something like `read_csv()` instead of directly calling
# the previous function that generated the said dataset. That way, {targets} is able to built the
# pipeline and track changes. If you try to call a target in a function, the pipeline will not work
# because only when you build it (using `tar_make()`) are the targets created, so you cannot call
# in a function an object that does not yet exist!
### To go further: "https://books.ropensci.org/targets/".
### See also: https://stackoverflow.com/questions/68683153/preferred-approach-for-making-the-targets-r-package-detect-changes-to-functi
list(
  ### All targets related to "external" input files #
  # ______________________________________
  targets::tar_target(raw_data_file, here::here("mydata", "ppl_dijon_tits_data.csv"),
             format = "file"),
  targets::tar_target(boxtemp_file, here::here("mydata", "paired_boxtemp.csv"),
             format = "file"),
  targets::tar_target(temp_file, here::here("mydata", "temp_data_20192022.csv"),
             format = "file"),
  targets::tar_target(predictor_file, here::here("mydata", "tits_predictors.csv"),
             format = "file"),



  ### All targets related to data-processing #
  # __________________________________________
  ### IMPORTANT NOTE: data-processing targets that produce new versions of the datasets are
  # typically stored in the "output files" section (see below) and not here!

  # Read the raw data and return a data.frame:
  targets::tar_target(raw_tits, import_raw_tits_data(mypath = raw_data_file)),



  ### All targets related to output files #
  # _______________________________________
  # Export the nestling aggregated dataset:
  targets::tar_target(nestling_agg_data, export_nestling_aggreg(myrawdata = raw_tits), format = "file"),
  # Produce the first tits data update (formatting and inclusion of the breeding_window and
  # temperature-related variables):
  targets::tar_target(tdata_temp, tdata_upD_temp(
    myboxtemp_data = boxtemp_file, mytemp_data = temp_file, mytits_data = nestling_agg_data),
    format = "file"),
  # Produce the second tits data update (joining of the raw independent variables):
  targets::tar_target(tdata_rawiv, tdata_upD_rawiv(
    my_tdata = tdata_temp, my_iv_data = predictor_file)$path, format = "file"),
  # Produce the third tits data update (computing the parental condition proxy variables):
  targets::tar_target(tdata_parcond, tdata_upD_parcond(
    my_tdata = tdata_rawiv)$path, format = "file")
  )

