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
# a consequence, when a function/target creates a new version of a dataset, I need to export

list(
  ### All targets related to "external" input files #
  # ______________________________________
  targets::tar_target(raw_data_file, here::here("mydata", "ppl_dijon_tits_data.csv"),
             format = "file"),
  targets::tar_target(boxtemp_file, here::here("mydata", "paired_boxtemp.csv"),
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
    myboxtemp_data = boxtemp_file, mytits_data = nestling_agg_data), format = "file"),
  # Produce the second tits data update (joining of the raw independent variables):
  targets::tar_target(tdata_rawiv, tdata_upD_rawiv(
    my_tdata = tdata_temp, my_iv_data = predictor_file)$path, format = "file")
  # Par ailleurs, ça ne sert à rien de faire les variables MOTHER/FATHER RF etc. pour le modèle local! Mais comme je vais
  # me concentrer d'abord sur le script Graphab et les analyses générales, je vais les calculer quand même! Ca sera fait!
  ### Par ailleurs, je pourrais modifier mes fonctions pour qu'elles retourne plusieurs choses: 1) le chemin et 2) les
  # tables. Comme ça je n'aurais pas forcément à read_csv à chaque fois que je veux ouvrir un ostie de tdata!!! Non?

  )

# Il faut que je lise "https://books.ropensci.org/targets/" si je veux vraiment m'y mettre, car ça ne va pas. Quand j'aurais le
# temps. Voir aussi : https://stackoverflow.com/questions/68683153/preferred-approach-for-making-the-targets-r-package-detect-changes-to-functi
# qui traite du développement de fonctions/packages en utilisant TRAGETS§§§
