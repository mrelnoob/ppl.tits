# ------------------------------------------------ #
##### Structure of my data processing workflow #####
# ------------------------------------------------ #

#library(targets) # Perhaps I don't need it if i use ::!!!!!

list(
  # Make the workflow depends on the raw data file
  targets::tar_target(raw_data_file, here::here("mydata", "ppl_dijon_tits_data.csv"),
             format = "file"),
  # Read the data and return a data.frame
  targets::tar_target(raw_tits, ppl.tits::import_raw_tits_data()),
  # Export the nestling aggregated data table
  targets::tar_target(nnestling_data, ppl.tits::export_nestling_aggreg(), format = "file")
  )
### La dernière target "marche", mais ne génère que le path (et pas la table), et il n'y a pas de connexions
# entre mes targets dans la visualisation. Pk???
