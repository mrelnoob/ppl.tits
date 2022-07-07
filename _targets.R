# ------------------------------------------------ #
##### Structure of my data processing workflow #####
# ------------------------------------------------ #

#library(targets) # Perhaps I don't need it if i use ::!!!!!

list(
  # Make the workflow depends on the raw data file
  targets::tar_target(raw_data_file, here::here("mydata", "ppl_dijon_tits_data_20192021.csv"),
             format = "file"),
  # Read the data and return a data.frame
  targets::tar_target(raw_tits, ppl.tits::import_raw_tits_data())
)
# NOTE: it might not work because my function already calls here::here (I should perhaps change it first)!
# But it seems to work!