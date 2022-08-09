# ------------------------------------------------ #
##### Structure of my data processing workflow #####
# ------------------------------------------------ #

#library(targets) # I don't need it if i use "::"!
# I should also source my custom functions but I don't want to, because that's one of the advantages of
# building an R package.

list(
  # Make the workflow depends on the raw data file
  targets::tar_target(raw_data_file, here::here("mydata", "ppl_dijon_tits_data.csv"),
             format = "file"),
  # Read the data and return a data.frame
  targets::tar_target(raw_tits, ppl.tits::import_raw_tits_data()),
  # Export the nestling aggregated data table
  targets::tar_target(nnestling_data, ppl.tits::export_nestling_aggreg(), format = "file")
  )
### The last 'target' works (so to speak) but does not export the table: it only returns the path. Also, there are no
# connections between my 'targets' in the visualisation (because I do not call previous 'targets' in my 'targets' so
# it cannot keep track???)....
# I lack time for this...
# C'est sans doute car je n'appelle pas les targets créés dans mes fonctions, donc targets n'arrive pas à suivre réellement
# les fichiers et modifs si les fonctions utilisées comme arguments des tar_target() n'appelle pas les objets targets déjà créés ??
# Il faut que je lise "https://books.ropensci.org/targets/" si je veux vraiment m'y mettre, car ça ne va pas. Quand j'aurais le
# temps. Voir aussi : https://stackoverflow.com/questions/68683153/preferred-approach-for-making-the-targets-r-package-detect-changes-to-functi
# qui traite du développement de fonctions/packages en utilisant TRAGETS§§§
