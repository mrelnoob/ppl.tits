# -------------------------------------------------------- #
##### Script (and functions?) for all Graphab analyses #####
# -------------------------------------------------------- #

### First, this will be my script for all Graphab analyses (preliminary and final). Then, if
# I can make RStudio accept that my R script behave like a shell CLI (command-line interface), I'll
# consider create associated functions and targets.
# If I find a way to do it, I shoud UPDATE _devhistory (at least to send future me here or, even better,
# to correct my script to avoid sentences saying that I did it in the Terminal/shell CLI).

### NOTE: For R packages with CLI wrappers it is important to name extra dependencies in the SystemRequirements
# field in the package description file. Thereby you formally declare that some additional piece of software
# is needed to make the package work (https://www.r-bloggers.com/2021/09/how-to-use-system-commands-in-your-r-script-or-package/)!


######################################### SUPER IMPORTANT NOTE #########################################
### The EASIER way to start here will be to use this script ONLY to keep track of my command lines for
# Graphab (so I should NOT try to load or install my package in the meantime UNLESS I assign this file
# to .RBuildignore first !!!). When I'll have something that works smoothly, I will be able to translate
# it to a true R script working as a CLI wrapper by using one of the packages described in the link above!
###########################################################################################################


test <- system2("dir")


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
usethis::use_git(message = ":boom: Started my Graphab script")
usethis::use_git(message = ":white_check_mark: Edited structure")
usethis::use_git(message = ":metal: Saved updates!")
#system("git push")
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
# ---------------------------------------------------------------------------- #
