# --------------------------------------- #
# --------------------------------------- #
##### Script for all Graphab analyses #####
# --------------------------------------- #
# --------------------------------------- #

### First, this will be my script for all Graphab analyses (preliminary and final). Then, if
# I can make RStudio accept that my R script behave like a shell CLI (command-line interface), I'll
# consider create associated functions and targets.
# If I find a way to do it, I shoud UPDATE _devhistory.R (at least to send future me here or, even
# better, to correct my script to avoid sentences saying that I did it in the Terminal/shell CLI).

### NOTE: For R packages with CLI wrappers it is important to name extra dependencies in the
# SystemRequirements field in the package DESCRIPTION file. Thereby you formally declare that some
# additional piece of software is needed to make the package work
# (https://www.r-bloggers.com/2021/09/how-to-use-system-commands-in-your-r-script-or-package/)!

##################################### SUPER IMPORTANT NOTE ###########################################
### The EASIER way to start here will be to use this script ONLY to keep track of my command lines for
# Graphab (so I should NOT try to load or install my package in the meantime UNLESS I assign this file
# to .RBuildignore first !!!). When I'll have something that works smoothly, I will be able to
# translate it to a true R script working as a CLI wrapper by using one of the packages described in
# the link above!
# -------------------------------------------------------------------------------------------------- #





# ----------------------- #
##### 0. How to start #####
# ----------------------- #

# The first thing to do is to have the Graphab software (.jar) in the desired folder. I will thus
# create a folder and paste it manually then tell R and Git to IGNORE it (because it's heavy and not
# expected in an R package). If you want to reproduce my work, you will have to paste the same version
# of Graphab at the same place (here, Graphab 2.8)!
# I also manually paste my input landcover raster file (lc_dijon_v8_simple.tif), and I tell Git to
# ignore the Graphab folder altogether because it contains heavy files.
dir.create("graphab")
usethis::use_build_ignore("graphab/")
usethis::use_build_ignore("graphab-2.8.1.jar")
usethis::use_git_ignore("graphab/")
usethis::use_git_ignore("graphab-2.8.1.jar")
# NOTE: As my landcover file is too heavy, I cannot put it on my Github account. To be able to
# reproduce my work, you will need this file though. You should then either contact me directly or
# find it in the project's Zenodo archive (*** WHEN READY).



# ---------------------------------------------------------------------------- #
### * 0.1. Useful command lines (for Windows) ----------------------------------

# ** 0.1.1. To know and change the current directory ----
cd \ # And write the path (with \ to separate directories) or press TAB for auto-completion
# (e.g. cd "\fmartin\Mes documents\projects\pubprivlands\analyses\ppl.tits"). The cmd will
# automatically add "" but you can just ignore them!
mydisk: # To change disk (e.g. D:).
dir # For the content of the directory.


# ** 0.1.2. To comment ----
rem # To comment (rem = remark). NOTE: this is a command, so it needs to be put first or after
# an operator if you comment AFTER a command (e.g. "cd & rem blabla"; and NOT "cd rem blabla"). So
# it sucks, that's because it is MS.DOS.



# ---------------------------------------------------------------------------- #
### * 0.2. Helpful Graphab command lines ---------------------------------------

# ** 0.2.1. Main commands and rules ----
java -jar graphab-2.8.1.jar # The "prefix command" telling the cmd to launch Graphab itself. This
# "prefix command" should start ANY new command line! That means that if I want to get the "help"
# from the software, I should first call Java and Graphab (and possibly my project) etc. As in:
java -jar graphab-2.8.1.jar --help
--mycommand # All Graphab related command start with a "--" (e.g. --linkset).
-myoption # All Graphab global options start with a "-" (e.g. -proc).
param # All tool parameters have no prefixes (e.g. maxcost).
# NOTE: as commands are separated by spaces, project elements cannot contain spaces!
--metrics # To display all available metrics.
--show # Shows the elements of a project (that needs to be loaded before this command).
# Note also that command lines are CASE SENSITIVE and do not tolerate SPACES in names (so I should
# call `my_LINK_100m` and not `My link 100M`, for instance)!!!


# ** 0.2.2. Starting, loading and saving projects ----
--create # To create a new project (includes compulsory parameters, cf. CLI User Guide).
--project myproject.xml # To load an existing project.


# ** 0.2.3. Intervals and range of values ----
min:inc:max # To define an interval of values, you have to specify a min, an increment, and a
# maximum value (e.g. 0:2:10 will do 0, 2, 4, 6, 8, 10). Decimal increments are accepted
# (e.g. 0:0.5:3).
0,1,4,8,30,200 # Will create a list of values (no spaces).
# NOTE: if a command contains several intervals/values for different parameters, the command will
# be executed for each possible combination!


# ** 0.2.4. Commands sequencing ----
# Graphab can be launched with several commands on the same line, except for -help and -metrics!
# The "--create" and "--project" commands can only be used once and should be the first command,
# after the prefix command that launches Graphab itself (e.g. java -jar graphab-2.8.jar --project...).



# ---------------------------------------------------------------------------- #
### * 0.3. Setting-up a Graphab project -----------------------------------------

# ** 0.3.1. To create a project ----
java -jar graphab-2.8.1.jar --create myproject.xml # To create the "myproject" project.
java -j(...)myproject.xml mylandcover.tif # To indicate the landcover raster file.
java -j(...)cover.tif habitat=5,7,13 # To define landcover codes 5, 7 and 13 as habitat.
java -j(...)5,7,13  nodata=0 # To define landcover code 0 as "habitat."no data".






java -j(...)myproject.xml --capa # To define the habitat patch capacity.





# --------------------------------- #
##### 1. Graphab project set-up #####
# --------------------------------- #

# ---------------------------------------------------------------------------- #
### * 1.1. Project creation and loading ----------------------------------------

# In the "tits reproduction study" of the PubPrivLands project, the Graphab project was created by
# Gilles Vuidel in order for him to create the habitat patches to be able to extract their pixels
# and compute the weightings for pixels and patches (i.e. their "capacity") based on the Random
# Forest model exported by the `ppl.tits::local_quality_model()` function.
# Consequently, I manually pasted the mentioned Graphab project and associated files in the graphab/
# folder of the {ppl.tits} package.
# I can then open the Graphab project (named `tits_gpb.xml`) with the following command:
java -jar graphab-2.8.1.jar --project .\tits_graphab_project\tits_gpb.xml


# --------------------------------- #
##### XX. Graphab projects for PM #####
# --------------------------------- #

# --------------------------------- #
##### XX.X. With patch area as capacity #####
# --------------------------------- #
##### XX.X. With predicted quality as capacity #####

# --------------------------------- #
##### XX. Graphab projects for CC #####
# --------------------------------- #

# --------------------------------- #
##### XX.X. With patch area as capacity #####
# --------------------------------- #
##### XX.X. With predicted quality as capacity #####



# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
usethis::use_git(message = ":boom: Started a new script")
usethis::use_git(message = ":metal: Created a new function")
usethis::use_git(message = ":zap: Ignoring something")
usethis::use_git(message = ":pencil: Edited a file")
usethis::use_git(message = ":hammer: Ongoing programming")
usethis::use_git(message = ":white_check_mark: Saved updates!")
#system("git push")
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
# ---------------------------------------------------------------------------- #
