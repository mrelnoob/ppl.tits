################### *----------------------------------------* #################
########################## DEV_HISTORY FOR {ppl.tits} ##########################
################### *----------------------------------------* #################

# All comments in this file or in this project are first meant for future me and, second,
# for other so they can reproduce my work. That is why there are so many of them, so anyone can
# do what I did, regardless of his/her own experience. Note also that English and R are not my
# native tongues, so please forgive any mistakes.
# NOTE: As you'll see, I chose to develop an R package alongside a {targets} pipeline to organise
# my data analysis procedure. HOWEVER, it is quite time consuming and, once you've understand how
# to use {targets} and {renv} and if you do not actually need to share your package, then using a
# simple {targets} project (coupled with {renv}) is largely sufficient! Think about it next time
# rather than always developing a package!
# Note also that I briefly explain how to use {targets} at the end of this document and I will add
# {renv} later, if I can.





# -------------------------------- #
##### 0. Important R reminders #####
# -------------------------------- #

### * 0.1. NA handling ---------------------------------------------------------
# ---------------------------------------------------------------------------- #

# Although it may seem strange, R doesn't handles NA as we could expect it. The use of "is.na()" or
# "!is.na()" should be preferred over the use of classical equality operators "==" or "!=".
# Cf. https://stackoverflow.com/questions/28857653/removing-na-observations-with-dplyrfilter



### * 0.2. Factor and character variables --------------------------------------
# ---------------------------------------------------------------------------- #

# R is kind of a pain in the arse when it comes to dealing with categorical variables. Theoretically,
# these are `character` objects but most R functions expect `factor` objects. So you have to frequently
# juggle between those two types of data format and remember that a `factor` has `levels` and thus that
# what you see on your screen is not how R handles categorical variables internally. So to modify a
# `factor` object, you often have to convert it first into a `character` object first.
# Cf. many of my custom functions.



### * 0.3. Package building, {targets} and RMarkdown ---------------------------
# ---------------------------------------------------------------------------- #

# Although I managed to create a full project associating Package building, {targets} and RMarkdown
# (cf. the present document), my way of doing it was not optimal. Next time, I should certainly take
# the time to learn to do it properly to avoid many mistakes and save time.
# Cf. https://books.ropensci.org/targets/





# ------------------------- #
##### 1. Project set up #####
# ------------------------- #

### * 1.1. Package project creation --------------------------------------------
# ---------------------------------------------------------------------------- #

# First, I created a package named "ppl.tits" (.Rproj) with the following command:
# usethis::create_package("d:/fmartin/Mes documents/projects/pubprivlands/analyses/ppl.tits")
# in which I specified the ABSOLUTE path to my package's folder (alternatively, I could have
# clicked one the buttons in RStudio to "create a package with devtools").
# IMPORTANT NOTE: to recreate this project (or reuse the code in another project), you do not
# need to re-run this line of code and recreate the package's folder because it already exists
# on my Github account.

# Then, I created a "dev history" file to keep track of everything I do in the project:
# usethis::edit_file("_devhistory.R")
# This is the document you are CURRENTLY reading!
# From now on, all command lines used to organize the project will be written in this file.
# IMPORTANT NOTE: in this case, I started a package project from scratch. If I want to start
# from a GitHub repository, the workflow is different but some same steps will be required so
# I encourage my future self to carefully review the steps of this workflow anyway.

# As this file is not part of a typical package structure, I need to tell R to ignore it
# when checking and installing the package:
usethis::use_build_ignore("_devhistory.R")



### * 1.2. Keeping track of changes (Git) --------------------------------------
# ---------------------------------------------------------------------------- #

# To keep track of all future changes and have a backup, I need to initiate a Git
# version control and link the 'ppl.tits' package with my GitHub account. In this precise case,
# I'm connecting my 'ppl.tits' project AFTER its creation, so I'm following the 'GitHub-last'
# procedure:
# STEP 1: Make sure Git is installed (e.g. by typing 'git status' in the R terminal), if not,
# install it!
# STEP 2: Enter your project and type 'git init' in the R Terminal, it will initiate a Git
# repository within your project:
system("git init") # NOTE: the 'system()' function enables sending commands to the Terminal/CLI,
# that is to work in "command-lines" (hence the name CLI, for "command-lines interface").
# It works yet the problem is that RStudio seems unable to display the CLI responses like a
# regular CLI, preventing interactions with it. That is why I sometimes say that I had to do
# stuff directly in a CLI/Terminal.
# STEP 3: Verify your configuration:
system("git config --global user.name 'fanf'")
system("git config --global user.email 'francois.martin74190@gmail.com'")
# Of course, you need to personalize the user.name and email. If you want the config to be
# only true for the current project (and not global), remove the '--global' from the previous
# lines.
# STEP 4: If you've never done it before (on this computer), you also need to set-up SSH keys
# to be able to connect to Git and GitHub without supplying you username and password every
# time you do something.
# The full procedure is described here: https://help.github.com/articles/generating-ssh-keys
# But here goes:
system("ls -al ~/.ssh") # Check for existing SSH keys. If none is found, you must generate one.
system("ssh-keygen -t rsa -C 'francois.martin74190@gmail.com'") # Generates a new SSH key
# using the rsa algorithm (however, I was forced to do that directly in a CLI).
system("eval '$(ssh-agent -s)'") # Checks if the ssh-agent is launched (same --> CLI).
# If it works, it should return something like "Agent pid 59552".
system("ssh-add ~/.ssh/id_rsa") # Adds the new SSH private key to the ssh-agent! (CLI).
# Then you have to add the public key to your GitHub account settings:
# e.g. https://kbroman.org/github_tutorial/pages/first_time.html
system("ssh -T git@github.com") # To check if it works. If it does, it should answer with
# something like "Hi mrelnoob! You've successfully authenticated, but Github does not provide
# shell access". There is clearly a bug in RStudio (or R) that prevents me from doing all
# that from RStudio, but anyway...

# STEP 5: Make some changes in the project and make your first commit:
usethis::use_git(message = ":tada: Initial commit") # Then restart RStudio and the Git tab will
# appear on the top-right box of RStudio.
# STEP 6: Log in your GitHub account and create a new repository (without anything in it).
# STEP 7: Use the following command to associate your R project with the GitHub project:
system2("git remote add origin git@github.com:mrelnoob/ppl.tits") # Here also, personalize
# with your own account and project names! And here again, it does not work (so --> CLI).
# STEP 8: Finally, you can push the changes made to your local files to GitHub:
system2("git push -u origin master") # Same (CLI).
# Even using a CLI (e.g. GitBash), you may receive an error message saying that the remote
# repository contains work that you do not have locally (...). It happens if you included files
# in your GitHub project when you created it (e.g. a README or a LICENCE file). So theoretically,
# you need to always start with a "pull" before you push anything! If, like me, you forgot,
# you'll be in trouble and won't be able to pull. To force Git, you may use "git push -f origin
# master" (the -f means to "force" the push).
# IMPORTANT NOTE: because of the CLI-RStudio bugs, I can "commit" from RStudio but I cannot push,
# so I will always be forced to do it from a CLI every time!



### * 1.3. Further basic configurations ----------------------------------------
# ---------------------------------------------------------------------------- #

# Before I go any further, I will edit some information about my package using the DESCRIPTION file.
usethis::edit_file("DESCRIPTION")
usethis::use_mit_license(copyright_holder = "François-Marie Martin")# Open-source license
usethis::use_git(message = ":page_facing_up: Edit package metadata")

# To create the package's documentation:
usethis::use_package_doc() # Creates a dummy file in the R folder that should NOT be modified!
devtools::document() # Creates the documentation and the man folder (for "manual").
usethis::use_git(message = ":bulb: Update documentation")





# ----------------------------------- #
##### 2. Project structure set-up #####
# ----------------------------------- #

### * 2.1. Folders architecture ------------------------------------------------
# ---------------------------------------------------------------------------- #

# Before writing my first functions to import and clean my data, I need to add the said data files
# in the project folder. So I create a "input_raw_data" folder and copy-paste my data in it (manually):
dir.create("input_raw_data") # As explained below, note that it is not the best way to create a package
# if the functions are to be exported in the end (cf. section 3.1.)!
usethis::use_build_ignore("input_raw_data") # Because it is not expected in a regular package root
# folder, I need to ignore it. If I don't, it will cause warnings in my package checks and all kind
# of problems.

# I may also need other folders to organize my data processing work (e.g. outputs). However, note that
# the right way to go that would be to create the desired folders in the associated functions that
# require those folders or, EVEN BETTER, to not create custom folders but to use the ones intended in
# a regular R package! Otherwise, the package will not be able to run properly for other end-users
# because functions will try to work with folders that were not created during the external
# installation of the package (cf. section 3.1.)!
# Still, if I want to create them in my workflow (out of the pipeline), I can use:
dir.create("output")
dir.create("output/plots")
dir.create("output/tables")
dir.create("output/texts")
dir.create("output/spatial_layers")

usethis::use_build_ignore("output/")
usethis::use_build_ignore("tables/")
usethis::use_build_ignore("texts/")
usethis::use_build_ignore("plots/")
usethis::use_build_ignore("spatial_layers/")
usethis::use_git_ignore("plots/")
usethis::use_git_ignore("spatial_layers/") # To avoid saturating Git, I ignore the folders prone to contain
# rather heavy files such as spatial layers and plots, but not tables and texts!



### * 2.2. Creating scripts for custom functions -------------------------------
# ---------------------------------------------------------------------------- #

# To use pipes (i.e. %>%) everywhere in the package without explicitly loading the {magrittr}
# package:
usethis::use_pipe() # Automatically creates a pipe function (and associated R file in the R folder),
# while updating the DESCRIPTION file to tell R that it should import the "magrittr" package.

usethis::use_r("01_00_import_raw_data") # Automatically places the R file in the R folder (as it
# should be). The R folder should ONLY contain R scripts for functions and NOTHING ELSE! Yet,
# RStudio may sometimes put other things in it, so it is a good idea to go and see once in
# a while.
usethis::use_r("01_01_prepare_data")
usethis::use_r("01_02_export_r_data")
usethis::use_r("02_00_local_predictive_modelling")
usethis::use_r("02_01_graphab_analyses")
usethis::use_r("02_02_exploratory_data_analyses")
usethis::use_r("03_01_pm_inference_modelling")
usethis::use_r("03_03_ntits_inference_modelling")



### * 2.3. Creating reports (RMarkdown) ----------------------------------------
# ---------------------------------------------------------------------------- #

file.create(... = "output/texts/ppl.tits.exploration_report.Rmd")
file.create(... = "output/texts/ppl.tits.intermediate_analyses_report.Rmd") # Using this command,
# a .Rmd file will be created but will lack the YAML header skeleton that should thus be manually
# placed at the top of the document.
# NOTE: for guidance to use .Rmd documents with {targets}, please refer to the {targets} related
# section (chapter 3 below).



# ** 2.3.1. To manage citations and bibliography ----

# To manage citations and get an automatic bibliography with RMarkdown, I have to follow these
# steps:
#  1) Using Zotero (or something similar), I have to 'export' the references to be cited in the
#     report in a BibTex format (.bib) and place this text file in the same folder as my .Rmd file.
#  2) Call this document in the `bibliography` field in the YAML metadata header (e.g.
#     bibliography: my_example.bib).
#  3) In text, I use arobases (@) and brackets ([], use semi-colons ";" for separation between
#     references) to add citations (e.g. "@Martin2022 said that..." or "blabla [@Martin2022;
#     see also @Darwin1832]").
#  4) I can change the citation style by using the `csl` field in the YAML metadata header
#     (e.g. csl: my_style.csl) and pasting the said style in the same folder as before.
# Thus, I pasted my BibTex file in the same folder as my .Rdm file. But in the case were my .Rmd
# file would be at the root of my package, I need to tell R to ignore it:
usethis::use_build_ignore("ppl.tits_biblio.bib")
# For practical reasons, this .bib file will certainly be updated many times during the duration
# of the project. Also, it may be useful to manually edit the file to shorten the reference tags
# since Zotero tends to create long tag using the name of the 1st author, the 1st work of the title
# and the year of publication.





# --------------------------------------- #
##### 3. Package content and creation #####
# --------------------------------------- #

### * 3.1. Managing data in a R package ----------------------------------------
# ---------------------------------------------------------------------------- #

# I'm used to creating custom folders to store my input and output data in my the R packages I build
# (e.g. input_raw_data/; output/text/), although these folders are not expected by R. I recently
# discovered why and how it may be problematic when I tried to share my package with someone else for
# the first time. Therefore, from now on, I'll try to respect the way R deals with data with regard
# to their nature: e.g. R objects, CSV files, texts, etc.
# Cf. https://r-pkgs.org/data.html#data (the next sections are only a summary).


# ** 3.1.1. To export R objects for end-users ----

# If you want to store R objects and make them available to the user, put them in `data/`. This is
# the best place to put example datasets (keep in mind that it is for R objects only). See section
# Section 8.2. of the above link for more details.
# To do that, the easiest way is to use:
usethis::use_data(some_R_object)
# This snippet creates `data/some_R_object.rda` inside the source of the package and adds
# "LazyData: true" in your DESCRIPTION. This makes the `some_R_object` R object available to users of
# the package via `pkg::some_R_object` or, after attaching the package with:
library(pkg)
some_R_object
# NOTE: the `use_data()` function used above is a workflow code and, theoretically, SHOULD NOT appear
# in the R/ folder (it should be kept somewhere else: e.g. _devhistory.R, or in the R files in the
# data-raw/ folder as shown below). HOWEVER, as I want to use it with my {targets} pipeline (see
# Chapter 4. if you do not know what I mean), I will create custom functions to export and update
# my exported datasets and call these functions in my _targets.R pipeline!
# IMPORTANT NOTE: remember that data exported as `data/some_R_object.rda` are only meant for export
# to end users and CANNOT be used internally (e.g. to load the data within a given function) because
# R modifies the content of `data/` during install meaning that it won't be able to find the datasets
# that are called (after installation, `some_R_object.rda` doesn't exist anymore, it's compiled in a
# different way and named differently so it's still available to users but not to function internals)!!!
# The same statement is true for .csv files stored in folders R does not expect (e.g. `outputs/` or
# `results/`).
# To store datasets for internal use, you NECESSARILY NEED TO use the proper way (cf. section 3.1.2.)!

# The way `some_R_object` is created (raw data import, wrangling etc.) should preferably be saved as
# well, but not necessarily as true function in R/. If you don't want to source the code directly in R/,
# you can use:
usethis::use_data_raw("to_make_my_R_object")
# This function creates the data-raw/ folder and lists it in .Rbuildignore. A typical script in
# data-raw/ includes code to prepare a dataset and ends with a call to `use_data()`.
# NOTE: Objects in data/ are always effectively exported (they use a slightly different mechanism
# than NAMESPACE but the details are not important). This means that they must be documented using:
usethis::use_r("data") # To create R/data.R to document my dataset! See section 8.2. of
# https://r-pkgs.org/data.html#data for more details (e.g. how to document data properly with
# Roxygen2, how to include non-ACSII characters, etc.).


# ** 3.1.2. To store R objects for internal use ----

# If you want to store R objects for your own use as a developer, put them in R/sysdata.rda. This is
# the best place to put internal data that your functions need. See section Section 8.3. of
# https://r-pkgs.org/data.html#data for more details.
# Sometimes the objects you need are small and simple enough that you can define them with `c()` or
# `data.frame()` in the code below R/, perhaps in R/data.R. Larger or more complicated objects should
# be stored in your package’s internal data in R/sysdata.rda.
# The easiest way to create R/sysdata.rda is to use:
internal_this <- ...
internal_that <- ...
usethis::use_data(internal_this, internal_that, internal = TRUE) # The documentation of `use_data()`
# is also useful here.
# Unlike data/, where you use one .rda file per exported data object, you store all of your internal
# data objects together in the single file R/sysdata.rda.
# Let’s imagine we are working on a package named “pkg”. The snippet above creates R/sysdata.rda
# inside the source of the {pkg} package. This makes the objects `internal_this` and `internal_that`
# available for use inside of the functions defined below R/ and in the tests. During interactive
# development, `internal_this` and `internal_that` are available after a call to `devtools::load_all()`,
# just like any internal function.
# IMPORTANT NOTE: unlike data in data/, objects in R/sysdata.rda are not exported (they shouldn’t be),
# so they don’t need to be documented. Also, usage of R/sysdata.rda has no impact on DESCRIPTION,
# i.e. the need to specify the "LazyData" field is strictly about the exported data below data/.


# ** 3.1.3. To store and export data in non-R format (e.g. HTML files, .csv) ----

# If you want to store data in some raw, non-R-specific form and make it available to the user, put it
# in inst/extdata/. See section Section 8.4. of https://r-pkgs.org/data.html#data for more details.
# When the package is installed, all files (and folders) in inst/ are moved up one level to the
# top-level directory, which is why they can’t have names that conflict with standard parts of an R
# package, like R/ or DESCRIPTION . The files below inst/extdata/ in the source package will be
# located below extdata/ in the corresponding installed package.
dir.create("inst")
dir.create("inst/extdata")
# NOTE: The path to a package file found below extdata/ clearly depends on the local environment,
# i.e. it depends on where installed packages live on that machine. The base function `system.file()`
# can report the full path to files distributed with an R package. It can also be useful to list the
# files distributed with an R package; e.g.:
system.file("extdata", package = "readxl") |> list.files()
system.file("extdata", "clippy.xlsx", package = "readxl")
# But don't forget `here::here()` either!

# IMPORTANT NOTE: remember that data exported in custom folders (e.g. outputs/ or results/) are
# only available to users if they clone the package repository directly from Github, not if they
# install the package from Github! Similarly to objects exported to `data/` (see section 3.1.1.), these
# data thus CANNOT BE CALLED for internal use, as they do not exist anymore in the installed version
# of the package (unless, perhaps [I did not try], if they are stored in int/extdata/)!!!
# Here again, the reason is because R modifies the content of the package folder during install
# meaning that it won't be able to find the datasets that are called (after installation, `data.csv`
# doesn't exist anymore if it was stored in a custom folder, but I think it does if it was stored in
# the proper folder: i.e. int/extdata/).
# To store datasets for internal use, you NECESSARILY NEED TO use the proper way (cf. section 3.1.2.)!



### * 3.2. Writing functions for the package -----------------------------------
# ---------------------------------------------------------------------------- #

# Now I can write my functions in the R files (= populating my R files) while keeping in mind
# that, for EACH function I create:
#  --> I need to insert a roxygen skeleton to write the documentation.
#  --> I should NOT SOURCE my functions (i.e. I should save my function's file but I should
#      not run the code in it and create "manually" the function I just wrote) to avoid
#      conflicts, see below!
# IMPORTANT NOTE: if a function uses functions from other packages, I need to tell R about
# it by updating the NAMESPACE file (for R to know what to load when it loads my package).
# It will be done automatically by {devtools} when we produce our documentation if we have
# previously listed the dependencies (packages) in the Roxygen2 header of the functions thanks
# to the tags (@import package OR @importFrom package function)! So I do this for every function
# and add the required tags and packages in the function's Roxygen2 header, and I DO NOT FORGET
# to also add these dependencies in the "Imports" field of the DESCRIPTION with the following
# command lines:
usethis::use_package("readr")
usethis::use_package("here")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("tidyselect")
usethis::use_package("stats")
usethis::use_package("stringr")
usethis::use_package("degday")
usethis::use_package("missMDA")
usethis::use_package("FactoMineR")
usethis::use_package("assertthat")
usethis::use_package("missForest")
usethis::use_package("randomForest")
usethis::use_package("ggplot2")
usethis::use_package("usethis")
# REMINDER: The NAMESPACE controls what happens when my package is loaded but not when it's
# installed. This is the role of the DESCRIPTION file!
usethis::use_git(message = ":white_check_mark: Updated DESCRIPTION to add new packages")



### * 3.3. Load and check the package ------------------------------------------
# ---------------------------------------------------------------------------- #

# The following lines are run iteratively every time a new function is created within
# the package or every time a function is modified. This is the central part that loads,
# inspects (i.e. run the R CMD check) and test my functions and my whole package!


# ** 3.3.1. To load and document functions ----
devtools::load_all() # Now, all functions in the R folder are available!
# IMPORTANT NOTE: if you try to load (or check) your package while working on a new script to
# create new functions (i.e. having unfinished functions in a R file), make sure to put your
# unfinished code as 'comments' (e.g. Ctrl+Shift+C) or you'll get an error! Trust me!

devtools::document() # To create the functions' documentation in the "man" folder, and to
# update the NAMESPACE file of the package (that should NEVER be edited manually).
usethis::use_git(message = ":pencil: Documented new function(s)")
usethis::use_git(message = ":white_check_mark: Updated function(s)")


# ** 3.3.2. To test functions ----
# To test my functions, I could use the "testthat" package:
#usethis::use_testthat()

# Here, I don't want to do real tests because I know my functions work as I want them to.
# For other cases, I should look more closely into that (cf. lesson from N.Casajus FRB-Cesab
# on package building or other online resources)!
# NOTE: All tests files are stored in tests/testthat/ and their names must start with test-*


#  ** 3.3.3. To check the package integrity ----
devtools::check() # Ok!
# IMPORTANT NOTE: I had a lot of PROBLEMS in my first attempts to create a loading function
# because the dataset contained comments in French and English, with special characters and
# punctuation (;,:[] etc.) so R thought that my punctuation was field separators!
# Therefore, NEVER put comments in .csv or .txt files!



### * 3.4. To install and version the package ----------------------------------
# ---------------------------------------------------------------------------- #

# ** 3.4.1. Installing the package ----
devtools::install()
usethis::use_git(message = ":metal: Installed updated functions!")
# My package is now installed on my R so I can use its functions whenever I want.


# ** 3.4.2. Set package versions ----
usethis::use_version(which = "minor") # Automatically updates my package version.
usethis::use_news_md() # Creates a NEWS.md file, that I should maintain updated.
usethis::use_git(message = ":package: Release v0.1.0")


# ** 3.4.3. Add and update a README file ----
usethis::use_readme_rmd() # Creates a README.Rmd and adds it automatically to .Rbuildignore
# (and opens it). After manually editing the file, I need to compile it into a .md document
# (otherwise, GitHub and the CRAN won't be able to read and display it on their websites):
rmarkdown::render("README.Rmd")
# As render() also produces a .html file that is not useful here, I will ignore it:
usethis::use_build_ignore("README.html")
usethis::use_git_ignore("README.html")

usethis::use_git(message = ":pencil: Edited README")
# IMPORTANT NOTE: Each time you edit the README.Rmd you will have to update the .md
# with rmarkdown::render("README.Rmd") and, of course, you should also commit+push it
# to update GitHub!





# ---------------------------------------------- #
##### 4. Pipeline programming with {targets} #####
# ---------------------------------------------- #

# To ensure maximum reproducibility, help me organize my data processing, and avoid
# wasting too much time every time I have to go back in my code to change something,
# it may be a good idea to use a pipeline programming tool such as {targets} (or 'Drake').
# I won't explain here the advantages of such tools (Google is your friend), but I
# will explain how to use it and associate it while also developing a package to
# process my data (or I'll try).

# If I understand correctly, I think that {targets} and 'package development' can
# be used simultaneously as long as I don't mix the two frameworks up, i.e. they both
# live under the same roof (my project folder), but they don't directly work together:
#   - The 'package' part of my project is here to i) be the fundamental structure of
#     the project and ii) to create (documented) custom functions that I can use
#     wherever I want (in this project or any others). To work fine with {targets},
#     the 'package' part should simply ignore what {targets} is doing and only focus
#     on its own job!
#   - The {targets} part of my project is here to i) organize my data processing
#     workflow, ii) keep track of interconnected elements in the workflow, and iii)
#     only in case of changes (of the data or code), automatically re-run the parts
#     of the code/workflow that are impacted by the changes and leave the other parts
#     as they are! Targets uses the functions made available by the 'ppl.tits' package
#     but it should probably not consider the 'package development' as a goal (a target)
#     in itself, or at least I will prevent it from doing so for the sake of simplicity.
# So that means that the package building tools (e.g. R CMD Check) will have to
# ignore what {targets} is doing and, conversely, that the {targets} files will have
# to ignore what 'devtools' etc. are doing.
# So it means that I WILL HAVE TO manage and run both sides separately and iteratively!



### * 4.1. Setting-up the {targets} subproject ---------------------------------
# ---------------------------------------------------------------------------- #

# To create the {targets} master script file:
file.create("_targets.R") # Alternatively, I can use:
targets::use_targets() # It will create the _targets.R file with helpful comments in it.
# NOTE: This is the "master script", where every targets (goal) of the project is defined. It
# should thus be UPDATED every time I progress in my analysis workflow and make a new
# achievement (e.g. producing new results, new data tables, new figures, etc.).
# To know how to specify the workflow, please refer to this file, to section 4.2. below,
# and to online resources!

# To tell devtools to ignore what {targets} does:
usethis::use_build_ignore("_targets.R")
usethis::use_build_ignore("_targets/")
usethis::use_git_ignore("_targets/")

# IMPORTANT NOTE: as I am developing a package {ppl.tits} alongside the {targets} pipeline,
# I will not source my functions but call them directly.
# Consequently, {targets} will not be able to track changes in my functions unless I specify
# it in the {targets} global options (see _targets.R to know how, or refer to online resources
# such as https://books.ropensci.org/targets/packages.html#package-based-invalidation).



### * 4.2. Main {targets} functions --------------------------------------------
# ---------------------------------------------------------------------------- #

# To create a target:
targets::tar_target()
# To run the pipeline:
targets::tar_make()
# To load a built target into my R session:
targets::tar_load() # e.g. tar_load(my_target)
# To read and return a built target:
targets::tar_read()
# To show target inter-dependencies:
targets::tar_glimpse()
# To show the complete visual network representation of my workflow:
targets::tar_visnetwork(targets_only = FALSE) # This argument enables the display of global
# functions as well as genuine targets.



### * 4.3. Literate programming with RMarkdown and {targets} -------------------
# ---------------------------------------------------------------------------- #

# IMPORTANT NOTE: to ensure proper compatibility with {targets}, RMarkdown reports should be
# lightweight. That means they should mostly include text and their chunks of code should be short
# and quickly executed. In other words, R Markdown reports are just targets that document prior
# results. The bulk of the computation should have already happened upstream, and the most of the
# code chunks in the report itself should be terse calls to 'tar_read()' and 'tar_load()'.
# Of course, it is possible to create reports designed to NOT BE "targets"!
# The use of 'tar_read()' and 'tar_load()' allows us to run the report outside the pipeline. As
# long as the "_targets/" folder contains data on the required targets from a previous
# 'tar_make()', I can open the RStudio IDE, edit the report, and click the Knit button like I
# would for any other RMarkdown report.

# Note also that the report needs to be rendered by 'tar_make()'. If a report contains targets,
# but is not itself a target (created with 'tarchetypes::tar_render()') that is rendered with
# 'tar_make()', then I will not be able to knit it!
# That is because apparently, {targets}, R/RStudio and {knitr} don't work well together by
# default. For it to work, {knitr} needs to be able to locate the "_targets/" folder storing the
# targets that are called in the .Rmd code chunks. To do that, I need to change the working
# directory within the RStudio global options! If I change them with another method, I can run
# the code chunks but I cannot knit the .Rmd document! For an example, see the reports in the
# "ppl.tits" project.

### NOTE: Don't forget to fill the README file and to create a _make.R for others to be able
# to reproduce my work!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





################### *----------------------------------------* #################
########################## Main Git commits ####################################
# ---------------------------------------------------------------------------- #
usethis::use_git(message = ":boom: First exported results!")
usethis::use_git(message = ":metal: Created a new function")
usethis::use_git(message = ":zap: Ignoring something")
usethis::use_git(message = ":pencil: Documented a function or wrote something")
usethis::use_git(message = ":hammer: Ongoing programming!")
usethis::use_git(message = ":white_check_mark: Updated the {target} pipeline")
usethis::use_git(message = ":x: Problem detected!")
#system("git push") # Or using a CLI!
# Don't forget to push your commits once you're sure you made no mistakes.
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
################### *----------------------------------------* #################
