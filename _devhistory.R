# ---------------------------------------- #
# ---------------------------------------- #
##### DEV_HISTORY FOR ppl.tits #####
# ---------------------------------------- #
# ---------------------------------------- #


# ------------------------- #
##### 0. Project set up #####
# ------------------------- #

# ---------------------------------------------------------------------------- #
### * 0.1. Package project creation --------------------------------------------

# First, I created a package named "ppl.tits" (.Rproj) with the following command:
# usethis::create_package("d:/fmartin/Mes documents/projects/pubprivlands/analyses/ppl.tits")
# in which I specified the ABSOLUTE path to my package's folder (alternatively, I could have clicked the
# buttons in RStudio to create a package with devtools).
# IMPORTANT NOTE: to recreate this project (or reuse the code in another project), you do not need to re-run
# this line of code and recreate the package's folder.

# Then, I created a "dev history" file to keep track of everything I do in the project:
# usethis::edit_file("_devhistory.R")
# This is the document you are CURRENTLY reading!
# From now on, all command lines used to organize the project will be written in this file.
# IMPORTANT NOTE: in this case, I started a package project from scratch. If I want to start from a
# GitHub repository, the workflow is different but some steps will be required so I encourage my future
# self to carefully review the steps of this workflow anyway.



# ---------------------------------------------------------------------------- #
### * 0.2. Ignoring and keeping track of changes -------------------------------

# First thing first, we will tell R to ignore our _devhistory.R file as it's only for us!
# As this file is not part of a typical package structure, we need to tell R to ignore it when checking
# and installing the package:
usethis::use_build_ignore("_devhistory.R")

# Then, to keep track of all future changes and have a backup, I need to initiate a Git version control
# and link the 'ppl.tits' package with my GitHub account. In this precise case, I'm connecting
# my 'ppl.tits' project AFTER its creation, so I'm following the 'GitHub-last' procedure:
# STEP 1: Make sure Git is installed (e.g. by typing 'git status' in the R terminal), if not, install it!
# STEP 2: Enter your project and type 'git init' in the R Terminal, it will initiate a Git repository within
# your project:
system("git init")
# STEP 3: Verify your configuration:
system("git config --global user.name 'fanf'")
system("git config --global user.email 'francois.martin74190@gmail.com'")
# Of course, you need to personalize the user.name and email. If you want the config to be only true
# for the current project (and not global), remore the '--global' from the previous lines.
# STEP 4: If you've never done it before (on this computer), you also need to set-up SSH keys to be
# able to connect to Git and GitHub without supplying you username and password every time you to
# something. The full procedure is described here: https://help.github.com/articles/generating-ssh-keys
# But here goes:
system("ls -al ~/.ssh") # Check for existing SSH keys. If none is found, I must generate one.
system("ssh-keygen -t rsa -C 'francois.martin74190@gmail.com'") # Generates a new SSH key
# using the rsa algorithm (however, I was forced to do that directly in a Terminal (e.g. GitBash).
system("eval '$(ssh-agent -s)'") # Checks if the ssh-agent is launched (same --> GitBash). If it works,
# it should return something like "Agent pid 59552".
system("ssh-add ~/.ssh/id_rsa") # Adds the new SSH private key to the ssh-agent! (GitBash)
# Then you have to add the public key to your GitHub account settings :
# e.g. https://kbroman.org/github_tutorial/pages/first_time.html
system("ssh -T git@github.com") # To check if it works. If it does, it should answer with something
# like "Hi mrelnoob! You've successfully authenticated, but Github does not provide shell access".
# There is clearly a bug in RStudio (or R) that prevents me from doing all that from RStudio,
# but anyway...

# STEP 5: Make some changes in the project and make your first commit:
usethis::use_git(message = ":tada: Initial commit") # Then restart RStudio and the Git tab will appear
# on the to-right box of RStudio.
# STEP 6: Log in your GitHub account and create a new repository.
# STEP 7: Use the following command to associate your R project with the GitHub project:
system2("git remote add origin git@github.com:mrelnoob/ppl.tits") # Here also, personalize with your
# own account and project names! And here again, it does not work (so GitBash or Terminal).
# STEP 8: Finally, you can push the changes made to your local files to GitHub:
system2("git push -u origin master") # Same problem.
# Even using a Terminal (e.g. GitBash), you may receive an error message saying that the remote repository
# contains work that you do not have locally (...). It happens if you included files in your GitHub
# projects when you created it (e.g. a README or a LICENCE file). So theoretically, you need to
# always start with a "pull" before you push anything! If, like me, you forgot, you'll be
# in trouble and won't be able to pull. To force Git, you may use "git push -f origin master" (the
# -f means to "force" the push).
# IMPORTANT NOTE: because of these bugs, I can "commit" from RStudio but I cannot push, so I will always
# be forced to do it from the Terminal!



# ---------------------------------------------------------------------------- #
### * 0.3. Further basic configurations ----------------------------------------

# Before we go any further, we will edit some information about our package using the DESCRIPTION file
usethis::edit_file("DESCRIPTION")
usethis::use_mit_license(copyright_holder = "François-Marie Martin")# Open-source license
usethis::use_git(message = ":page_facing_up: Edit package metadata")

# To create the package's documentation:
usethis::use_package_doc() # It creates a dummy file in the R folder that should NOT be modified!
devtools::document() # Creates the documentation and the man folder (for "manual").
usethis::use_git(message = ":bulb: Update documentation")





# ----------------------------------- #
##### 1. Project structure set-up #####
# ----------------------------------- #

# ---------------------------------------------------------------------------- #
### * 1.1. Folders architecture ------------------------------------------------

# Before writing my first functions to import and clean my data, I need to add the said data files
# in the project folder. So I create a "mydata" folder and copy-paste my data in it (manually):
dir.create("mydata")
usethis::use_build_ignore("mydata") # Because it is not expected in a regular package root folder (if
# I don't ignore it, it will cause warnings in my package checks and all kind of craps. I just spent
# five hours to get rid of them, so trust me).

# I also need other folders to organize my data processing work (e.g. outputs):
dir.create("output")
dir.create("output/plots")
dir.create("output/tables")
dir.create("output/texts")
dir.create("output/spatial_data")

usethis::use_build_ignore("output/")
usethis::use_build_ignore("tables/")
usethis::use_build_ignore("texts/")
usethis::use_build_ignore("plots/")
usethis::use_build_ignore("spatial_data/")



# ---------------------------------------------------------------------------- #
### * 1.2. Creating scripts for custom functions -------------------------------

# To use pipes (i.e. %>%) everywhere in the package without explicitly loading the "magrittr" package:
usethis::use_pipe() # Automatically creates a pipe function (and associated R file in the R folder),
# while updating the DESCRIPTION file to tell R that it should import the "magrittr" package.

usethis::use_r("01_00_import_raw_data")
# NOTE: it automatically places the R file in the R folder (as it should be). The R folder should ONLY
# contain R scripts for functions and NOTHING ELSE! Yet, RStudio may sometimes put other things in it,
# so it is a good idea to go and see once in a while.
usethis::use_r("01_01_prepare_data")
usethis::use_r("01_02_graphab_analyses")





# --------------------------------------- #
##### 2. Package content and creation #####
# --------------------------------------- #

# ---------------------------------------------------------------------------- #
### * 2.1. Writing functions for the package -----------------------------------

# Now I can write my functions in the associated R files (= populating my R files) while keeping
# in mind that, for EACH function I create:
#  --> I need to insert a roxygen skeleton to write the documentation.
#  --> I should NOT SOURCE my functions (i.e. I should save my function's file but I should
#      not run the code in it and create "manually" the function you just wrote) to avoid
#      conflicts, see below!
# IMPORTANT NOTE: if a function uses functions from other packages, you need to tell R about
# it by updating the NAMESPACE file (for R to know what to load when it loads my package).
# It will be done automatically by devtools when we produce our documentation if we have
# previously listed the dependencies (packages) in the Roxygen2 header of the functions thanks
# to the tags (#' @import package OR #' @importFrom package function)! So we do this and add
# in our function's Roxygen2 header the required tags and packages, and we DO NOT FORGET to
# also add these dependencies in the "Imports" field of the DESCRIPTION:
usethis::use_package("readr")
usethis::use_package("here")
# REMINDER: The NAMESPACE controls what happens when our package is loaded but not when
# it's installed. This is the role of DESCRIPTION!



# ---------------------------------------------------------------------------- #
### * 2.2. Load and check the package ------------------------------------------

# The following lines are run iteratively every time a new function is created within
# the package or every time a function is modified. This is the central part that loads,
# inspects (i.e. run the R CMD check) and test my functions and my whole package!


# ** 2.2.1. To load and document functions ----
devtools::load_all() # Now, all functions in the R folder are available!
# IMPORTANT NOTE: if you try to load (or check) your package while working on a new
# script to create new functions (i.e. having unfinished functions in a R file),
# make sure to put your unfinished code as 'comment' (e.g. Ctrl+Shift+C) or you'll
# get an error! Trust me, I just lost an hour trying to fix this!

devtools::document() # To create the functions' documentation in the "man" folder, and to
# update the NAMESPACE file of the package (that should NEVER be edited manually).
usethis::use_git(message = ":white_check_mark: Documented new function(s)")
usethis::use_git(message = ":white_check_mark: Updated function(s)")


# ** 2.2.2. To test functions ----
# To test our functions, we could use the "testthat" package:
#usethis::use_testthat()

# Here, I don't want to do real tests because I know my function works as I want it to.
# For other functions and purposes, I should look more closely into that (cf. lesson from
# N.Casajus FRB-Cesab on package building, or other online resources)!
# NOTE: All tests files are stored in tests/testthat/ and their names must start with test-*


#  ** 2.2.3. To check the package integrity ----
devtools::check() # Ok!
# IMPORTANT NOTE: I had a lot of PROBLEMS in my first attempts to create a loading function
# because the dataset contained comments in French and English, with special characters
# and punctuation (;,:[] etc.) and R thought that my punctuation was field separators!
# So NEVER put comments in .csv or .txt files!



# ---------------------------------------------------------------------------- #
### * 2.3. To install and version the package ----------------------------------

# ** 2.3.1. Installing the package ----
devtools::install()
usethis::use_git(message = ":metal: Installed function: import_raw_tits_data()!")


# ** 2.3.2. Set package versions ----
usethis::use_version(which = "minor") # It automatically updates our package version.
usethis::use_news_md() # Creates a NEWS.md file, that I should maintain updated.
usethis::use_git(message = ":package: Release v0.1.0")
# Don't forget to push your commits, once in a while: go to the Terminal and type
# "git push" (for Windows) or use system("git push") (for Mac).


# ** 2.3.3. Add and update a README file ----
usethis::use_readme_rmd() # Creates a README.Rmd and adds it automatically to .Rbuildignore
# (and opens it). After manually editing the file, we need to compile it into a .md document
# (otherwise, GitHub and the CRAN won't be able to read it and display it on their websites):
rmarkdown::render("README.Rmd")
# As render() also produces a .html file that is not useful here, we will ignore it:
usethis::use_build_ignore("README.html")
usethis::use_git_ignore("README.html")

usethis::use_git(message = ":pencil: Edited README")
# IMPORTANT NOTE: Each time you edit the README.Rmd you will have to update the .md
# with rmarkdown::render("README.Rmd") and, of course, you should also commit+push it
# to update GitHub!





# ---------------------------------------------- #
##### 3. Pipeline programming with 'targets' #####
# ---------------------------------------------- #

# To ensure maximum reproducibility, help me organize my data processing, and avoid
# wasting too much time every time I have to go back in my code to change something,
# it may be a good idea to use a pipeline programming tool such as 'targets' (or 'Drake').
# I won't explain here the advantages of such tools (Google is your friend), but I
# will explain how to use it and associate it while also developing a package to
# process my data (or I'll try).

# If I understand correctly, I think that 'targets' and 'package development' can
# be used simultaneously as long as I don't mix the two frameworks up, i.e. they both
# live under the same roof (my project folder), but they don't directly work together:
#   - The 'package' part of my project is here to i) be the fundamental structure of
#     the project and ii) to create (documented) custom functions that I can use
#     wherever I want (in this project or any others). To work fine with 'targets',
#     the 'package' part should simply ignore what 'targets' is doing and only focus
#     on its own job!
#   - The 'targets' part of my project is here to i) organize my data processing
#     workflow, ii) keep track of interconnected elements in the workflow, and iii)
#     only in case of changes (of the data or code), automatically re-run the parts
#     of the code/workflow that are impacted by the changes and leave the other parts
#     as they are! Targets uses the functions made available by the 'ppl.tits' package
#     but it should probably not consider the 'package development' as a goal (a target)
#     in itself, or at least I will prevent it from doing so for the sake of simplicity.
# So that means that the package building tools (e.g. R CMD Check) will have to
# ignore what 'targets' is doing and, conversely, that the 'targets' files will have
# to ignore what 'devtools' etc. are doing.
# So it means that I WILL HAVE TO manage and run both sides separately and iteratively!



# ---------------------------------------------------------------------------- #
### * 3.1. Setting-up the 'targets' subproject ---------------------------------

# To create the 'targets' master script file:
file.create("_targets.R")
# This is the "master script", where every targets (goal) of the project is defined. It
# should thus be UPDATED every time I progress in my analysis workflow and make a new
# achievement (e.g. producing new results, new data tables, new figures, etc.).
# To know how to specify the workflow, please refer to this file, to section 3.2. below,
# and to online resources!

# To tell devtools to ignore what 'targets' does:
usethis::use_build_ignore("_targets.R")
usethis::use_build_ignore("_targets/")
usethis::use_git_ignore("_targets/")



# ---------------------------------------------------------------------------- #
### * 3.2. Main 'targets' functions --------------------------------------------

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
targets::tar_visnetwork()





# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
usethis::use_git(message = ":pencil: Edited function!")
usethis::use_git(message = ":zap: Saved updates!")
#system("git push")
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
# ---------------------------------------------------------------------------- #
