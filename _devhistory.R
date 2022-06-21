# ---------------------------------------- #
# ---------------------------------------- #
##### DEVELOPMENT HISTORY FOR ppl.tits #####
# ---------------------------------------- #
# ---------------------------------------- #

# ________________________
##### Project set up #####

### First, I created a package named "ppl.tits" (.Rproj) with the following command:
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



### First thing first, we will tell R to ignore our _devhistory.R file as it's only for us!
# As this file is not part of a typical package structure, we need to tell R to ignore it when checking
# and installing the package:
usethis::use_build_ignore("_devhistory.R")

### Then, to keep track of all future changes and have a backup, I need to initiate a Git version control
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
system("git push")

usethis::use_git(message = ":boom: Saved updates!")
system("git status")
