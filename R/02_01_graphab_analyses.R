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





# ----------------------------------------------- #
##### 0. Getting started with CLI and Graphab #####
# ----------------------------------------------- #

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
Ctrl+C # Stops an ongoing process.


# ** 0.1.2. To comment ----
rem # To comment (rem = remark). NOTE: this is a command, so it needs to be put first or after
# an operator if you comment AFTER a command (e.g. "cd & rem blabla"; and NOT "cd rem blabla"). So
# it sucks, that's because it is MS.DOS.



# ---------------------------------------------------------------------------- #
### * 0.2. Main commands and rules ---------------------------------------------

# ** 0.2.1. General rules, commands and options ----
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
-nosave # Global option to avoid saving results.
{value} # Curly brackets {} automatically convert metric distances into cost-distance in distance
# threshold parameters!
# IMPORTANT NOTE: the flash memory assigned to Graphab must be set through java using the -Xmx option,
# and so does the number of cores/processors (but with -proc). E.g.:
java -Xmx4g -jar graphab-2.8.1.jar -proc 3 (...) # To allow 4Go of RAM and 3 processors to Graphab.


# ** 0.2.2. Creating, loading and saving projects ----
--create # To create a new project (includes compulsory parameters, cf. CLI User Guide).
--project myproject.xml # To load an existing project.


# ** 0.2.3. Intervals and range of values ----
min:inc:max # To define an interval of values, you have to specify a min, an increment, and a
# maximum value (e.g. 0:2:10 will do 0, 2, 4, 6, 8, 10). Decimal increments are accepted
# (e.g. 0:0.5:3).
0,1,4,8,30,200 # Will create a list of values (no spaces).
# NOTE: if a command contains several intervals/values for different parameters, the command will
# be executed for each possible combination!


# ** 0.2.4. Command sequencing ----
# Graphab can be launched with several commands on the same line, except for -help and -metrics!
# The "--create" and "--project" commands can only be used once and should be the first command,
# after the prefix command that launches Graphab itself (e.g. java -jar graphab-2.8.jar --project...).



# ---------------------------------------------------------------------------- #
### * 0.3. Commands and parameters for running a classical Graphab project -----

# ** 0.3.1. Project creation ----
# Compulsory parameters (with usage example):
java -jar graphab-2.8.1.jar --create myproject.xml # To create the "myproject" project.
java -j(...)myproject.xml mylandcover.tif # To indicate the landcover raster file.
java -j(...)cover.tif habitat=5,7,13 # To define landcover codes 5, 7 and 13 as habitat.
# Optional parameters:
(...) nomerge # Does not merge contiguous patches with differing codes.
(...) nodata=0 # To define landcover code 0 as "habitat."no data".
(...) minarea=0.01 # Minimal area for a habitat patch in hectares (here, 0.01 ha = 100m2).
(...) maxsize=5 # Maximal area for a habitat patch (in m). This parameters thus cuts patches > maxsize
# on a grid of maxsize dimensions.
(...) dir # The path to save the project (default is the current folder).



# ** 0.3.2. Defining patches capacity ----
java -j(...)myproject.xml --capa # To define the habitat patch capacity.
# Optional parameters:
(...) area # Defines capacity as patch area in m2 (default setting).
(...) exp=2 # Modifies patches area with an exponent (e.g. 2 power term).
(...) 5=5 7=3 13=1 # To assign different weightings to habitat classes.

(...) file=my_capacity.csv # Defines capacity from an external file (must have the same ID as patches).
(...) id=id_column # To indicate which columns of the CSV contains the ID field.
(...) capa=capa_column # To indicate which columns of the CSV contains the capacity field.

(...) maxcost=100 codes=1,3 # Defines capacity as the number of pixels of codes 1 and 3 in a 100 distance
# radius around the habitat patch. The distance is a cost-distance, so for an euclidian link-set, that
# would be equivalent to meters with a cost of 1 for all classes!
# This command thus allows the use of a given link-set (cf. -uselinkset).


# ** 0.3.3. Meta-patch building ----
# Graphab also allows for meta-patch to be built (cf. Graphab documentation).


# ** 0.3.4. Creating a link-set ----
java -j(...)myproject.xml --linkset distance=euclid|cost # To create a link-set and indicate the type
# of distance to use. Note that `distance` must be the first parameter after `--linkset`!
# Cost-distance specific parameters:
(...) slope=coef # To weight costs by slopes (requires a loaded DEM).
--dem my_dem.tif # To include the `my_dem` raster file in the link-set computations (note that the
# geometry must perfectly match that of the landcover map).
(...) remcrosspath # To delete links that cross patches.
java -j(...)distance=cost (...) 1,2,3=1 4,5,6,7=2 # To assign cost to landcover classes. Increments
# are possible and will create several link-sets (cf. Graphab documentation).The (...) relates to
# additional parameters.
extcost=my_cost.tif # To define cost based on the `my_cost` raster file.

# General optional parameters:
(...) name=my_link5-150 # To name this link-set `my_link5-150` (better be meaningful). If missing, the
# names will be automatically generated based on cost definitions. No spaces and case sensitive!
(...) complete # To use complete topology instead of the planar one. Thresholding is possible.
(...) maxcost=400 # To assign a maximal distance of 400 (meters or cost-distance) for links computation.
# For more details or parameters, refer to Graphab documentation.


# ** 0.3.5. Using or removing a link-set ----
java -j(...)myproject.xml --uselinkset cost200,cost500 (...) # To apply (...) on the `cost200` and
# `cost500` link-sets. By default, all link-sets are selected!
java -j(...)myproject.xml --removelinkset cost500 # To delete the `cost500` link-set. All graphs that
# depend on this link-set will be removed as well.


# ** 0.3.6. Creating a graph ----
# Graph creation requires a link-set, so --graph requires --linkset or --uselinkset!
java -j(...)linkset cost200 --graph # To create a graph on the `cost200` link-set.
# Optional parameters:
(...) name=my_graph # To name the graph. Only for single graph creation. If missing, names will be
# automatically generated by concatenating the "comp_" prefix with the names of the used link-sets,
# except if the threshold parameter is used, in which case names will use the "thresh_" prefix instead!
(...) nointra # To deactivate intra-patch distances for metric computations.
(...) threshold={min:inc:max} # To define the maximal distance of the links included in the graph(s).
# REMINDER: increments are possible (will thus create several graphs) and {} will convert metric
# distances into cost-distances.
# For more details, please refer to the Graphab documentation.


# ** 0.3.7. Using or removing a graph ----
java -j(...)myproject.xml --usegraph thresh_1000-cost500 (...) # To apply (...) on the `thresh_1000
# -cost500` graph. By default, all graphs are selected!
java -j(...)myproject.xml --removegraph (...) # To delete all the selected graphs.



# ---------------------------------------------------------------------------- #
### * 0.4. Connectivity metric computations ------------------------------------

# ** 0.4.1. Global metrics ----
java -j(...)myproject.xml --gmetric metricacronym # Computes the `metricacronym` global metric on the
# selected graph.
# Optional parameters:
(...) resfile=myresults.txt # To save resulting computations in "myresults.txt". By default, the short
# name of the metric is used to name the file.
(...) maxcost=500 # To limit computations to a distance of 500 for a global metric using paths
# computations. Saves computation times but lowers accuracy.
(...) param1=[{]min:inc:max[}] # To set the metric parameters, if they exist.
--metrics # To get the list of all available metrics. Must be called alone, not after loading a project
# or something else.
# For metrics with parameters, several parameter values may be simultaneously tested. Their names will
# be concatenations of the metric name and parameter values.


# ** 0.4.2. Component or local metrics ----
# The same principles apply to component and local metrics:
java -j(...)myproject.xml --cmetric
java -j(...)myproject.xml --lmetric


# ** 0.4.3. Metrics interpolation ----
(...) --interp (...) # This command allows for spatial interpolation of local metrics. As it can be
# quite complicated, please refer to the Graphab documentation.



# ---------------------------------------------------------------------------- #
### * 0.5. Additional fancy commands -------------------------------------------

# Many other things can be modelled using Graphab (cf. documentation), including:
--corridor # To compute dispersal corridors around links (and export a shapefile) representing all
# possible paths to link two patches that have a cost-distance < to a specified value.
--cluster # To partition a graph. into clusters that maximise modularity.
--landmode # To assess the effects of landcover modifications (shapefile based).
--model # To compute SDM (spatial distribution models) for given point sets based on local metrics.
--delta # To add or remove patches or links and recompute global metrics. Many variations are allowed.
# Etc.





# --------------------------------------------------- #
##### 1. Connectivity models for PM (Parus major) #####
# --------------------------------------------------- #

# In the "tits reproduction study" of the PubPrivLands project, we tried to assess the usefulness of
# using a model-based habitat quality approximation for the computation of landscape connectivity
# metrics requiring "capacity" measures. For that purpose, we developed a Random Forest (RF) model to
# predict the clutch size Parus major (PM) couples based on local environmental predictors.
# To be able to assess whether this approach yields more accurate results than the regular one
# consisting in using habitat patch surface area as proxy of patches capacity, we have to use both
# approaches and compare the results.



# ---------------------------------------------------------------------------- #
### * 1.1. Project with RF-based capacity --------------------------------------
# ** 1.1.1. Loading the project ----

# In this case, we will not start the Graphab project from scratch as it has already been created by
# Gilles Vuidel because he needed to create the habitat patches to be able to extract their pixels
# and compute the weightings for pixels and patches (i.e. their "capacity") based on the RF model
# exported by the `ppl.tits::local_quality_model()` function.
# Consequently, here, I only need to manually paste the mentioned Graphab project and associated files
# in the graphab/ folder of the {ppl.tits} package.
# I can then open the Graphab project (named `pmrf_gpb.xml`) using:
java -jar graphab-2.8.1.jar --project .\pmrf_gpb\pmrf_gpb.xml


# ** 1.1.2. Computing relevant link-sets ----
# Survey-based ranking and costs:
java -jar graphab-2.8.1.jar --project .\pmrf_gpb\pmrf_gpb.xml --linkset distance=cost
name=pmrf_survcost maxcost=1500 31,32,33,34,35,36=1 51=75 11=100 25,62=200 1,2=250 81=350 71=500 61=750
12=1000
# The tested resistance values are stored in `ppl_survey_results_simplified.xlsx`.
# Literature-based ranking and costs:
java -jar graphab-2.8.1.jar --project .\pmrf_gpb\pmrf_gpb.xml --linkset distance=cost
name=pmrf_littcost maxcost=1500 31,32,33,34,35,36=1 51=150 11,81=350 25=20 62=200 1,2=250 71=500 61=750
12=1000


# ** 1.1.3. Creating pruned graphs ----
java -jar graphab-2.8.1.jar --project .\pmrf_gpb\pmrf_gpb.xml --graph threshold=750
# This value was chosen because it is half the maximum value answered in our survey as well as almost
# twice the second highest value answered, thus representing an acceptable compromise for a maximal value,
# especially considering that 3 respondents gave maximal distances <= 50.


# ** 1.1.4. Computing local or delta metrics ----
java -jar graphab-2.8.1.jar --project .\pmrf_gpb\pmrf_gpb.xml --lmetric F d={15,60,140,270}
p=0.5 beta=0,1,2
java -jar graphab-2.8.1.jar --project .\pmrf_gpb\pmrf_gpb.xml --lmetric BC d={15,60,140,270}
p=0.5 beta=0,1,2
# Among our 6 respondents, there was a clear divide in estimated gap-crossing distances with one group
# that always gave very conservative values (median gap-crossing values ranging from 5 to 25m) while the
# other half gave much higher values (ranging from 100 to 500m). It was thus hard to choose a single
# central parameter to summarise PM gap-crossing abilities.
# Consequently, I chose to test for various distance estimates:
#   - 15 = the mean value for 'median gap-crossing distance' estimated by the "conservative half" of the
# respondents.
#   - 60 = roughly the median value estimated by all respondents.
#   - 140 = roughly the mean value estimated by all respondents.
#   - 270 = roughly the mean value for 'median gap-crossing distance' estimated by the "liberal half" of
# the respondents.

# For the beta parameter, three values were tested representing three hypotheses:
#   - beta=0 - Tits disregard patches capacity when deciding whether to cross a gap.
#   - beta=1 - Implying that the weight of patches' capacity acts linearly with the weight of distances
# (i.e. local patch quality affects proportionately the ability or willingness or tits to cross a gap).
#   - beta=2 - Implying that the weight of patches' capacity is squared compared to the weight of
# distances (i.e. birds are more likely to cross a given distance if patches are of good quality).



# ---------------------------------------------------------------------------- #
### * 1.2. Project with area-based capacity --------------------------------------
# ** 1.2.1. Creating the project ----

# In this case, we have to start the Graphab project from scratch:
java -jar graphab-2.8.1.jar --create titscaparea_gpb lc_dijon_v8_simple.tif habitat=31,32,33,34,35,36
nodata=0 minarea=0.001 maxsize=200


# ** 1.2.2. Capacity, linkset and graphs ----
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --capa

# Survey-based ranking and costs:
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --linkset distance=cost
name=pmarea_survcost maxcost=1500 31,32,33,34,35,36=1 51=75 11=100 25,62=200 1,2=250 81=350 71=500 61=750
12=1000
# Literature-based ranking and costs:
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --linkset distance=cost
name=pmarea_littcost maxcost=1500 31,32,33,34,35,36=1 51=150 11,81=350 25=20 62=200 1,2=250 71=500 61=750
12=1000

# Pruned graphs:
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --graph threshold=750


# ** 1.2.3. Computing local metrics ----
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --lmetric F d={15,60,140,270}
p=0.5 beta=0,1,2
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --lmetric BC d={15,60,140,270}
p=0.5 beta=0,1,2





# ----------------------------------------------------------- #
##### 2. Connectivity models for CC (Cyanistes caeruleus) #####
# ----------------------------------------------------------- #

# As we did not build a RF model for CC, we only need to compute metrics based on the "area" approximation
# of patch capacity. As such, we can use the same project as before.



# ---------------------------------------------------------------------------- #
### * 2.1. Project with area-based capacity --------------------------------------
# ** 2.1.1. Loading and computing relevant link-sets ----
# Survey-based ranking and costs:
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --linkset distance=cost
name=cc_survcost maxcost=1500 31,32,33,34,35,36=1 11=100 51=150 62=250 25=350 1,2,71=500 81=600 61=750
12=1000
# Literature-based ranking and costs:
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --linkset distance=cost
name=cc_littcost maxcost=1500 31,32,33,34,35,36=1 11=100 51=300 62=250 25=50 1,2,71=500 81=600 61=750
12=1000


# ** 2.1.2. Creating pruned graphs ----
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --graph threshold=500


# ** 2.1.3. Computing local metrics ----
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --lmetric F d={10,30,130,250}
p=0.5 beta=0,1,2
java -jar graphab-2.8.1.jar --project .\titscaparea_gpb\titscaparea_gpb.xml --lmetric BC d={10,30,130,250}
p=0.5 beta=0,1,2
# Same rationale as for PM (but with slightly lower values for CC).

####### SCRIPT MUST BE UPDATED #########
####### SCRIPT MUST BE UPDATED #########
####### SCRIPT MUST BE UPDATED #########
####### SCRIPT MUST BE UPDATED #########





# ---------------------------------------------- #
##### 3. Exploration of connectivity metrics #####
# ---------------------------------------------- #

# ---------------------------------------------------------------------------- #
### * 3.1. Exploration functions creation --------------------------------------

# As I'm unable to install my {jk.dusz.tarping} package that contains some useful functions for data
# exploration, I will recreate the said functions here (ultimately, I should 1) fix the bug in the
# {jk.dusz.tarping} package that prevents installation [see my "update script"] and 2) transfer the
# useful functions in my truly exported utility package {airpoumpoum}!):

# Outlier detection with Cleveland dotplots:
uni.dotplots <- function(dataset, MAR=c(3,2,0.5,1.5), CEX.LAB = 1.2, FONT.LAB = 2, BTY = "n",
                         FG = "gray35", COL.AXIS = "gray35", COL.LAB = "gray20", CEX.PAR = 0.6,
                         TCL = -0.3, MGP = c(1.7, 0.6, 0.1), OMA = c(1, 0, 1, 0), LAB = c(5, 10, 7),
                         COL.PCH = "lightcoral", PCH = 19, COL.GRID = "lavender", NX = 5, NY = 9, LTY = 6,
                         ...){
  num.data <- dataset[, sapply(dataset, is.numeric)]
  nam <- names(num.data)
  ncol.data <- ncol(num.data)
  ncol.adjust <- ceiling(x = ncol.data/4) # Round to the next integer (e.g. ceiling(x = 7.12) returns 8)!
  num.data <- as.matrix(num.data)

  graphics::par(mfrow= c (ncol.adjust,4), mar=MAR, cex.lab = CEX.LAB, font.lab=FONT.LAB, bty = BTY, fg = FG,
                col.axis = COL.AXIS, col.lab = COL.LAB, cex = CEX.PAR, tcl = TCL,
                mgp = MGP, oma = OMA, lab = LAB)
  for (i in c(1:ncol(num.data))) {
    graphics::plot(x = num.data[,i], y = 1:length(num.data[,i]), type = "p", xlab = nam[i], ylab = "",
                   col = COL.PCH, pch = PCH, panel.first = {
                     grid(col=COL.GRID,nx = NX,ny = NY, lty = LTY)
                   }, ...) }
  # Here, the argument panel.first={} is used to draw the grid first, so behind the points!
}

# Distribution observation with multi-histograms:
uni.histograms <- function(dataset, MAR=c(3,2,0.5,1.5), CEX.LAB = 1.2, FONT.LAB = 2, BTY = "n",
                           FG = "gray35", COL.AXIS = "gray35", COL.LAB = "gray20", CEX.PAR = 0.6,
                           TCL = -0.3, MGP = c(1.7, 0.6, 0.1), OMA = c(1, 0, 1, 0), LAB = c(5, 10, 7),
                           BREAKS = 10, COL = "moccasin", BORDER = "white"){
  num.data <- dataset[, sapply(dataset, is.numeric)]
  nam <- names(num.data)
  ncol.data <- ncol(num.data)
  ncol.adjust <- ceiling(x = ncol.data/4) # Round to the next integer (e.g. ceiling(x = 7.12) returns 8)!
  num.data <- as.matrix(num.data)

  graphics::par(mfrow= c (ncol.adjust,4), mar=MAR, cex.lab = CEX.LAB, font.lab=FONT.LAB, bty = BTY, fg = FG,
                col.axis = COL.AXIS, col.lab = COL.LAB, cex = CEX.PAR, tcl = TCL,
                mgp = MGP, oma = OMA, lab = LAB)
  for (i in c(1:ncol(num.data))) {
    graphics::hist(num.data[,i], breaks = BREAKS, col = COL, border = BORDER,
                   main = "", xlab = nam[i], ylab = "")
  }
}





# ---------------------------------------------------------------------------- #
### * 3.2. F-metrics for PM ----------------------------------------------------
# ** 3.1.1. Loading and joining datasets ----

tits_clean <- ppl.tits::ntits_clean
tits_clean %>% dplyr::filter(species == "PM") %>%
  dplyr::select(id_nestbox, site, coord_x, coord_y, breeding_window,
                clutch_size, brood_size, fledgling_nb, mass, tarsus_length, wing_length,
                father_cond, mother_cond, min_t_between, lsource_vs150_m, noise_m,
                built_area, open_area, woody_area, woodyveg_volume, age_class, strata_div) -> pm

fmetrics <- readr::read_csv2(here::here("input_raw_data", "cmetrics_pm.csv"),
                          col_names = TRUE, na = "NA",
                          col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                  id_patch = readr::col_factor()))
fmetrics %>% dplyr::select(-id_patch, -cost_to_patch, -perim) %>%
  dplyr::inner_join(pm, fmetrics, by = "id_nestbox") -> pm_f # Not clear why left_join worked for
# ppl.tits::tdata_upD_rawiv() and not here (so I had to use inner_join())!!!





# ** 3.1.3. Univariate metric exploration ----

