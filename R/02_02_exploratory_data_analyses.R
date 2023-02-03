# ----------------------------------------------- #
##### Functions for exploratory data analyses #####
# ----------------------------------------------- #

# The functions of this R file are meant for all exploratory data analyses (EDA), whether for
# connectivity metrics evaluation or for the construction of statistical models.




### ____________________________________________________________________
#' Univariate Cleveland dotplots
#'
#' @description The `uni.dotplots` function draws, within a single panel, an independent Cleveland dotplot
#' (i.e. plotting value against rank) for each numeric (continuous or discrete) variable in a given
#' dataset. It is particularly useful for data exploration (e.g. Zuur \emph{et al.}, 2010). For instance,
#' to simultaneously observe the distributions of all numeric variables or \strong{to detect their
#' univariate outliers}.
#'
#' @details The `uni.dotplots` function only modifies the graphical parameters of the
#' \code{\link[graphics:plot.default]{plot}} function in the `{graphics}` package to match some predefined
#' preferences. Therefore, default values of `uni.dotplots` create nice looking dotplots but retain
#' some aspects of the original `plot` function. These aspects can however be changed as in
#' \code{\link[graphics:plot.default]{plot}}. \cr
#' On the other hand, panel parameters are internally controlled using `par`. However, to avoid unforeseen
#' conflicts with other internal parameters, it is not possible to tune panel parameters as we would
#' do with `par`. Instead, parametrization is only possible with the given subset of arguments (see below).
#'
#' @note To avoid \emph{recursive argument errors}, internal arguments should be called using upper case
#' letters (e.g. CEX.LAB = 0.9) whereas other arguments from the `plot` function should be called with
#' their normal case writing (e.g. sub = "My subtitle")!
#'
#' @param dataset The input dataset containing all variables to be plotted (must be a `data.frame` with
#' at least 2 variables). It may contain all kinds of columns, the `uni.dotplots` function will
#' automatically detect and plot numeric variables (columns).
#' @param MAR A numerical vector of the form `c(bottom, left, top, right)` which gives the number of lines
#' of margin to be specified on the four sides of the plot. The default is `c(0.5,4.1,1.1,1.5)`.
#' @param CEX.LAB The magnification to be used for x and y labels relative to the current setting
#' of `CEX.PAR`.
#' @param FONT.LAB The font to be used for x and y labels.
#' @param BTY A character string which determined the type of box which is drawn about plots. If `BTY` is
#' one of "o", "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper
#' case letter. A value of "n" suppresses the box (the default).
#' @param FG The color to be used for the foreground of plots. This is the default color used for things
#' like axes and boxes around plots (defaults to "gray35").
#' @param COL.AXIS The color to be used for axis annotation. Defaults to "gray35".
#' @param COL.LAB The color to be used for x and y labels. Defaults to "gray20".
#' @param CEX.PAR A numerical value giving the amount by which plotting text and symbols should be
#' magnified relative to the default (for `par`, the panel manager). This starts as 1 when a device
#' is opened, and is reset when the layout is changed, e.g. by setting `mfrow`. Defaults to 0.8.
#' @param TCL The length of tick marks as a fraction of the height of a line of text. The default
#' value is -0.3.
#' @param MGP The margin line (in `mex` units) for the axis title, axis labels and axis line.
#' Note that `mgp[1]` affects title whereas `mgp[2:3]` affect axis. The default is c(2.4, 0.6, 0).
#' @param OMA A vector of the form `c(bottom, left, top, right)` giving the size of the outer margins
#' in lines of text.
#' @param LAB A numerical vector of the form `c(x, y, len)` which modifies the default way that axes
#' are annotated. The values of `x` and `y` give the (approximate) number of tickmarks on the x and y
#' axes and len specifies the label length. The default is `c(5, 5, 7)`. Note that this only affects
#' the way the parameters xaxp and yaxp are set when the user coordinate system is set up, and is
#' not consulted when axes are drawn. `len` is unimplemented in `R`.
#' @param COL.PCH The color to be used for points. Default is "lightcoral".
#' @param PCH The type of points to be drawn. Default is 19. See \code{\link[graphics:points]{points}}
#' for possible values and their interpretation.
#' @param COL.GRID The color of the background grid. Default is "lavender".
#' @param NX The number of lines for the grid in x. Default value is 5.
#' @param NY The number of lines for the grid in Y. Default value is 9.
#' @param LTY The type of lines to be drawn in the background grid. Default value is 6.
#' @param ... Any other parameter that can be incorporated in \code{\link[graphics:plot.default]{plot}}.
#'
#' @return A panel of univariate dotplots.
#' @export
#' @import graphics
#'
#' @examples
#' data("mtcars")
#' uni.dotplots(dataset = mtcars, COL.GRID = "lightblue", LTY = 1, NX = 10, NY = 20)
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





### ____________________________________________________________________
#' Histogram Panel
#'
#' @description The `uni.histograms` function draws, within a single panel, an independent histogram
#' or each numeric (continuous or discrete) variable in a given dataset. It is particularly useful
#' for data exploration (e.g. Zuur \emph{et al.}, 2010). For instance, to simultaneously observe
#' the distributions of all numeric variables and determine which one will require transformation.
#'
#' @param dataset The input dataset containing all variables to be plotted (must be a `data.frame` with
#' at least 2 variables). It may contain all kinds of columns, the `uni.histograms` function will
#' automatically detect and plot numeric variables (columns).
#' @param MAR A numerical vector of the form `c(bottom, left, top, right)` which gives the number of lines
#' of margin to be specified on the four sides of the plot. The default is `c(0.5,4.1,1.1,1.5)`.
#' @param CEX.LAB The magnification to be used for x and y labels relative to the current setting
#' of `CEX.PAR`.
#' @param FONT.LAB The font to be used for x and y labels.
#' @param BTY A character string which determined the type of box which is drawn about plots. If `BTY` is
#' one of "o", "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper
#' case letter. A value of "n" suppresses the box (the default).
#' @param FG The color to be used for the foreground of plots. This is the default color used for things
#' like axes and boxes around plots (defaults to "gray35").
#' @param COL.AXIS The color to be used for axis annotation. Defaults to "gray35".
#' @param COL.LAB The color to be used for x and y labels. Defaults to "gray20".
#' @param CEX.PAR A numerical value giving the amount by which plotting text and symbols should be
#' magnified relative to the default (for `par`, the panel manager). This starts as 1 when a device
#' is opened, and is reset when the layout is changed, e.g. by setting `mfrow`. Defaults to 0.8.
#' @param TCL The length of tick marks as a fraction of the height of a line of text. The default
#' value is -0.3.
#' @param MGP The margin line (in `mex` units) for the axis title, axis labels and axis line.
#' Note that `mgp[1]` affects title whereas `mgp[2:3]` affect axis. The default is c(2.4, 0.6, 0).
#' @param OMA A vector of the form `c(bottom, left, top, right)` giving the size of the outer margins
#' in lines of text.
#' @param LAB A numerical vector of the form `c(x, y, len)` which modifies the default way that axes
#' are annotated. The values of `x` and `y` give the (approximate) number of tickmarks on the x and y
#' axes and len specifies the label length. The default is `c(5, 5, 7)`. Note that this only affects
#' the way the parameters xaxp and yaxp are set when the user coordinate system is set up, and is
#' not consulted when axes are drawn. `len` is unimplemented in `R`.
#' @param BREAKS One of:
#' * a vector giving the breakpoints between histogram cells,
#' * a function to compute the vector of breakpoints,
#' * a single number giving the number of cells for the histogram,
#' * a character string naming an algorithm to compute the number of cells (see
#' \code{\link[graphics:hist]{hist}}),
#' * a function to compute the number of cells.
#'
#' In the last three cases the number is a suggestion only; as the breakpoints will be set to pretty
#' values, the number is limited to 1e6 (with a warning if it was larger). If breaks is a function,
#' the x vector is supplied to it as the only argument (and the number of breaks is only limited by
#' the amount of available memory).
#' @param COL The color of the bar of the histograms (bins).
#' @param BORDER The color of the border of the bars.
#'
#' @return A panel of histograms.
#' @import graphics
#' @export
#'
#' @examples
#' uni.histograms(dataset = iris[,1:4])
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








########################## ************************************************* ###############################
####### SUPER IMPORTANT NOTE #######
# To finish converting all that into functions, I'll have to regenerate the data (used for the report) and
# create a META-function that calls all these functions and exports "targetable" outputs, for both PM
# and CC!!!!!



##### DATA IMPORT #####
##### Data import
# _______________

library(magrittr)
.pardefault <- par()

### Data import and preparation_________________________________________________#
# Import and binding:
tits_clean <- ppl.tits::ntits_clean
tits_clean %>% dplyr::filter(dist == 150) -> tits_clean # Only selects the 150m-based dataset!

manag <- readr::read_csv2(here::here("input_raw_data", "veg_manag_factor.csv"),
                                col_names = TRUE, na = "NA",
                                col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                        manag_intensity = readr::col_factor(
                                                          levels = c("0", "1", "2"),
                                                          include_na = FALSE)))
tits_clean <- dplyr::inner_join(tits_clean, manag, by = "id_nestbox")

cmetrics_pm <- readr::read_csv2(here::here("input_raw_data", "cmetrics_pm.csv"),
                                col_names = TRUE, na = "NA",
                                col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                        id_patch = readr::col_factor()))
cmetrics_pm %>% dplyr::select(-cost_to_patch, -id_patch, -patch_area, -perim,
                              -pmRr_d1_capa2, -pmRr_d2_capa2, -pmRr_d3_capa2) %>%
  dplyr::inner_join(tits_clean, cmetrics_pm, by = "id_nestbox", multiple = "all") -> ttt # Only selects
# some metrics!

cmetrics_cc <- readr::read_csv2(here::here("input_raw_data", "cmetrics_cc.csv"),
                                col_names = TRUE, na = "NA",
                                col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                        id_patch = readr::col_factor()))
cmetrics_cc %>% dplyr::select(-cost_to_patch, -ccRr_d1_capa2, -ccRr_d2_capa2, -ccRr_d3_capa2) %>%
  dplyr::inner_join(ttt, cmetrics_cc, by = "id_nestbox", multiple = "all") -> ttt # Only selects some metrics!

tits_clean$age_num <- as.numeric(as.character(tits_clean$age_class))
tits_clean$strata_num <- as.numeric(as.character(tits_clean$strata_div))
tits_clean$manag_num <- as.numeric(as.character(tits_clean$manag_intensity))
tits_clean %>%  dplyr::relocate(manag_intensity, .after = age_class) -> tits_clean
tits_clean %>%  dplyr::relocate(manag_num, .after = manag_intensity) -> tits_clean
tits_clean %>%  dplyr::relocate(age_num, .after = age_class) %>%
  dplyr::rename(traffic = trafic) -> tits_clean

ntits <- cbind(tits_clean, ttt[,2:26]) # To select all patch related variables!
ntits %>% dplyr::select(-dist, -breeding_window, -father_id, -mother_id, -mean_winter_t, -sd_winter_t,
                        -lsource_vs150_m, -lsource_vs150_iq, -soft_manag_area) -> ntits
rm(ttt, cmetrics_pm, cmetrics_cc, manag, tits_clean)
ntits <- tibble::as_tibble(ntits)

# Weighting of woodyveg_volume with strata_num:
ntits %>% dplyr::mutate(strata_w = dplyr::case_when(
  strata_div == "0" ~ 0.25,
  strata_div == "1" ~ 0.5,
  strata_div == "2" ~ 0.6,
  strata_div == "3" ~ 0.75,
  strata_div == "4" ~ 0.9)) %>%
  dplyr::mutate(woodyveg_vw = woodyveg_volume*strata_w) %>%
  dplyr::select(-strata_div, -strata_num, -strata_w) %>%
  dplyr::relocate(woodyveg_vw, .after = woodyveg_sd) -> ntits





##### DATA REDUCTION #####
##### Data reduction
# __________________

### For variables related to vegetation characteristics_________________________#
# Does not yield meaningful results.

### For variables related to landscape composition______________________________#
ntits %>% dplyr::select(id_nestbox, site, traffic, built_area, build_volume,
                        build_sd, open_area) -> xxx

# Normed-PCA:
res.pca <- FactoMineR::PCA(X = xxx[, 3:ncol(xxx)], scale.unit = TRUE, graph = FALSE)
# To plot results:
landscape.varplot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
landscape.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
#gridExtra::grid.arrange(landscape.varplot, landscape.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (59%)
# of my three variables, we can use the coordinates of observations on this axis as a synthetic variable:
zzz <- res.pca$ind$coord[,1]
ntits$urban_intensity <- zzz # This variable opposes nestboxes located in very dense urban areas with a
# lot of traffic to those located in greener contexts.



### For variables related to light pollution____________________________________#
ntits %>% dplyr::select(id_nestbox, site,
                        lsource_0_m, lsource_10_m, lflux_0_m, lflux_10_m) -> xxx

# Normed-PCA:
res.pca <- FactoMineR::PCA(X = xxx[, 3:ncol(xxx)], scale.unit = TRUE, graph = FALSE)
# To plot results:
lightpol.varplot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
lightpol.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
#gridExtra::grid.arrange(lightpol.varplot, lightpol.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (76%)
# of my four variables, we can use the coordinates of observations on this axis as a synthetic variable:
zzz <- res.pca$ind$coord[,1]
ntits$light_pollution <- zzz # This variable opposes nestboxes located in areas experiencing strong light
# pollution against more preserved areas.





##### DATASETS FORMATTING #####
##### For PM and CC
# _________________

ntits %>% dplyr::filter(species == "PM") %>%
  dplyr::select(id_nestbox, site, coord_x, coord_y, year, breeding_window, laying_date, flight_date,
                clutch_size, brood_size, fledgling_nb, mass, tarsus_length, wing_length,
                pmF_d1_beta0, pmF_d2_beta0, pmF_d3_beta0, pmF_d1_beta1, pmF_d2_beta1, pmF_d3_beta1,
                woodyveg_volume, woodyveg_vw, woodyveg_sd, woody_area,
                herbaceous_area, age_class, manag_intensity,
                light_pollution, noise_m, noise_iq, traffic, built_area, open_area, urban_intensity,
                cumdd_30, min_t_before, min_t_between,
                father_cond, mother_cond) -> pm
ntits %>% dplyr::filter(species == "CC") %>%
  dplyr::select(id_nestbox, site, coord_x, coord_y, year, breeding_window, laying_date, flight_date,
                clutch_size, brood_size, fledgling_nb, mass, tarsus_length, wing_length,
                ccF_d10_beta0, ccF_d30_beta0, ccF_d130_beta0, ccF_d10_beta1, ccF_d30_beta1, ccF_d130_beta1,
                woodyveg_volume, woodyveg_vw, woodyveg_sd, woody_area,
                herbaceous_area, age_class, manag_intensity,
                light_pollution, noise_m, noise_iq, traffic, built_area, open_area, urban_intensity,
                cumdd_30, min_t_before, min_t_between) -> cc



##### For all tit nestlings (ntits)
# _________________________________
ntits %>% dplyr::mutate(
  F_metric_d1b0 = dplyr::case_when(
    species == "PM" ~ pmF_d1_beta0, species == "CC" ~ ccF_d1_beta0),
  F_metric_d2b0 = dplyr::case_when(
    species == "PM" ~ pmF_d2_beta0, species == "CC" ~ ccF_d2_beta0),
  F_metric_d3b0 = dplyr::case_when(
    species == "PM" ~ pmF_d3_beta0, species == "CC" ~ ccF_d3_beta0),
  F_metric_d1b1 = dplyr::case_when(
    species == "PM" ~ pmF_d1_beta1, species == "CC" ~ ccF_d1_beta1),
  F_metric_d2b1 = dplyr::case_when(
    species == "PM" ~ pmF_d2_beta1, species == "CC" ~ ccF_d2_beta1),
  F_metric_d3b1 = dplyr::case_when(
    species == "PM" ~ pmF_d3_beta1, species == "CC" ~ ccF_d3_beta1),
  Rr_metric_d1c1 = dplyr::case_when(
    species == "PM" ~ pmRr_d1_capa1, species == "CC" ~ ccRr_d1_capa1),
  Rr_metric_d2c1 = dplyr::case_when(
    species == "PM" ~ pmRr_d2_capa1, species == "CC" ~ ccRr_d2_capa1),
  Rr_metric_d3c1 = dplyr::case_when(
    species == "PM" ~ pmRr_d3_capa1, species == "CC" ~ ccRr_d3_capa1),
  Dr_metric_c1 = dplyr::case_when(
    species == "PM" ~ pmDr_capa1, species == "CC" ~ ccDr_capa1),
  Dr_metric_c2 = dplyr::case_when(
    species == "PM" ~ pmDr_capa2, species == "CC" ~ ccDr_capa2),
  ) %>%
  dplyr::select(id_nestbox, id_patch, site, coord_x, coord_y, year, species, laying_date, flight_date,
                clutch_size, brood_size, fledgling_nb, mass, tarsus_length, wing_length,
                F_metric_d1b0, F_metric_d2b0, F_metric_d3b0, F_metric_d1b1, F_metric_d2b1, F_metric_d3b1,
                Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
                woodyveg_volume, woodyveg_vw, woodyveg_sd, woody_area, patch_area, perim,
                herbaceous_area, manag_intensity,
                light_pollution, noise_m, noise_iq,
                traffic, built_area, build_volume, build_sd, open_area, water_area, urban_intensity,
                cumdd_30, cumdd_60, cumdd_between, min_t_before, min_t_between) %>%
  dplyr::rename(patch_perim = perim)-> ntits_reduced
rm(res.pca, xxx, zzz)





########################## ************************************************* ###############################
##### UNIVARIATE OUTLIERS #####
##### Looking for outliers
# ________________________

uni.boxplots <- function(dataset, MAR=c(0.5,4.1,1.1,1.5), CEX.LAB=1, FONT.LAB=2, BTY = "n", FG = "gray35",
                         COL.AXIS = "gray35", COL.LAB = "gray20", CEX.PAR = 0.8, TCL = -0.3,
                         MGP = c(2.4, 0.6, 0), OMA = c(1, 0, 0, 0),
                         TYPE = "n", BORDER = "lightcoral", COL = "moccasin",  LTY = 1, STAPLEWEX = 0,
                         WHISKLWD = 2, BOXWEX = 0.7, BOXLWD = 0.1, MEDLWD = 2.6, PCH = 19, ...){
  num.data <- dataset[, sapply(dataset, is.numeric)]
  nam <- names(num.data)
  ncol.data <- ncol(num.data)
  ncol.adjust <- ceiling(x = ncol.data/4) # Round to the next integer (e.g. ceiling(x = 7.12) returns 8)!

  graphics::par(mfrow= c(ncol.adjust,4), mar=MAR, cex.lab=CEX.LAB, font.lab=FONT.LAB, bty=BTY, fg=FG,
                col.axis=COL.AXIS, col.lab=COL.LAB, cex=CEX.PAR, tcl=TCL, mgp=MGP, oma=OMA)
  for (i in c(1:ncol.data)) {
    graphics::boxplot(num.data[,i],ylab =(nam[i]), type=TYPE, border=BORDER, col = COL,
                      lty=LTY, staplewex=STAPLEWEX, whisklwd=WHISKLWD, boxwex=BOXWEX, boxlwd=BOXLWD,
                      medlwd=MEDLWD, pch=PCH, cex=0.7, ...) }
}

### For PM______________________________________________________________________#

uni.boxplots(pm[,15:ncol(pm)]) # I CANNOT export them as object (-> NULL), I don't know why. The problem is
# not that it's a custom function (it doesn't work for airpoumpoum::superplot() either)! Nor that it's not
# a list! Then why (linked to par())??? Ask SO?
ppl.tits::uni.dotplots(pm[,15:ncol(pm)])
# We can see that:
# - There are extreme values for all F-metrics, especially the "beta1" (but we already knew that).
# - There are extreme values for the "woodyveg_volume" variables, yet the weighting by "strata_div" slightly
#   alleviate the problem, and the "woodyveg_sd" variable has a rather nice distribution. Besides, "woody_area" also
#   has a nicer distribution than the volume ones.
# - The "pollution" variables are very slightly skewed, and so do "herbaceous_area". Interestingly, "traffic" has a
#   very large range and zero values!
# - Otherwise, the IVs are quite well sampled.



### For CC______________________________________________________________________#

uni.boxplots(cc[,15:ncol(cc)]) # Only for IVs, not Ys.
ppl.tits::uni.dotplots(cc[,15:ncol(cc)])
# We can see that:
# - There are extreme values for all F-metrics, especially the "beta1".
# - There are extreme values for the "woodyveg_volume" variables. Here, the weighting does not change things much.
# - Same as for PM.



### For NTITS___________________________________________________________________#

uni.boxplots(ntits_reduced[,15:ncol(ntits_reduced)]) # Only for IVs, not Ys.
ppl.tits::uni.dotplots(ntits_reduced[,15:ncol(ntits_reduced)])
# We can see that:
# - There are extreme values for all F-metrics, especially the "beta1", but it seems that pooling together PM and CC
#   helps to have smoother distributions.
# - The "woodyveg" variables also seem to have nicer distributions.
# - The "noise_m" variable still is quite strongly left-skewed, and so does "open_area".
# - Otherwise, the IVs look relatively nice.





##### DISTRIBUTION, SKEWNESS AND KURTOSIS #####
##### Examining normality, skewness and kurtosis
# ______________________________________________

### For PM______________________________________________________________________#

ppl.tits::uni.histograms(pm[,15:ncol(pm)])

pm.x <- pm[,15:ncol(pm)]
pm.xnum <- pm.x[, sapply(pm.x, is.numeric)]
tab <- data.frame(moments::skewness(x = pm.xnum), moments::kurtosis(x = pm.xnum)-3)
pmx_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# We can see that, as expected:
# - Some variables have quite excessive skewness: e.g. "pmF_d3_beta1"!
# - Some variables have quite excessive kurtosis: e.g. "pmF_d1_beta0", "pmF_d1_beta1", "woodyveg_volume",
#   "woodyveg_vw", "noise_m" and "mother_cond"!



### For CC______________________________________________________________________#

ppl.tits::uni.histograms(cc[,15:ncol(cc)])

cc.x <- cc[,15:ncol(cc)]
cc.xnum <- cc.x[, sapply(cc.x, is.numeric)]
tab <- data.frame(moments::skewness(x = cc.xnum), moments::kurtosis(x = cc.xnum)-3)
ccx_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# Interestingly, most values are not that high but still, quite excessive for the beta1 "F-metrics", the "woodyveg"
# variables and "cumdd_30."



### For NTITS___________________________________________________________________#

ppl.tits::uni.histograms(ntits_reduced[,15:ncol(ntits_reduced)])

ntits.x <- ntits_reduced[,15:ncol(ntits_reduced)]
ntits.xnum <- ntits.x[, sapply(ntits.x, is.numeric)]
tab <- data.frame(moments::skewness(x = ntits.xnum), moments::kurtosis(x = ntits.xnum)-3)
ntitsx_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# If skewness values are fairly acceptable (except for "F_metric_d3b1"), several variables present very excessive
# kurtosis: "F_metric_d1b0", all beta1 F-metrics, "woodyveg_volume", "woodyveg_vw", and "noise_m"!
rm(tab)





##### MULTIVARIATE RELATIONSHIPS #####
##### Bivariate correlations and outliers
# _______________________________________

### For PM______________________________________________________________________#
pm.xnum %>%
  dplyr::select(-pmF_d3_beta0, -pmF_d2_beta1, -pmF_d3_beta1, -woodyveg_volume, -open_area,
                -father_cond, -mother_cond) -> pm.xnum # For the sake of readability, I delete some
# variables.

# To compute the correlation matrix:
res.cor.pmx <- round(stats::cor(pm.xnum, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.pmx <- ggcorrplot::cor_pmat(x = pm.xnum, method = "spearman")

pmx.corplot <- ggcorrplot::ggcorrplot(res.cor.pmx, type = "upper",
                       outline.col = "white",
                       ggtheme = ggplot2::theme_gray,
                       colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.pmx, insig = "blank")
# We can see that:
# - All F-metrics and woodyveg variables are quite strongly positively correlated, but negatively with
#   urban intensity, management intensity and the pollution variables. That's quite unsurprising, the more
#   woody vegetation in the landscape, the more connectivity, and the less room for urban features such as
#   buildings, roads, pollution emissions, etc.
# - Quite logically, the pollution variables are positively correlated with urban intensity.
# - We see that "cumdd_30" is not correlated with any variable, except father_cond.
# - Father_cond and mother_cond present reversed correlation patterns (I should verify the meaning of their
#   values): fathers seem positively correlated with woody vegetation and connectivity proxies while mothers,
#   are negatively correlated or, not correlated at all.

pmx.pairplot <- GGally::ggpairs(pm.xnum)
# We find again the same patterns. Yet, we can see that:
# - Some relationships are actually not linear but curvilinear (e.g. between woodyveg/connectivity
#   variables and "urban intensity" or "light pollution").
# - We don't see very clear multivariate outliers, except perhaps between the woodyveg-connectivity variables
#   (or may be the sign of interactions).



### For CC______________________________________________________________________#
cc.xnum %>%
  dplyr::select(-ccF_d130_beta0, -ccF_d30_beta1, -ccF_d130_beta1, -woodyveg_volume, -open_area) -> cc.xnum # Same.
# To compute the correlation matrix:
res.cor.ccx <- round(stats::cor(cc.xnum, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.ccx <- ggcorrplot::cor_pmat(x = cc.xnum, method = "spearman")

ccx.corplot <- ggcorrplot::ggcorrplot(res.cor.ccx, type = "upper",
                       outline.col = "white",
                       ggtheme = ggplot2::theme_gray,
                       colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.ccx, insig = "blank")
# We can see that:
# - All F-metrics and woodyveg variables are quite strongly positively correlated, but negatively with
#   urban intensity, management intensity and the pollution variables. That's quite unsurprising, the more
#   woody vegetation in the landscape, the more connectivity, and the less room for urban features such as
#   buildings, roads, pollution emissions, etc.
# - Quite logically, the pollution variables are positively correlated with urban intensity.
# - We see that "cumdd_30" is not correlated with any variable.

ccx.pairplot <- GGally::ggpairs(cc.xnum)
# We find again the same patterns. Yet, we can see that:
# - Some relationships are actually not linear but curvilinear (e.g. between woodyveg/connectivity
#   variables and "urban intensity" or "light pollution").
# - There may be problematic outliers.



### For NTITS___________________________________________________________________#
ntits.xnum %>%
  dplyr::select(-F_metric_d3b0, -F_metric_d3b1, -urban_intensity, -woodyveg_volume, -open_area) -> ntits.xnum # Same.
# To compute the correlation matrix:
res.cor.ntitsx <- round(stats::cor(ntits.xnum, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.ntitsx <- ggcorrplot::cor_pmat(x = ntits.xnum, method = "spearman")

ntitsx.corplot <- ggcorrplot::ggcorrplot(res.cor.ntitsx, type = "upper",
                                      outline.col = "white",
                                      ggtheme = ggplot2::theme_gray,
                                      colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.ntitsx,
                                      insig = "blank")
# We can see that:
# - All F-metrics and woodyveg variables are quite strongly positively correlated, but negatively with the
#   urban intensity and pollution variables. That's quite unsurprising, the more woody vegetation in the
#   landscape, the more connectivity, and the less room for urban features such as buildings, roads, pollution
#   emissions, etc.
# - Herbaceous areas are less strongly correlated that woody areas.
# - Quite logically, the pollution variables are positively correlated with other urban related ones.
# - We see that "cumdd_30" is not correlated with any variable except "min_t_between".
# - Interestingly, "min_t_before" is slightly correlated with most variables when "min_t_between" is not! It also
#   shows that the minimal temperature before laying tends to be higher when there is not much vegetation (i.e.
#   when the stations are located in densely urbanised areas).

ntitsx.pairplot <- GGally::ggpairs(ntits.xnum)
# We find again the same patterns. Yet, we can see that:
# - Some relationships are actually not linear but curvilinear.
# - There may be problematic outliers.





##### EXPLORATION OF THE RESPONSES #####
##### Outliers and distribution
# _____________________________

pm.y <- pm[,9:14]
cc.y <- cc[,9:14]
ntits.y <- ntits_reduced[,9:14]

### For PM______________________________________________________________________#

uni.boxplots(pm.y)
ppl.tits::uni.dotplots(pm.y)
# we can say that the 6 DV (dependant variables) have relatively nice distributions although we could have
# a probable zero-inflation for brood_size and fledgling_nb, as well as a slight left-skewness for the
# morphometric variables.
tab <- data.frame(moments::skewness(x = stats::na.omit(pm.y)),
                  moments::kurtosis(x = stats::na.omit(pm.y))-3)
pmy_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# But skewness and kurtosis values are quite satisfactory.


### For CC______________________________________________________________________#

uni.boxplots(cc.y)
ppl.tits::uni.dotplots(cc.y)
# Overall, the same patterns as for PM can be seen but the distributions of "clutch_size" and especially
# "brood_size" might reveal problematic.
tab <- data.frame(moments::skewness(x = stats::na.omit(cc.y)),
                  moments::kurtosis(x = stats::na.omit(cc.y))-3)
ccy_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# As expected, "clutch_size" present a slight excess in kurtosis.


### For NTITS___________________________________________________________________#

uni.boxplots(ntits.y)
ppl.tits::uni.dotplots(ntits.y)
# we can say that the 6 DV (dependant variables) have relatively nice distributions although we seem to have
# a zero-inflation for "brood_size" and "fledgling_nb", as well as a slight left-skewness for the morphometric
# variables.
tab <- data.frame(moments::skewness(x = stats::na.omit(ntits.y)),
                  moments::kurtosis(x = stats::na.omit(ntits.y))-3)
pmy_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# But skewness and kurtosis values are very satisfactory.


##### Multivariate relationships
# ______________________________

### For PM______________________________________________________________________#

# To compute the correlation matrix:
res.cor.pmy <- round(stats::cor(pm.y, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.pmy <- ggcorrplot::cor_pmat(x = pm.y, method = "spearman")

pmy.corplot <- ggcorrplot::ggcorrplot(res.cor.pmy, type = "upper",
                       outline.col = "white",
                       ggtheme = ggplot2::theme_gray,
                       colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.pmy, insig = "blank")
# We can see that:
# - The 3 reproductive variables are strongly positively correlated.
# - Mass is slightly negatively correlated with "cluch" and "brood_size" while "fledgling_nb" is weakly
#   negatively correlated with "wing_length".
# - The 3 morphometric variables seem to covary in the same direction as well!

pmy.pairplot <- GGally::ggpairs(pm.y)
# We can see that:
# - The positive relationship among the morphometric variables is undeniable.
# - Among the reproductive variables, positive trends are also quite clear although slightly muddied by the
#   presence of zero-inflation and increasing variation with "time" (as in subsequent variable: egg > chick
#   > fledgling).
# - The negative correlation between "mass" and "clutch_size" and "brood_size" is not so clear here, and
#   reproductive DV seem rather unrelated to morphometric ones.


### For CC______________________________________________________________________#

# To compute the correlation matrix:
res.cor.ccy <- round(stats::cor(cc.y, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.ccy <- ggcorrplot::cor_pmat(x = cc.y, method = "spearman")

ccy.corplot <- ggcorrplot::ggcorrplot(res.cor.ccy, type = "upper",
                       outline.col = "white",
                       ggtheme = ggplot2::theme_gray,
                       colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.ccy, insig = "blank")
# We can see that:
# - If "clutch" and "brood_sizes" are strongly positively correlated, surprisingly, "fledgling_nb" appears
#   uncorrelated to "clutch_size" (and "brood_size"?). Either this reflects a very non-linear relationship
#   between these variables or that would imply that, for CC, the number of juvenile that take-off is
#   globally unrelated to the number of eggs layed or the number of chicks in the brood, which seems
#   IMPOSSIBLE! Perhaps a textbook example of the limits of biv <- riate correlations???
# - On the other hand, "fledgling_nb" is positively correlated to all 3 morphometric DV!
# - The 3 morphometric variables seem to covary in the same direction!

ccy.pairplot <- GGally::ggpairs(cc.y)
# We can see that:
# - The positive relationship among the morphometric variables is undeniable.
# - The positive relationship between "fledgling_nb" and the morphometric DV seems also quite strong!
# - Increasing variability in "fledgling_nb" with increasing values of the other 2 reproductive DV seem
#   to explain the lack of positive rank-based correlations (note that here, correlations are significant),
#   but relationships do exist!
# - Negative correlation between clutch" and "brood_sizes" and several morphometric DV might exist as well!


### For NTITS___________________________________________________________________#

# To compute the correlation matrix:
res.cor.ntitsy <- round(stats::cor(ntits.y, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.ntitsy <- ggcorrplot::cor_pmat(x = ntits.y, method = "spearman")

ntitsy.corplot <- ggcorrplot::ggcorrplot(res.cor.ntitsy, type = "upper",
                                      outline.col = "white",
                                      ggtheme = ggplot2::theme_gray,
                                      colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.ntitsy,
                                      insig = "blank")
# We can see that:
# - The 3 reproductive variables are strongly positively correlated and so are the 3 morphometric ones!
# - Both "clutch_size" and "brood_size" seem to be negatively correlated with the morphometric variables
#   suggesting that when large clutches or broods lead to smaller chicks!
# - Interestingly, the number of fledglings seem uncorrelated with nestlings morphology.

ntitsy.pairplot <- GGally::ggpairs(ntits.y)
# We can see that:
# - The positive relationship among the morphometric variables is undeniable.
# - The zero-inflation may be due to another process as the total failures in "brood_size" or "fledgling_nb"
#   (i.e. 0's) appear unrelated with the numbers of the preceding processes (e.g. egg laying or hatching).
# - The negative correlation between "clutch_size" and "brood_size" and the morphometric variables is not so
#   clear here but seem to exist nonetheless. However, it may be an artefact of "species" because CC is smaller
#   but tends to be more productive (see below).

# Disclaimer, remind that:
summary(pm.y)
summary(cc.y)
# - PM: CS ranges 2-12 (mode 8); BS 0-12 (8); FN 0-11 (0 or 6);
#       MA 9.9-18.4 (mean 14.8); TL 15.7-20.6 (18.9); WL 27-50.4 (41.5).
# - CC: CS ranges 2-14 (mode 11); BS 0-14 (10); FN 0-12 (0 or 8);
#       MA 5.8-13 (mean 9); TL 12-17.7 (15.8); WL 17.5-40.1 (32.9).





########################## ************************************************* ###############################
##### DATA PREP. FOR MODELLING #####

rm(cc.x, cc.y, cc.xnum, pm.x, pm.y, pm.xnum, tab, ntits.x, ntits.y, ntits.xnum,
   res.cor.ccx, res.cor.ccy, res.cor.pmx, res.cor.pmy, res.pcor.ccx, res.pcor.ccy, res.pcor.pmx, res.pcor.pmy)

### For PM and CC_______________________________________________________________#
pm %>% dplyr::mutate(woodyveg_vw = woodyveg_vw/1000, # Converting m3 into dm3.
                     noise_m = noise_m/10, # Converting dB into B.
                     cumdd_30 = cumdd_30/100) %>% # Converting degree-days into hundred of degree-days.
  dplyr::mutate(brood_size = round(brood_size, digits = 0), # There was a decimal count.
                logged_Fmetric = log10(pmF_d2_beta0), # Predictors normalisation.
                logged_woodyveg = log10(woodyveg_vw)) %>%
  dplyr::mutate(year = stats::relevel(x = year, ref = 3)) %>% # Assign 2019 as the reference group.
  dplyr::mutate(manag_low = ifelse(manag_intensity == "0", "1", "0"),
                manag_mid = ifelse(manag_intensity == "1", "1", "0"),
                manag_high = ifelse(manag_intensity == "2", "1", "0")) %>%
  dplyr::mutate(dplyr::across(where(is.matrix), as.numeric),
                dplyr::across(where(is.character), as.factor)) %>%
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>%
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) -> pm2 # Added a very small amount of
# noise to coordinates to avoid groups with exactly similar coordinates (related to low Lat/Long
# resolution) which prevent the proper use of the DHARMa package autocorrelation test!
# I could also try median-centering + IQR.

# If I need to generate a dataset for CC (that is, if I intend to analyse the CC data alone), then I could copy-paste
# the code for PM and simply remove the "parental condition" variables.


### For all tit nestlings (NTITS)_______________________________________________#
ntits_reduced %>% dplyr::mutate(woodyveg_vw = woodyveg_vw/1000, # Converting m3 into dm3.
                     noise_m = noise_m/10, # Converting dB into B.
                     cumdd_30 = cumdd_30/100) %>% # Converting degree-days into hundred of degree-days.
  dplyr::mutate(brood_size = round(brood_size, digits = 0), # There was a decimal count.
                logged_Fmetric = log10(F_metric_d2b0), # Predictors normalisation.
                logged_Fmetric_d1 = log10(F_metric_d1b0),
                logged_Fmetric_d3 = log10(F_metric_d3b0),
                logged_Fmetric_d1b1 = log10(F_metric_d1b1),
                logged_Fmetric_d2b1 = log10(F_metric_d2b1),
                logged_woodyveg = log10(woodyveg_vw),
                logged_woodyvol = log10(woodyveg_volume),
                logged_woody_area = log10(woody_area),
                logged_herby_area = log10(herbaceous_area),
                logged_traffic = log10(traffic+1),
                logged_built_area = log10(built_area+1),
                logged_open_area = log10(open_area)) %>%
  dplyr::mutate(brood_size = as.integer(brood_size),
                hatching_rate = brood_size/clutch_size) %>%
  dplyr::mutate(year = stats::relevel(x = year, ref = 3), # Assign 2019 as the reference group.
                species = stats::relevel(x = species, ref = 2)) %>% # Assign PM as the reference group.
  dplyr::mutate(manag_low = ifelse(manag_intensity == "0", "1", "0"),
                manag_mid = ifelse(manag_intensity == "1", "1", "0"),
                manag_high = ifelse(manag_intensity == "2", "1", "0")) %>%
  dplyr::mutate(dplyr::across(where(is.matrix), as.numeric),
                dplyr::across(where(is.character), as.factor)) %>%
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>%
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) -> ntits2

