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








####### SUPER IMPORTANT NOTE #######
# To finish converting all that into functions, I'll have to regenerate the data (used for the report) and
# create a META-function that calls all these functions and exports "targetable" outputs, for both PM
# and CC!!!!!



##### DATA IMPORT #####
##### Data import
# _______________

library(magrittr)

### Data import and preparation_________________________________________________#
# Import and binding:
tits_clean <- ppl.tits::ntits_clean

manag <- readr::read_csv2(here::here("input_raw_data", "veg_manag_factor.csv"),
                                col_names = TRUE, na = "NA",
                                col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                        manag_intensity = readr::col_factor(
                                                          ordered = TRUE,
                                                          levels = c("0", "1", "2"),
                                                          include_na = FALSE)))
tits_clean <- dplyr::inner_join(tits_clean, manag, by = "id_nestbox")

fmetrics_pm <- readr::read_csv2(here::here("input_raw_data", "cmetrics_pm.csv"),
                                col_names = TRUE, na = "NA",
                                col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                        id_patch = readr::col_factor()))
fmetrics_pm %>% dplyr::select(id_nestbox, pmF_d113_beta0, pmF_d531_beta0, pmF_d113_beta1, pmF_d531_beta1) %>%
  dplyr::inner_join(tits_clean, fmetrics_pm, by = "id_nestbox") -> ttt
fmetrics_cc <- readr::read_csv2(here::here("input_raw_data", "cmetrics_cc.csv"),
                                col_names = TRUE, na = "NA",
                                col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                        id_patch = readr::col_factor()))
fmetrics_cc %>% dplyr::select(id_nestbox, ccF_d92_beta0, ccF_d311_beta0, ccF_d92_beta1, ccF_d311_beta1) %>%
  dplyr::inner_join(ttt, fmetrics_cc, by = "id_nestbox") -> ttt

tits_clean$age_num <- as.numeric(as.character(tits_clean$age_class))
tits_clean$strata_num <- as.numeric(as.character(tits_clean$strata_div))
tits_clean$manag_num <- as.numeric(as.character(tits_clean$manag_intensity))
tits_clean %>%  dplyr::relocate(manag_intensity, .after = age_class) -> tits_clean
tits_clean %>%  dplyr::relocate(manag_num, .after = manag_intensity) -> tits_clean
tits_clean %>%  dplyr::relocate(age_num, .after = age_class) -> tits_clean

ntits <- cbind(tits_clean, ttt[,2:9])
ntits %>% dplyr::select(-coord_x, -coord_y, -father_id, -mother_id, -mean_winter_t, -sd_winter_t,
                        -lsource_vs150_m, -lsource_vs150_iq, -soft_manag_area) -> ntits
rm(ttt, fmetrics_pm, fmetrics_cc, manag, tits_clean)

# Deletion of problematic observations:
ntits <- tibble::as_tibble(ntits)
ntits %>% dplyr::filter(id_nestbox != "DIJ-201") %>%
  dplyr::filter(id_nestbox != "DIJ-212") %>%
  dplyr::filter(id_nestbox != "DIJ-213") %>%
  dplyr::filter(id_nestbox != "DIJ-214") %>%
  dplyr::filter(id_nestbox != "DIJ-215") %>%
  dplyr::filter(id_nestbox != "DIJ-216") %>%
  dplyr::filter(id_nestbox != "DIJ-220") -> ntits
# These observations were problematic because of a inmportant classification error from the landcover
# map that underestimated their woody vegetation cover and thus affected many other variables, such as
# the proportion of each landcover class as well as the connectivity metrics. If we have time, we will
# correct the landcover map and recompute everything to avoid losing these 12 observations.

# Weighting of woodyveg_volume with strata_num:
ntits %>% dplyr::mutate(strata_w = dplyr::case_when(
  strata_div == "0" ~ 0.25,
  strata_div == "1" ~ 0.5,
  strata_div == "2" ~ 0.6,
  strata_div == "3" ~ 0.75,
  strata_div == "4" ~ 0.9)) %>%
  dplyr::mutate(woodyveg_vw = woodyveg_volume*strata_w) -> ntits





##### DATA REDUCTION #####
##### Data reduction
# __________________

### For variables related to vegetation characteristics_________________________#
# Does not yield meaningful results.

### For variables related to landscape composition______________________________#
ntits %>% dplyr::select(id_nestbox, site, trafic, built_area, open_area, herbaceous_area) -> xxx

# Normed-PCA:
res.pca <- FactoMineR::PCA(X = xxx[, 3:ncol(xxx)], scale.unit = TRUE, graph = FALSE)
# To plot results:
landscape.varplot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
landscape.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
#gridExtra::grid.arrange(landscape.varplot, landscape.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (52%)
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

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (76.2%)
# of my four variables, we can use the coordinates of observations on this axis as a synthetic variable:
zzz <- res.pca$ind$coord[,1]
ntits$light_pollution <- zzz # This variable opposes nestboxes located in very dense urban areas with a
# lot of traffic to those located in greener contexts.



### Final formatting____________________________________________________________#
ntits %>% dplyr::filter(species == "PM") %>%
  dplyr::select(id_nestbox, site, year, breeding_window, laying_date, flight_date,
                clutch_size, brood_size, fledgling_nb, mass, tarsus_length, wing_length,
                pmF_d113_beta0, pmF_d531_beta0, pmF_d113_beta1, pmF_d531_beta1,
                woodyveg_volume, woodyveg_vw, strata_div,
                urban_intensity, manag_intensity, light_pollution, noise_m, cumdd_30,
                father_cond, mother_cond) -> pm
ntits %>% dplyr::filter(species == "CC") %>%
  dplyr::select(id_nestbox, site, year, breeding_window, laying_date, flight_date,
                clutch_size, brood_size, fledgling_nb, mass, tarsus_length, wing_length,
                ccF_d92_beta0, ccF_d311_beta0, ccF_d92_beta1, ccF_d311_beta1,
                woodyveg_volume, woodyveg_vw,
                urban_intensity, manag_intensity, light_pollution, noise_m, cumdd_30) -> cc
rm(res.pca, xxx, zzz)





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

uni.boxplots(pm[,13:ncol(pm)])
ppl.tits::uni.dotplots(pm[,13:ncol(pm)])
# We can see that:
# - There are extreme values for all F-metrics, especially the "beta1" (we already know that, remove
#   them for the FINAL VERSION)!!!
# - There are extreme values for the "woodyveg" variables, yet the weighting by "strata_div" slightly
#   alleviate the problem.
# - The "pollution" variables are very slightly skewed.
# - Otherwise, the IVs are quite well sampled.



### For CC______________________________________________________________________#

uni.boxplots(cc[,13:ncol(cc)]) # Only for IVs, not Ys.
ppl.tits::uni.dotplots(cc[,13:ncol(cc)])
# We can see that:
# - There are extreme values for all F-metrics, especially the "beta1" (we already know that, remove
#   them for the FINAL VERSION)!!!
# - There are extreme values for the "woodyveg" variables. Here, the weighting does not change things much.
# - The "pollution" variables are very slightly skewed.





##### DISTRIBUTION, SKEWNESS AND KURTOSIS #####
##### Examining normality, skewness and kurtosis
# ______________________________________________

### For PM______________________________________________________________________#

ppl.tits::uni.histograms(pm[,13:ncol(pm)])

pm.x <- pm[,13:ncol(pm)]
pm.xnum <- pm.x[, sapply(pm.x, is.numeric)]
tab <- data.frame(moments::skewness(x = pm.xnum), moments::kurtosis(x = pm.xnum)-3)
knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# We can see that some variables have, as expected, quite excessive kurtosis, especially "woodyveg_vw"!



### For CC______________________________________________________________________#

ppl.tits::uni.histograms(cc[,13:ncol(cc)])

cc.x <- cc[,13:ncol(cc)]
cc.xnum <- cc.x[, sapply(cc.x, is.numeric)]
tab <- data.frame(moments::skewness(x = cc.xnum), moments::kurtosis(x = cc.xnum)-3)
knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# Interestingly, values are not that high although yet quite excessive.
rm(tab)





##### MULTIVARIATE RELATIONSHIPS #####
##### Bivariate correlations and outliers
# _______________________________________

### For PM______________________________________________________________________#

pm.x$strata_div <- as.numeric(as.character(pm.x$strata_div))
pm.x$manag_intensity <- as.numeric(as.character(pm.x$manag_intensity))

# To compute the correlation matrix:
res.cor.pmx <- round(stats::cor(pm.x, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.pmx <- ggcorrplot::cor_pmat(x = pm.x, method = "spearman")

ggcorrplot::ggcorrplot(res.cor.pmx, type = "upper",
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

GGally::ggpairs(pm.xnum)
# We find again the same patterns. Yet, we can see that:
# - Some relationships are actually not linear but curvilinear (e.g. between woodyveg/connectivity
#   variables and "urban intensity" or "light pollution").
# - We don't see very clear multivariate outliers.



### For CC______________________________________________________________________#

cc.x$manag_intensity <- as.numeric(as.character(cc.x$manag_intensity))

# To compute the correlation matrix:
res.cor.ccx <- round(stats::cor(cc.x, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.ccx <- ggcorrplot::cor_pmat(x = cc.x, method = "spearman")

ggcorrplot::ggcorrplot(res.cor.ccx, type = "upper",
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


GGally::ggpairs(cc.xnum)
# We find again the same patterns. Yet, we can see that:
# - Some relationships are actually not linear but curvilinear (e.g. between woodyveg/connectivity
#   variables and "urban intensity" or "light pollution").
# - There may be problematic outliers.





##### MULTICOLLINEARITY #####

### For PM______________________________________________________________________#

pm.x$response <- rnorm(n = nrow(pm.x), mean = 50, sd = 10)
pm.x %>% dplyr::select(-pmF_d113_beta0, -pmF_d113_beta1, -pmF_d531_beta1, -woodyveg_volume) %>%
  dplyr::mutate(manag_intensity = as.factor(manag_intensity),
                strata_div = as.factor(strata_div)) -> pm.test
test_lm <- lm(response~., data = pm.test)
summary(test_lm)
car::vif(mod = test_lm)
# Curiously, the VIF and GVIF values are quite acceptable! And, as I was told on CV, they're invariant
# to model type or response variable.



### For CC______________________________________________________________________#

cc.x$response <- rnorm(n = nrow(cc.x), mean = 50, sd = 10)
cc.x %>% dplyr::select(-ccF_d92_beta0, -ccF_d92_beta1, -ccF_d311_beta1, -woodyveg_volume) %>%
  dplyr::mutate(manag_intensity = as.factor(manag_intensity)) -> cc.test
test_lm <- lm(response~., data = cc.test)
summary(test_lm)
car::vif(mod = test_lm)
# Here, the GVIF is a bit high to my taste (yet < 5).





##### EXPLORATION OF THE RESPONSES #####
##### Outliers and distribution
# _____________________________

pm.y <- pm[,7:12]

### For PM______________________________________________________________________#

uni.boxplots(pm.y)
ppl.tits::uni.dotplots(pm.y)



colnames(cc)

### WHAT Ys????? Explore their values too!!!
### WHAT Ys????? Explore their values too!!!
### WHAT Ys????? Explore their values too!!!
### WHAT Ys????? Explore their values too!!!

