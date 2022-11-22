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





# # ---------------------------------------------------------------------------- #
# # ** 3.1.1. Loading and joining datasets ----
#
# tits_clean <- ppl.tits::ntits_clean
# tits_clean %>% dplyr::filter(species == "PM") %>%
#   dplyr::select(id_nestbox, site, coord_x, coord_y, breeding_window,
#                 clutch_size, brood_size, fledgling_nb, mass, tarsus_length, wing_length,
#                 father_cond, mother_cond, min_t_between, lsource_vs150_m, noise_m,
#                 built_area, open_area, woody_area, woodyveg_volume, age_class, strata_div) -> pm
#
# fmetrics <- readr::read_csv2(here::here("input_raw_data", "cmetrics_pm.csv"),
#                              col_names = TRUE, na = "NA",
#                              col_types = readr::cols(id_nestbox = readr::col_factor(),
#                                                      id_patch = readr::col_factor()))
# fmetrics %>% dplyr::select(-id_patch, -cost_to_patch, -perim) %>%
#   dplyr::inner_join(pm, fmetrics, by = "id_nestbox") -> pm_f # Not clear why left_join worked for
# # ppl.tits::tdata_upD_rawiv() and not here (so I had to use inner_join())!!!
#
#
