# --------------------------------------------------------- #
##### Functions to create the "local" predictive models #####
# --------------------------------------------------------- #

# The functions of this R file are meant to prepare and build the so-called "local models" of our
# analysis workflow. These predictive models, based only on local environmental factors (hence their
# name), are intended to approximate the "quality" of habitat patches in our study area and thus to
# assess the "capacity" of these patches in the subsequent Graphab connectivity models.


### ________________________________________________
#' Build a predictive model of local habitat quality
#'
#' @description The `local_quality_model` function builds a Random Forest (RF) regression model to
#' predict the local habitat quality for Great tits (\emph{Parus major} only) approximated by the species
#' clutch size. \cr
#' The RF model, fitted using the \code{\link[randomForest:randomForest]{randomForest}} package, is
#' build without hyperparameter tuning even though predictor variables importance is also computed.
#' Additionally, the `local_quality_model` function also assess the stability of the RF modelling
#' process by repeating it 100 times and reporting the associated out-of-bag R squared values as
#' well as permutation-based variable importance metrics (computed as averaged increase in Mean
#' Squared Error).
#' @note This function only produces outputs for the Great tits (\emph{Parus major}). Computations
#' for Blue tits (\emph{Cyanistes caeruleus}) were too unstable to be exploited due to insufficient
#' sample size.
#'
#' @param my_tdata The name of the \strong{target} that contains the dataset generated
#' by \code{\link[ppl.tits:tdata_upD_final]{tdata_upD_final}}. Note that this argument is not
#' necessarily required but is strongly advised.
#' Therefore, if you want to reproduce this work, you should not call this function unless you
#' have previously built the {targets} pipeline described in _targets.R (e.g. by using
#' targets::tar_make())! If you DO NOT KNOW what I'm talking about, then you should
#' read the README file (https://github.com/mrelnoob/ppl.tits) and PROBABLY NOT modify the
#' function's arguments (see examples section).
#'
#' @return This functions returns three things: 1) The Random Forest object built to be used for
#' prediction using new data (e.g. using
#' \code{\link[randomForest:predict.randomForest]{predict.randomForest}}; accessible through
#' `local_quality_model()$rf4pm`); 2) the OOB R2 stability plot (accessible through
#' `local_quality_model()$r_squared.stab`); and 3) the variables importance stability plot
#' (accessible through `local_quality_model()$var_importance.stab`).
#' @export
#' @importFrom here here
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_integer
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom randomForest randomForest
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 coord_flip
#'
#' @examples
#' \dontrun{
#' # This line should work as long as "my_newdata" is an appropriately formatted dataset containing
#' # the same variables as the input RF model:
#' patch_quality <- predict.randomForest(
#'   ppl.tits::local_quality_model()$rf4pm,
#'   newdata = my_newdata)
#'
#' ppl.tits::local_quality_model()$r_squared.stab # To generate the R2 stability plot
#' ppl.tits::local_quality_model()$var_importance.stab # To generate the variable importance
#' # stability plot
#' }
local_quality_model <- function(my_tdata = here::here("output", "tables", "ndata_clean.csv")){

  ##### Data preparation
  # ____________________

  # Dataset import and reduction:
  tits <- readr::read_csv2(my_tdata, col_names = TRUE,
                           col_types = readr::cols(clutch_size = readr::col_integer(),
                                                   species = readr::col_factor())) # NOTE: I have
  # to generate "tits" by reading the exported "ndata_parcond" dataset in order for {targets}
  # to be able to follow modifications.
  # Otherwise, I could just have used ppl.tits::tdata_upD_final())$dataset, or call its associated
  # target!

  tits %>% dplyr::select(clutch_size, species, coord_x, coord_y, noise_m, noise_iq,
                         built_area, open_area, soft_manag_area, woodyveg_volume,
                         woodyveg_sd) -> tits





  ##### Random Forest to predict Great tits clutch size
  # ___________________________________________________

  ### Random Forest building____________________________________________________#
  tits %>% dplyr::filter(species == "PM") %>%
    dplyr::select(-species) -> pm

  set.seed(38)
  pm.rf <- randomForest::randomForest(clutch_size ~ ., data = pm, importance = TRUE,
                                      nPerm = 2, keep.forest = TRUE) # keep.forest is crucial to export
  # the forest and make predictions with new data.
  # print(pm.rf) # To print a summary of the RF object.



  ### Model stability assessment________________________________________________#
  set.seed(200)
  # To replicate the RF analysis a 100 times while storing R2 and variable importance metrics:
  rf.replic <- replicate(100, {
    pm.rf.tmp <- randomForest::randomForest(clutch_size ~ ., data = pm, importance = TRUE, nPerm = 2)
    list(r.squared = pm.rf.tmp$rsq[500], varimp = pm.rf.tmp$importance[,1])
  }, simplify = FALSE)
  # Note that here, only the R2 of the 500th tree is exported and only the permutation-based
  # variable importance metric.

  # Make a data.frame with the R2 values:
  rsquared <- data.frame(r.squared = sapply(rf.replic, function(x){x[["r.squared"]]}))

  #To plot the kernel density of the R2 values:
  rsq.plot <- ggplot2::ggplot(rsquared, ggplot2::aes(r.squared)) +
    ggplot2::geom_density() +
    ggplot2::theme_bw()

  # Make a data.frame with variable importance data from the replications:
  var_importance <- unlist(lapply(rf.replic, function(x){x[["varimp"]]}))
  var_importance <- data.frame(
    Variable = ordered(names(var_importance),
                       levels = c("coord_x", "coord_y", "noise_m", "noise_iq", "built_area",
                                  "open_area", "soft_manag_area", "woodyveg_volume", "woodyveg_sd")),
    Importance = var_importance)

  # To plot variable importance as rotated boxplots:
  var_imp.plot <- ggplot2::ggplot(var_importance, ggplot2::aes(x=Variable, y=Importance)) +
    ggplot2::geom_boxplot(color="black", size=0.2) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50", linetype = 1) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::ylab("Permutation-based importance (% increase in MSE)") +
    ggplot2::coord_flip()

  # To dismiss notes regarding "visible binding for global variables" during the CMD Check:
  clutch_size <- Importance <- Variable <- built_area <- coord_x <- coord_y <- noise_iq <-
    noise_m <- open_area <- r.squared <- soft_manag_area <- species <- woodyveg_sd <-
    woodyveg_volume <- NULL





  ##### To export the outputs
  # _______________________________
  output <- list(rf4pm = pm.rf,
                 r_squared.stab = rsq.plot,
                 var_importance.stab = var_imp.plot)
  return(output)
  # _______________________________
}

