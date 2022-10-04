# # --------------------------------------------------------- #
# ##### Functions to create the "local" predictive models #####
# # --------------------------------------------------------- #
#
# # The functions of this R file are meant to prepare and build the so-called "local models" of our
# # analysis workflow. These predictive models, based only on local environmental factors (hence their
# # name), are intended to approximate the "quality" of habitat patches in our study area and thus to
# # assess the "capacity" of these patches in the subsequent Graphab connectivity models.
#
# utils::globalVariables("where") # This is necessary for now as tidyselect::where is not
# # an exported function!
#
# tits <- ppl.tits::tdata_upD_final()$final_dataset
