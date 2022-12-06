# ------------------------------------------------- #
##### Functions for initial inference modelling #####
# ------------------------------------------------- #

# The functions of this R file are meant to wrap all formal inference modelling made in this study, as
# opposed to "preparation modelling" (cf. previous scripts) and "exploratory modelling" that will perhaps
# be made afterwards; i.e. after we have formally tested our research hypotheses in a robust inferential
# framework. The so-called "exploratory modelling" is related to additional questions and perspectives
# that will have, by nature, far less support as the data have already been used for formal testing (so
# type-I error rates cannot be guaranteed anymore)!

####### SUPER IMPORTANT NOTE #######
# SHOULD START BY CALLING/SOURCING the EDA script first (because no function yet???)!
# SHOULD START BY CALLING/SOURCING the EDA script first (because no function yet???)!
# SHOULD START BY CALLING/SOURCING the EDA script first (because no function yet???)!
# SHOULD START BY CALLING/SOURCING the EDA script first (because no function yet???)!
# SHOULD START BY CALLING/SOURCING the EDA script first (because no function yet???)!

##### PM MODELS #####
##### 1. Clutch size: Poisson GLMM #####
# ______________________________________


hist(pm$clutch_size)

colnames(pm)
summary(pm)
