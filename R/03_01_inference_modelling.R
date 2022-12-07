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

pm %>% dplyr::mutate(f_metric = scale(pmF_d531_beta0),
                     woodvol = scale(woodyveg_vw),
                     urbanity = scale(urban_intensity),
                     light_pol = scale(light_pollution),
                     noise_pol = scale(noise_m),
                     temperature = scale(cumdd_30),
                     male_cond = scale(father_cond),
                     female_cond = scale(mother_cond),
                     f_metric_cent = scale(pmF_d531_beta0, scale = FALSE),
                     woodvol_cent = scale(woodyveg_vw, scale = FALSE)) -> pm2
## QUID de factors????? manag_intensity + year (et quid des levels de year?)!!!
# schielzeth dit dummy coding + centrage -----> mais ça dépend des objectifs!!!
# Sans dummy: manag0 = gestion nat; year0 = 2019!
# Mais poser question CV car toujours pas vraiment régler cette histoire de scaling/centrage!!!!!!

##### 1. Clutch size: Poisson GLMM #####
# ______________________________________

### Initial model fit___________________________________________________________#
# Without centering or scaling:
pmclutch_glmm0 <- lme4::glmer(clutch_size ~ woodyveg_vw*pmF_d531_beta0 + urban_intensity +
                                manag_intensity + light_pollution + noise_m + cumdd_30 + father_cond +
                                mother_cond + (1|id_nestbox) + (1|breeding_window),
                              data = pm2, family = poisson) #### Voir ?glmerControl
# Voir rescaling (jouer avec les unités) > std > glmercontrol!!!!!!!!
# Voir rescaling (jouer avec les unités) > std > glmercontrol!!!!!!!!
# Voir rescaling (jouer avec les unités) > std > glmercontrol!!!!!!!!
# Voir rescaling (jouer avec les unités) > std > glmercontrol!!!!!!!!
# Voir rescaling (jouer avec les unités) > std > glmercontrol!!!!!!!!
# Voir rescaling (jouer avec les unités) > std > glmercontrol!!!!!!!!


### Checking preliminary assumptions____________________________________________#
# Poisson distribution:
theo_count <- rpois(n = nrow(pm), lambda = mean(pm$clutch_size))
tc_df <-data.frame(theo_count)

ggplot2::ggplot(pm, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that clutch_size may not be following a Poisson distribution (underdispersion?
# Normality?), but perhaps it's ok.

# Overdispersion:
aods3::gof()





hist(pm$clutch_size)
ppl.tits::uni.histograms(pm2)
colnames(pm2)
summary(pm)
