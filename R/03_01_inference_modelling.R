######################### *--------------------------------------------------* #########################
############################## Inferential modelling for PM (Parus major) ##############################
######################### *--------------------------------------------------* #########################


# The functions of this R file are meant to wrap all formal inference modelling made in this study, as
# opposed to "preparation modelling" (cf. previous scripts) and "exploratory modelling" that will perhaps
# be made afterwards; i.e. after we have formally tested our research hypotheses in a robust inferential
# framework. The so-called "exploratory modelling" is related to additional questions and perspectives
# that will have, by nature, far less support as the data have already been used for formal testing (so
# type-I error rates cannot be guaranteed anymore)!


# --------------------------- #
##### 0. Data preparation #####
# --------------------------- #

# For now, this script should be run after having sourced the EDA script (because no function yet)!

pm %>% dplyr::mutate(woodyveg_vw = woodyveg_vw/1000, # Converting m3 into dm3.
                     noise_m = noise_m/10, # Converting dB into B.
                     cumdd_30 = cumdd_30/100) %>% # Converting degree-days into hundred of degree-days.
  dplyr::mutate(logged_Fmetric = log10(pmF_d531_beta0), # Predictors normalisation.
                logged_woodyveg = log10(woodyveg_vw),
                noise_sqd = noise_m^2) %>%
  dplyr::mutate(year = stats::relevel(x = year, ref = 3)) %>% # Assign 2019 as the reference group.
  dplyr::mutate(manag_low = ifelse(manag_intensity == "0", "1", "0"),
                manag_mid = ifelse(manag_intensity == "1", "1", "0"),
                manag_high = ifelse(manag_intensity == "2", "1", "0")) %>%
  dplyr::mutate(dplyr::across(where(is.matrix), as.numeric),
                dplyr::across(where(is.character), as.factor)) %>%
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>%
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) %>%
  dplyr::mutate(l_Fmetric_std = ((logged_Fmetric-stats::median(logged_Fmetric))/ # Custom standardisation!
                                   1.5*stats::IQR(logged_Fmetric)),
                l_woodyveg_std = ((logged_woodyveg-stats::median(logged_woodyveg))/
                                   1.5*stats::IQR(logged_woodyveg)),
                urban_int_std = ((urban_intensity-stats::median(urban_intensity))/
                                   1.5*stats::IQR(urban_intensity)),
                light_pol_std = ((light_pollution-stats::median(light_pollution))/
                                   1.5*stats::IQR(light_pollution)),
                noise_pol_std = ((noise_sqd-stats::median(noise_sqd))/
                                   1.5*stats::IQR(noise_sqd)),
                temperature_std = ((cumdd_30-stats::median(cumdd_30))/
                                   1.5*stats::IQR(cumdd_30)),
                patern_cond_std = ((father_cond-stats::median(father_cond))/
                                   1.5*stats::IQR(father_cond)),
                matern_cond_std = ((mother_cond-stats::median(mother_cond))/
                                   1.5*stats::IQR(mother_cond))) -> pm2 # Added a very small amount of
# noise to coordinates to avoid groups with exactly similar coordinates (related to low Lat/Long
# resolution) which prevent the proper use of the DHARMa package autocorrelation test!
# schielzeth dit dummy coding + centrage -----> mais ça dépend des objectifs!!!
# Sans dummy: manag0 = gestion nat; year0 = 2019!
# Mais poser question CV car toujours pas vraiment régler cette histoire de scaling/centrage!!!!!!
# I could also try median-centering + IQR (cf. marque-page NOVA)!





########################## ************************************************* ###############################
# -------------------------------- #
##### 1. Modelling clutch size #####
# -------------------------------- #

##### * 1.1. Clutch size: Poisson GLMM -----------------------------------------
# ---------------------------------------------------------------------------- #
### ** 1.1.1. Initial model fit ----
# __________________________________

# I first started with the full model including the interaction term, but it appears to be too complex
# for the data, so I'll switch to trying to properly fit the full additive-only model first:
pmCy_glmm0 <- lme4::glmer(clutch_size ~ woodyveg_vw + pmF_d531_beta0 + urban_intensity + manag_low +
                                manag_high + light_pollution + noise_m + cumdd_30 +
                                father_cond + mother_cond + (1|id_nestbox) + (1|breeding_window),
                              data = pm2, family = poisson) # See ?glmerControl and Bolker's examples
# in case of fitting warnings and errors: e.g. convergence, etc.
# I get the following warnings:
# "boundary (singular) fit: see help('isSingular')
# Warning message:
#   Some predictor variables are on very different scales: consider rescaling".
# So my model is singular and some variables should be rescaled!

# So I'll try by log-transforming some variables:
pmCy_glmm1 <- lme4::glmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                manag_low + manag_high + light_pollution + noise_m +
                                cumdd_30 + father_cond + mother_cond + (1|id_nestbox) +
                                (1|breeding_window),
                              data = pm2, family = poisson)
# lme4::VarCorr(pmCy_glmm1)
# It works but gives a singular fit (i.e. random effect variance = 0)!

# So I'll continue by dropping "breeding_window" as random effect and using year as fixed effect instead:
pmCy_glmm2 <- lme4::glmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                manag_low + manag_high + light_pollution + noise_m +
                                cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                              data = pm2, family = poisson)
# Still singular, so "id_nestbox" is problematic as well!

# I will then try to better tune the models parameters. First, by trying using the "bobyqa" optimizer:
pmCy_glmm2_b <- stats::update(pmCy_glmm2, control = lme4::glmerControl(optimizer = "bobyqa"))
# Does not change a thing! So I'll try using the Gauss-Hermite quadrature (GHQ) for estimation:
pmCy_glmm2_bGHQ <- stats::update(pmCy_glmm2_b, nAGQ = 10)
# Changing the fitting method strongly modified the AIC/BIC, log-likelihood and deviance but changed
# absolutely NOTHING to the parameter estimates! That is simply because GHQ computes things differently.
# I'll try fitting a simple GLM (without 'id_nestbox') to compare:
pmCy_glm0 <- stats::glm(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                          manag_low + manag_high + light_pollution + noise_m +
                          cumdd_30 + father_cond + mother_cond + year,
                        data = pm2, family = poisson)
summary(pmCy_glm0)
# Switching to a simple GLM gives absolutely identical estimates with the first GLMMs (with Laplace
# fits), indicating that the in the absence of variance in the random effects, glmer() was actually
# computing a simple GLM this whole time!
# Even then, estimates and p-values are extremely surprising as only one effect is significant.

# Following Ben Bolker's advice, I'll try the approach proposed by Chung et al. (2013)
# that sets a weak prior on the variance to get an approximate Bayesian maximum a posteriori estimate
# that avoids singularity:
pmCy_bglmm0 <- blme::bglmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                              manag_low + manag_high + light_pollution + noise_m +
                              cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                            data = pm2, family = poisson)
# Estimates are now different but this yields a warning regarding convergence:
# Warning message:
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#     Model failed to converge with max|grad| = 1.98177 (tol = 0.002, component 1)
# To try and fix it, I follow the steps from Ben Bolker presented here in the following document:
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# I don't keep track of all of it, you can do it again if you want.
# Restart, increase iterations and change optimizer:
ss <- lme4::getME(pmCy_bglmm0, c("theta", "fixef"))
pmCy_bglmm1 <- update(pmCy_bglmm0, start=ss,
                      control=lme4::glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
summary(pmCy_bglmm1) # It now converges and throws no warnings but estimates are still weird!
# # Try all optimizers:
# pmCy_bglmm_all <- lme4::allFit(pmCy_bglmm1)
# summary(pmCy_bglmm_all) # Only "bobyqa" converges but this lack of convergence does not seem to matter
# # much as values from all optimizers do not seem to vary much (e.g. logLik, estimates...) and are
# # still wonky.
#
# # Perhaps, with GHQ:
# pmCy_bglmm2 <- update(pmCy_bglmm1, nAGQ = 10)
# summary(pmCy_bglmm2) # Nope...
# pmCy_bglmm2_bootCI <- stats::confint(pmCy_bglmm2, method = "boot") # Very long to run (write it in
# # a file to know how long)!
#
# # As apparently, lme4 output values from models fitted with GHQ are not comparable with models fitted
# # with Laplace approximation or with simple GLMs, I cannot compare the "GHQ model" with the others:
# AICcmodavg::aictab(cand.set = list(pmCy_glmm0, pmCy_glmm1, pmCy_glmm2, pmCy_glmm2_b),
#                    modnames = c("glmm0", "glmm1", "glmm2", "glmm2_b")) # Only works for same type models,
# # so I cannot compare with the "bglmm" but it doesn't change much anyway. I really have to check the
# # underlying modelling assumptions!



### ** 1.1.2. Diagnostics and assumption checks ----
# ________________________________________________

### *** 1.1.2.1. Residuals inspection ----
# Traditional residuals:
plot(pmCy_bglmm1)
plot(pmCy_bglmm1, id_nestbox~stats::resid(.)) # Justification for the nestbox random effect (RE).
plot(pmCy_bglmm1, site~stats::resid(.)) # That's interesting because it shows that there's quite a lot
# of among and within sites variability in the residuals. It may mean a "site" RE is required.
# Note also that "chateau_de_larrey" is an 'outlier' site and 3 other sites have extreme values
# ("seminaire", "chateau_de_pouilly" and "arquebuse").

# Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmCy_bglmm1, n = 1000, plot = FALSE)
par(.pardefault)
plot(simu.resid) # Not ok!

# Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm2$coord_x, y = pm2$coord_y, plot = TRUE)
performance::check_autocorrelation(pmCy_bglmm1) # Significant autocorrelation: use "site" RE?
performance::check_collinearity(pmCy_bglmm1) # Ok.


### *** 1.1.2.2. Distribution (family, ZI, dispersion) ----
# Theoretical count distribution:
theo_count <- rpois(n = nrow(pm), lambda = mean(pm$clutch_size))
tc_df <- data.frame(theo_count)

ggplot2::ggplot(pm, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that clutch_size may not be following a Poisson distribution: it seems slightly
# underdispersed and relatively symmetrical.
performance::check_distribution(pmCy_bglmm1)

# Zero-inflation:
theo_count_zi <- VGAM::rzipois(n = nrow(pm), lambda = mean(pm$clutch_size), pstr0 = 0.01) # Zero-
# inflation probability of 1%.
tczi_df <- data.frame(theo_count_zi)

ggplot2::ggplot(pm, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#33CCFF") +
  ggplot2::geom_bar(data = tczi_df, ggplot2::aes(theo_count_zi, fill="#CC3333", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that Y is not following a zero-inflated Poisson distribution (with p=0.01).

# Assessing over or under-dispersion:
aods3::gof(pmCy_bglmm1)
# The sum of squared Pearson residuals is less than the residual degrees of freedom, it's a known
# sign of underdispersion! We can confirm it by dividing the residual deviance by the number of degrees
# of freedom: 65.7/245=0.27 (<< 1), so underdispersion!
AER::dispersiontest(object = pmCy_glm0, alternative = c("less"))
DHARMa::testDispersion(simu.resid)
# All methods indicate significantly under-dispersed data (explaining the lack of fit?).


### *** 1.1.2.3. Model goodness-of-fit (GOF) and performances ----
# GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(pmCy_bglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(pmCy_bglmm1)) # p = 1, indicating that
# the probability that the model does NOT fit the data is 1!

# Computing a pseudo-R2:
performance::r2_nakagawa(pmCy_bglmm1) # Nakagawa's pseudo-R2 for GLMM.

## Likelihood-ration tests (LRT) of GOF:
# For the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- DHARMa::simulateLRT(m0 = pmCy_glm0, m1 = pmCy_bglmm1, n = 250, seed = 882)
tictoc::toc() # DISCLAIMER: took ~1h25 to run.
# The test is NOT significant, meaning that H0 cannot be rejected and we cannot say that M1 better
# describes the data than M0 (the reduced model)!
## For the whole model:
pmCy_nullglm <- stats::glm(formula = clutch_size ~ 1, family = poisson, data = pm2)
res.LRT_null <- stats::anova(object = pmCy_nullglm, pmCy_glm0, test = "LRT")
# Here again, the test is NOT significant, meaning that the GLM does not a improve the model compared to
# a completely random model.


### *** 1.1.2.4. Posterior predictive simulations ----
# Predicted counts:
par(.pardefault)
obsprop <- prop.table(table(pm2$clutch_size))
sims <- stats::simulate(pmCy_bglmm1, nsim = 1000)
nsim1 <- colSums(sims == 1) # Number of ones (min obs value)
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim1)),
     ylab="Probability", xlab="Number of ones")
obs1 <- sum(pm2$clutch_size == 1)
points(obs1, 0.004, col="red", pch=16, cex=2) # See y values in obsprop

nsim8 <- colSums(sims == 8) # Number of eights (modal obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim8)),
     ylab="Probability", xlab="Number of eights")
obs8 <- sum(pm2$clutch_size == 8)
points(obs8, 0.26, col="red", pch=16, cex=2)

nsim12 <- colSums(sims == 12) # Number of twelves (max obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim12)),
     ylab="Probability", xlab="Number of twelves")
(obs12 <- sum(pm2$clutch_size == 12))
points(obs12, 0.13, col="red", pch=16, cex=2)
# These three examples confirm that the model does not fit the data at all!

# Among-nestbox variance:
sims2 <- simulate(pmCy_bglmm1,nsim=1000)
vfun <- function(x) {
  m_new <- update(pmCy_bglmm1, data = transform(pm2, clutch_size = x))
  pm2$.resid <- stats::residuals(m_new, "pearson")
  nestboxmeans <- plyr::ddply(pm2, "id_nestbox", plyr::summarise, mresid = mean(.resid))
  stats::var(nestboxmeans$mresid)
}
# tictoc::tic("vdist simulation") # To measure computing time.
# vdist <- sapply(sims2, vfun) # DISCLAIMER: very long simulation (~3h40)!
# tictoc::toc()
#
# par(.pardefault)
# pm2$.glmresid <- stats::residuals(pmCy_bglmm1, "pearson")
# obs_boxmeans <- plyr::ddply(pm2, "id_nestbox", plyr::summarise, mresid = mean(.glmresid))
# obs_boxvar <- stats::var(obs_boxmeans$mresid)
# par(las=1, bty="l")
# hist(vdist, breaks = 30, col = "gray", freq = FALSE, main = "",
#      xlab="Among-nestbox variance in residuals")
# par(xpd = NA) # Prevents point getting cut off at top of plot.
# points(obs_boxvar, 0.0011, col="red", pch=16, cex=2) # The second argument should be the RE variance
# # estimated by the model (i.e. variance of the RE reported in the summary).
# # Here again, the model does not model well!

# Try COMREGPOISSON !
# Try COMREGPOISSON !



##################### *------------------------------* #########################
##### * 1.2. Clutch size: LMM --------------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 1.2.1. Initial model fit ----
# __________________________________

# Since the data are under-dispersed but the response is rather symmetrical, I'll now try to fit a
# more simple linear mixed-model:
pmCy_lm0 <- lm(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                          manag_low + manag_high + light_pollution + noise_m +
                          cumdd_30 + father_cond + mother_cond + year,
                          data = pm2)
pmCy_lmm0 <- lme4::lmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                          manag_low + manag_high + light_pollution + noise_m +
                          cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                          data = pm2)
pmCy_lmm1 <- lme4::lmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                          manag_low + manag_high + light_pollution + noise_m +
                          cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox) + (1|site),
                          data = pm2)



### ** 1.2.2. Diagnostics and assumption checks ----
# __________________________________________________

# Fitted vs residuals:
plot(pmCy_lmm0, type=c("p","smooth"), col.line = 2) # Bibof.
# Scale-location plot:
plot(pmCy_lmm0,
     sqrt(abs(stats::resid(.)))~stats::fitted(.),
     type=c("p","smooth"), col.line=1)
# Linearity plots:
plot(pm2$logged_woodyveg, stats::residuals(pmCy_lmm0))
plot(pm2$logged_Fmetric, stats::residuals(pmCy_lmm0))
plot(pm2$father_cond, stats::residuals(pmCy_lmm0)) # Linearity seems ok.
# QQ plot:
stats::qqnorm(stats::residuals(pmCy_lmm0)) # Ok-ish but clear non-normality for the extreme values.
stats::qqline(stats::residuals(pmCy_lmm0))
# Residuals vs leverage:
plot(pmCy_lmm0, stats::rstudent(.) ~ stats::hatvalues(.))
cd <- stats::cooks.distance(pmCy_lmm0)
plot(cd)
pm2[which(cd>0.4),] # Should not be > 0.5!
# Voir comments in the raw DATA§§§§§


### LRT:
pmCy_nullglm <- lme4::lmer(clutch_size ~ 1 + (1|id_nestbox), data = pm2)
stats::anova(pmCy_nullglm, pmCy_lmm0)

pmCy_lmm1 <- lme4::lmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                          manag_low + manag_high + light_pollution + noise_m +
                          cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox) + (1|site),
                        data = pm2)
stats::anova(pmCy_lmm0, pmCy_lmm1)
performance::check_autocorrelation(pmCy_lmm1) # Kewa?


## Likelihood-ration tests (LRT) of GOF:
# For the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT (LM vs LMM0)")
res.LRT_re1 <- DHARMa::simulateLRT(m0 = pmCy_lm0, m1 = pmCy_lmm0, n = 500, seed = 42)
tictoc::toc() # DISCLAIMER: took ~9.97s to run.
tictoc::tic("Parametric bootstrap LRT (LMM0 vs LMM1)")
res.LRT_re2 <- DHARMa::simulateLRT(m0 = pmCy_lmm0, m1 = pmCy_lmm1, n = 500, seed = 963)
tictoc::toc() # DISCLAIMER: took ~17.81s to run.





### ** 1.XX. Inference and predictions ----
# _________________________________________

### *** 1.XXX Hypothesis testing ----
broom::tidy(mymodel) %>%
  broom::glance(mymodel)
# + WALD for fixed effects
# See bolker + delladata for other stuff in inference+predictions
# See bolker + delladata for other stuff in inference+predictions
# See bolker + delladata for other stuff in inference+predictions





########################## ************************************************* ###############################
# -------------------------------------- #
##### 2. Modelling hatching success #####
# -------------------------------------- #

##### * 2.1 Hatching success: Binomial GLMM ------------------------------------
# ---------------------------------------------------------------------------- #
### ** 2.1.1. Initial model fit ----
# __________________________________

## Fitting a regular binomial GLM:
pmHSy_glm1 <- stats::glm(brood_size/clutch_size ~ logged_woodyveg + logged_Fmetric +
                           urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                           cumdd_30 + father_cond + mother_cond + year,
                         weights = clutch_size, # Prior weights!
                         data = pm2, family = "binomial") # Weights should not
# be forgotten. Otherwise, the formulation should be: cbind(brood_size, clutch_size-brood_size)!

## Fitting an additive GLMM:
pmHSy_glmm1 <- lme4::glmer(brood_size/clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                             manag_low + manag_high + light_pollution + noise_m +
                             cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                           weights = clutch_size, data = pm2, family = "binomial")
# As there are convergence issues, I change the optimizer and increase iterations:
ss <- lme4::getME(pmHSy_glmm1, c("theta", "fixef"))
pmHSy_glmm2 <- update(pmHSy_glmm1, start=ss,
                      control=lme4::glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5))) # Convergence ok.

# # *** FURTHER TESTS *** #
# # So I'll try using the Gauss-Hermite quadrature (GHQ) for estimation:
# pmHSy_glmm2_bGHQ <- stats::update(pmHSy_glmm2, nAGQ = 10)
# summary(pmHSy_glmm2_bGHQ) # Same!
# # Remember that GHQ compute things differently!
# # Try all optimizers:
# pmHSy_glmm2_all <- lme4::allFit(pmHSy_glmm2)
# summary(pmHSy_glmm2_all) # All optimizers converge but give rather similar results, even though Nelder-
# # Mead appears to disagree.

## Fitting interactive (mediated) GLMMs:
pmHSy_glmm3 <- lme4::glmer(brood_size/clutch_size ~
                             scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
                             urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                             cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                           weights = clutch_size, data = pm2, family = "binomial",
                           control=lme4::glmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5)))
# Test by removing possible overly influential observations:
pm2_wo <- pm2[-c(188,185,86,50,186,27,70,87,187,58,45,151),]
pmHSy_glmm3_wo <- lme4::glmer(brood_size/clutch_size ~
                             scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
                             urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                             cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                           weights = clutch_size, data = pm2_wo, family = "binomial",
                           control=lme4::glmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5)))
summary(pmHSy_glmm3)$AIC
summary(pmHSy_glmm3_wo)$AIC # Strongly improved AIC, BIC, and deviance. However, it yields lower R2_glmm!
# Test with standardised predictors:
pmHSy_glmm3_std <- lme4::glmer(brood_size/clutch_size ~
                                 l_woodyveg_std * l_Fmetric_std +
                                 urban_int_std + manag_mid + manag_high + light_pol_std + noise_pol_std +
                                 temperature_std + patern_cond_std + matern_cond_std + year + (1|id_nestbox),
                              weights = clutch_size, data = pm2, family = "binomial",
                              control=lme4::glmerControl(optimizer="bobyqa",
                                                         optCtrl=list(maxfun=2e5)))
summary(pmHSy_glmm3)$AIC
summary(pmHSy_glmm3_std)$AIC # ???



### ** 2.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 2.1.2.1. Residuals inspection ----
## Traditional residuals:
par(.pardefault)
plot(pmHSy_glmm3, id = 0.05, idLabels = ~.obs) # Strange pattern.
pm2[c(27,45,50,58,87,186,187),] # Nestboxes with the lowest residuals = ~0% hatching success!
pm2[c(86,185,188),] # Nestboxes with the highest residuals = ~100% hatching success!
# Interestingly, they may belong to the same nestbox, suggesting a strong year effect.
performance::check_outliers(pmHSy_glmm3) # Obs. 58 and 70 as possible outliers!

# To further investigate patterns, I can plot the residuals against some predictors:
resid <- stats::resid(pmHSy_glmm3, type = 'deviance')
plot(x = pm2$noise_m, y = resid) # Only "light_pollution" and "noise_m" show slightly strange patterns.
# Possibly because "noise_m" should be transformed (standardisation?). Otherwise, most predictors only
# show a higher variability at medium values.
plot(pmHSy_glmm3, id_nestbox~stats::resid(.)) # Justification for the nestbox random effect (RE).
plot(pmHSy_glmm3, site~stats::resid(.)) # Interestingly, there are not that much among-sites variance.

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmHSy_glmm3, n = 1000, plot = FALSE)
par(.pardefault)
plot(simu.resid) # The QQplot is ok but there are significant quantile deviations detected, however it
# is perhaps not that bad?

# Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm2$coord_x, y = pm2$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(pmHSy_glmm3) # Significant autocorrelation, but the Durbin-Watson test
# is known to be very sensitive.
performance::check_collinearity(pmHSy_glmm3) # Ok.


### *** 2.1.2.2. Distribution and dispersion ----
## Assessing over or under-dispersion:
aods3::gof(pmHSy_glmm3) # Note that this test is meant for count data!
# The sum of squared Pearson residuals is less than the residual degrees of freedom, it's a known
# sign of underdispersion! We can confirm it by dividing the residual deviance by the number of degrees
# of freedom: 190.1/244=0.78 (<< 1), so underdispersion!
DHARMa::testDispersion(simu.resid) # Nope (but see the help page).
performance::check_overdispersion(x = pmHSy_glmm3) # Underdispersion?

## Distribution of the predicted probabilities:
par(.pardefault)
probabilities <- stats::predict(object = pmHSy_glmm3, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities)
hist(pm2$brood_size/pm2$clutch_size) # The model seems to nicely fit the observed proportions of hatching
# success. But then, why are estimated SE so high?


### *** 2.1.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
pm2 %>% dplyr::select(woodyveg_vw, pmF_d531_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d531_beta0,
                "Fmetric (log)" = log10(pmF_d531_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (log)" = log10(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(logit = log(probabilities/(1-probabilities))) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -logit)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = logit, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Seems ok.

## Using the residuals:
fitted <- predict(pmHSy_glmm3, type = "link") # Fitted predictions on the logit scale.
resid <- resid(pmHSy_glmm3, type = 'deviance')
d <- data.frame(cbind(fitted, resid))
d %>%
  ggplot2::ggplot(ggplot2::aes(fitted, resid))+
  ggplot2::geom_point() # Strange patterns, but residuals diagnostics for binomial GLMs is always tricky.



### *** 2.1.2.4. Goodness-of-fit (GOF) and performances ----
## GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(pmHSy_glmm3, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(pmHSy_glmm3)) # p = 0.99, indicating that there are
# no significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely
# complicated topic and interpretations are not straightforward.

## Computing a pseudo-R2:
performance::r2_nakagawa(pmHSy_glmm2) # [Additive model]: Marg_R2_glmm = 0.12; Cond_R2_glmm = 0.73.
performance::r2_nakagawa(pmHSy_glmm3) # [Interact. model]: Marg_R2_glmm = 0.12; Cond_R2_glmm = 0.73.

## Likelihood-ration tests (LRT) of GOF:
# Importance of the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- DHARMa::simulateLRT(m0 = pmHSy_glm1, m1 = pmHSy_glmm2, n = 500, seed = 51)
tictoc::toc() # Does not work...
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- pbkrtest::PBmodcomp(largeModel = pmHSy_glmm2, smallModel = pmHSy_glm1, nsim = 500, seed = 12)
tictoc::toc() # Does it work???
# Importance of the fixed effects:
pmHSy_glmm0 <- lme4::glmer(brood_size/clutch_size ~ 1 + (1|id_nestbox),
                           weights = clutch_size, data = pm2, family = "binomial",
                           control=lme4::glmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5)))
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- DHARMa::simulateLRT(m0 = pmHSy_glmm0, m1 = pmHSy_glmm2, n = 500, seed = 51)
tictoc::toc() # DISCLAIMER: took ~1h45 to run!
# The LRT is highly significant, suggesting that M1 better describes the data than M0!
# For the whole simple GLM model:
pmHSy_glm0 <- stats::glm(brood_size/clutch_size ~ 1,
                         weights = clutch_size, # Prior weights!
                         data = pm2, family = "binomial")
res.LRT_null <- stats::anova(object = pmHSy_glm0, pmHSy_glm1, test = "LRT")
# The test is significant, confirming that the model is useful to explain the data.





### ** 2.1.3. Inference and predictions ----
# __________________________________________

### *** 2.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----

tictoc::tic("Parametric bootstrap LRT for interaction effect")
res.LRT_inteff <- DHARMa::simulateLRT(m0 = pmHSy_glmm2, m1 = pmHSy_glmm3, n = 500, seed = 51)

tt <- as.data.frame(cbind(res.LRT_inteff$method,
                          res.LRT_inteff$data.name,
                          res.LRT_inteff$statistic,
                          res.LRT_inteff$p.value))
rownames(tt) <- NULL
tt %>% dplyr::rename("Method" = V1,
                     "Models" = V2,
                     "Log Likelihood (M1/M0)" = V3,
                     "p-value" = V4) -> tt
readr::write_csv2(x = tt, file = here::here("output", "tables", "res.pmHS_LRT_inteff.csv"))
tictoc::toc() # DISCLAIMER: took ~3h35 to run!
# If significant, I should compute bootCI for glmm3 parameters! But it's not.
# A FINIR§§§§§§§§
# A FINIR§§§§§§§§
# A FINIR§§§§§§§§
# INCLUDING LRT entre glmm1 (sans F-metric) et 2
# INCLUDING LRT entre glmm1 (sans F-metric) et 2
# INCLUDING LRT entre glmm1/0 (sans F-metric) et 2
# tester si les résultats sont identiques avec pbkrtest::PBmodcomp()!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# A FINIR§§§§§§§§
# A FINIR§§§§§§§§


### *** 2.1.3.2. Bootstrapped confidence intervals for estimated parameters ----



tictoc::tic("Bootstrap CI for additive GLMM parameters")
pmHSy_glmm2_CI_boot <- confint(pmHSy_glmm2, method="boot")
tictoc::toc() # DISCLAIMER: took ~2h10 to run!
# A FINIR§§§§§§§§
# A FINIR§§§§§§§§
# A FINIR§§§§§§§§
# A FINIR§§§§§§§§
# A FINIR§§§§§§§§
###################### FAIRE diagnostics+boot CI (si LRT ***) sur modèle interactifffff??????? -> glmm3?






########################## ************************************************* ###############################
par(.pardefault)
fitdistrplus::plotdist(data = pm2$brood_size, histo = TRUE, demp = TRUE) # In case of NA's, I can use
# 'na.omit()' in the call. This function works better for continuous unbounded variables.

######################################### TO DO LIST ####################################################
# 4) Try Sdt like NOVA with MEDIAN and 1.5*IQR??? (First step toward convergence according to Bolker)!
# 5) Move on to another Y.
# 6) Try the Conway-Maxwell Poisson (Shmueli et al, 2005*) avec {COMPoissonReg} (Sellers & Lotze, 2015*),
#    but search first if that exist in Mixed Model! (Same for Zero-Inflated models)!

# 7) Reunite PM & CC while keeping other predictors (for exploratory research)! But beware of coding,
#    what does the intercept mean? Which reference group?

# 1) Consider removing overly influential observations?
# 8) Compare all methods of estimation and inference (cf. inference bolker example) for the **same
#    model** to see if any is making a difference?
# 9) Explore other predictors!
########################## ************************************************* ###############################
