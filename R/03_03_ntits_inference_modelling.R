########################## *--------------------------------------------------* ########################
########################## Inferential modelling for all tit nestlings (ntits)  ########################
########################## *--------------------------------------------------* ########################

# The functions of this R file are meant to wrap all formal inference modelling made in this study, as
# opposed to "preparation modelling" (cf. previous scripts) and "exploratory modelling" that will perhaps
# be made afterwards; i.e. after we have formally tested our research hypotheses in a robust inferential
# framework. The so-called "exploratory modelling" is related to additional questions and perspectives
# that will have, by nature, far less support as the data have already been used for formal testing (so
# type-I error rates cannot be guaranteed anymore)!

# For now, this script should be run after having sourced the EDA script (because no function yet)!!!





# -------------------------------- #
##### 1. Modelling clutch size #####
# -------------------------------- #

##### * 1.1. Clutch size: Poisson GLMM -----------------------------------------
# ---------------------------------------------------------------------------- #
### ** 1.1.1. Initial model fit ----
# __________________________________

# # I first started with the full model including the interaction term, but it appears to be too complex
# # for the data, so I'll switch to trying to properly fit the full additive-only model first:
# pmCy_glmm0 <- lme4::glmer(clutch_size ~ woodyveg_vw + pmF_d60_beta0 + urban_intensity + manag_low +
#                                 manag_high + light_pollution + noise_m + cumdd_30 +
#                                 father_cond + mother_cond + (1|id_nestbox) + (1|breeding_window),
#                               data = pm2, family = poisson) # See ?glmerControl and Bolker's examples
# # in case of fitting warnings and errors: e.g. convergence, etc.
# # I get the following warnings:
# # "boundary (singular) fit: see help('isSingular')
# # Warning message:
# #   Some predictor variables are on very different scales: consider rescaling".
# # So my model is singular and some variables should be rescaled!
#
# # So I'll try by log-transforming some variables:
# pmCy_glmm1 <- lme4::glmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
#                                 manag_low + manag_high + light_pollution + noise_m +
#                                 cumdd_30 + father_cond + mother_cond + (1|id_nestbox) +
#                                 (1|breeding_window),
#                               data = pm2, family = poisson)
# # lme4::VarCorr(pmCy_glmm1)
# # It works but gives a singular fit (i.e. random effect variance = 0)!
#
# # So I'll continue by dropping "breeding_window" as random effect and using year as fixed effect instead:
# pmCy_glmm2 <- lme4::glmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
#                                 manag_low + manag_high + light_pollution + noise_m +
#                                 cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
#                               data = pm2, family = poisson)
# # Still singular, so "id_nestbox" is problematic as well!
#
# # I will then try to better tune the models parameters. First, by trying using the "bobyqa" optimizer:
# pmCy_glmm2_b <- stats::update(pmCy_glmm2, control = lme4::glmerControl(optimizer = "bobyqa"))
# # Does not change a thing! So I'll try using the Gauss-Hermite quadrature (GHQ) for estimation:
# pmCy_glmm2_bGHQ <- stats::update(pmCy_glmm2_b, nAGQ = 10)
# # Changing the fitting method strongly modified the AIC/BIC, log-likelihood and deviance but changed
# # absolutely NOTHING to the parameter estimates! That is simply because GHQ computes things differently.
# # I'll try fitting a simple GLM (without 'id_nestbox') to compare:
# pmCy_glm0 <- stats::glm(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
#                           manag_low + manag_high + light_pollution + noise_m +
#                           cumdd_30 + father_cond + mother_cond + year,
#                         data = pm2, family = poisson)
# summary(pmCy_glm0)
# # Switching to a simple GLM gives absolutely identical estimates with the first GLMMs (with Laplace
# # fits), indicating that the in the absence of variance in the random effects, glmer() was actually
# # computing a simple GLM this whole time!
# # Even then, estimates and p-values are extremely surprising as only one effect is significant.

# [LATER NOTE: I transformed previous parts into comments because it is not always useful to run them again.
# I only wish to keep track of these stages].
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
pmCy_bglmm1 <- stats::update(pmCy_bglmm0, start=ss,
                             control=lme4::glmerControl(optimizer="bobyqa",
                                                        optCtrl=list(maxfun=2e5)))
summary(pmCy_bglmm1) # It now converges and throws no warnings but estimates are still disappointing!
# # Try all optimizers:
# pmCy_bglmm_all <- lme4::allFit(pmCy_bglmm1)
# summary(pmCy_bglmm_all) # Only "bobyqa" converges but this lack of convergence does not seem to matter
# # much as values from all optimizers do not seem to vary much (e.g. logLik, estimates...) and are
# # still wonky.
#
# # Perhaps, with GHQ:
# pmCy_bglmm2 <- stats::update(pmCy_bglmm1, nAGQ = 10)
# summary(pmCy_bglmm2) # Nope...
#
# # As apparently, lme4 output values from models fitted with GHQ are not comparable with models fitted
# # with Laplace approximation or with simple GLMs, I cannot compare the "GHQ model" with the others:
# AICcmodavg::aictab(cand.set = list(pmCy_glmm0, pmCy_glmm1, pmCy_glmm2, pmCy_glmm2_b),
#                    modnames = c("glmm0", "glmm1", "glmm2", "glmm2_b")) # Only works for same type models,
# # so I cannot compare with the "bglmm" but it doesn't change much anyway.





### ** 1.1.2. Diagnostics and assumption checks ----
# ________________________________________________

### *** 1.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
plot(pmCy_bglmm1, id = 0.05, idLabels = ~.obs) # Ok-ish but there are clear extreme values.
# Remember, however, that interpreting regular (deviance or Pearson) residuals from GLMs is notoriously
# tricky, hence the later use of simulated residuals with {DHARMa} (see below).
pm2[c(236,198,233),] # Nestboxes with the lowest residuals = very small clutch sizes.
performance::check_outliers(pmCy_bglmm1) # Does not work for this type of model.

# To further investigate patterns, I can plot the residuals against some predictors:
resid <- stats::resid(pmCy_bglmm1, type = 'deviance')
plot(x = pm2$noise_m, y = resid) # Only "light_pollution" and "noise_m" show slightly strange
# patterns. Possibly because "noise_m" should be transformed (standardisation?). It could be a sign for
# heteroscedasticity. Otherwise, seems ok (but, once again, simulated residuals will be more useful).
plot(pmCy_bglmm1, id_nestbox~stats::resid(.)) # Justification for the nestbox random effect (RE).
plot(pmCy_bglmm1, site~stats::resid(.)) # That's interesting because it shows that there's quite a lot
# of among and within sites variability in the residuals. It may mean a "site" RE is required.
# Note also that "chateau_de_larrey" is an 'outlier' site and 3 other sites have extreme values
# ("seminaire", "chateau_de_pouilly" and "arquebuse").

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmCy_bglmm1, n = 1000, re.form = NULL) # The 're.form'
# argument is to base simulations on the model unconditional of the random effects (and only works for
# {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Not ok! There is a clear sign of underdispersion (more observations around 0.5 than
# expected) and significant tests! Possible sources of underdispersion are overfitting, zero-inflation,
# autocorrelation and the fact that the response is not generated by a Poisson process.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm2$coord_x, y = pm2$coord_y, plot = TRUE) # Significant spatial
# autocorrelation detected. Add a "site" RE?
performance::check_autocorrelation(pmCy_bglmm1) # Same!
performance::check_collinearity(pmCy_bglmm1) # Ok-ish, but "urban_intensity" and the "F-metric" are > 3!
stats::vcov(pmCy_bglmm1) # But values of the covariance matric seem ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_woodyveg)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_Fmetric)
DHARMa::plotResiduals(simu.resid, form = pm2$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$light_pollution)
DHARMa::plotResiduals(simu.resid, form = pm2$noise_m)
DHARMa::plotResiduals(simu.resid, form = pm2$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = pm2$father_cond)
DHARMa::plotResiduals(simu.resid, form = pm2$mother_cond)
DHARMa::plotResiduals(simu.resid, form = pm2$year)
# All these plots indicate underdispersion, with residuals too condensed around 0.5. Otherwise, no strange
# patterns could be seen but it is possible that patterns are hidden by the underdispersion of the points.



### *** 1.1.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
aods3::gof(pmCy_bglmm1)
# The sum of squared Pearson residuals is less than the residual degrees of freedom, it's a known
# sign of underdispersion! We can confirm it by dividing the residual deviance by the number of degrees
# of freedom: 65.7/245=0.27 (<< 1), so underdispersion!
AER::dispersiontest(object = pmCy_glm0, alternative = c("less")) # Significant underdispersion!
DHARMa::testDispersion(simu.resid) # Same.
# All methods indicate significantly under-dispersed data (possibly explaining the lack of fit?).

## Theoretical count distribution:
theo_count <- rpois(n = nrow(pm), lambda = mean(pm$clutch_size))
tc_df <- data.frame(theo_count)

ggplot2::ggplot(pm, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that clutch_size may not be following a Poisson distribution: it seems slightly
# underdispersed and relatively symmetrical.
performance::check_distribution(pmCy_bglmm1) # Not very helpful.
# As there seems to be an inflation in 7, 8 and 9 counts, I could also test it using the 'testGeneric()'
# function of {DHARMa}, but I don't see the point here, these count are clearly naturally inflated!

# Distribution of the predicted counts:
par(.pardefault)
pred_counts <- stats::predict(object = pmCy_bglmm1, type = "response") # Extract the predicted counts.
par(mfrow= c(1,2))
hist(pred_counts)
hist(pm2$clutch_size) # The model does not predict well!

## Zero-inflation:
theo_count_zi <- VGAM::rzipois(n = nrow(pm), lambda = mean(pm$clutch_size), pstr0 = 0.01) # Zero-
# inflation probability of 1%.
tczi_df <- data.frame(theo_count_zi)

ggplot2::ggplot(pm, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#33CCFF") +
  ggplot2::geom_bar(data = tczi_df, ggplot2::aes(theo_count_zi, fill="#CC3333", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that Y is not following a zero-inflated Poisson distribution (with p=0.01). See also:
DHARMa::testZeroInflation(simu.resid) # Nope. However, to get more conclusive results, I should try to
# fit a zero-inflated (ZI) model and compare it:
pmCy_ziglmm1 <- glmmTMB::glmmTMB(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                   manag_low + manag_high + light_pollution + noise_m +
                                   cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                 ziformula = ~1, # Specifies a null ZI model.
                                 family = "poisson", data = pm2)
summary(pmCy_ziglmm1) # Results are rather similar but the RE variance is too low!
simu.resid_zi <- DHARMa::simulateResiduals(fittedModel = pmCy_ziglmm1, n = 1000)
plot(simu.resid_zi) # Does not fix the problems, so ZI is not the issue here!



### *** 1.1.2.3. Linearity ----
## Plotting the response on the log scale against predictors:
# Format data:
pm2 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (log)" = log10(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind log(Y) and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(log_y = log(pm2$clutch_size)) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity seems respected but the patterns are very
# strange indeed...



### *** 1.1.2.4. Model goodness-of-fit (GOF) and performances ----
# GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(pmCy_bglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(pmCy_bglmm1)) # p = 1, which is probably a mistake?

# Computing a pseudo-R2:
performance::r2_nakagawa(pmCy_bglmm1) # [Additive model]: Marg_R2_glmm = 0.06; Cond_R2_glmm = 0.07.

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



### *** 1.1.2.5. Posterior predictive simulations ----
# Predicted counts:
par(.pardefault)
obsprop <- prop.table(table(pm2$clutch_size))
sims <- stats::simulate(pmCy_bglmm1, nsim = 1000)
nsim1 <- colSums(sims == 1) # Number of ones (min obs value)
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim1)),
     ylab="Probability", xlab="Number of ones")
obs1 <- sum(pm2$clutch_size == 1)
points(obs1, 0.004, col="red", pch=16, cex=2) # See the y value (0.004) in 'obsprop'!

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
  m_new <- stats::update(pmCy_bglmm1, data = transform(pm2, clutch_size = x))
  pm2$.resid <- stats::residuals(m_new, "pearson")
  nestboxmeans <- plyr::ddply(pm2, "id_nestbox", plyr::summarise, mresid = mean(.resid))
  stats::var(nestboxmeans$mresid)
}
# tictoc::tic("vdist simulation")
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



### *** 1.1.2.6. Conclusion ----

# Our diagnostics clearly show that the data are underdispersed, explaining why our models do not fit.
# We also saw other problems such as outliers and spatial autocorrelation, but the extent to which these
# issues are linked to the underdispersion is not clear.
# Clutch size should thus be modelled using a generalized-Poisson model or a Conway-Maxell Poisson model,
# and diagnostics should be run again.





########### *-----------------------------------------------------* ############
##### * 1.2. Clutch size: COM-Poisson GLMM -------------------------------------
# ---------------------------------------------------------------------------- #
### ** 1.2.1. Initial model fit ----
# __________________________________

## Fitting a regular Poisson regression:
pmCy_glm1 <- stats::glm(clutch_size ~ logged_woodyveg + logged_Fmetric +
                          urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                          cumdd_30 + father_cond + mother_cond + year,
                        data = pm2, family = "poisson")

## Fitting a regular Conway-Maxwell (COM) Poisson regression:
pmCy_comp1 <- COMPoissonReg::glm.cmp(formula.lambda =
                                       clutch_size ~ logged_woodyveg + logged_Fmetric +
                                       urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                                       cumdd_30 + father_cond + mother_cond + year,
                                     data = pm2, formula.nu = ~1) # Intercept only 'nu' (default).
summary(pmCy_glm1)
summary(pmCy_bglmm1)
summary(pmCy_comp1)
# The COM-Poisson GLM displays much improved coefficients and AIC values (AIC=976.5) compared to the regular
# Poisson GLM (AIC=1122.3) and the Poisson bGLMM (AIC=1125.8)! That's an improvement! Now let's see if the
# addition of random effect(s) (RE) further improves the fit.

## Fitting a COM-Poisson mixed regression model (COM-Poisson GLMM):
pmCy_comglmm1 <- glmmTMB::glmmTMB(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                    manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                  data = pm2, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1) # Intercept only 'nu' (default). Rather long to run!
summary(pmCy_comglmm1) # Gives AIC = 975 but a very small variance for the RE.

pmCy_comglmm3 <- glmmTMB::glmmTMB(clutch_size ~
                                    scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
                                    urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                  data = pm2, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1)
summary(pmCy_comglmm3) # Gives AIC = 977.





### ** 1.2.2. Diagnostics and assumption checks ----
# ________________________________________________

### *** 1.2.2.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(pmCy_comglmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Ok-ish but there are a few low residuals.
# performance::check_outliers(pmCy_comglmm1) # Does not work for this type of model.
pm2[which(resid < -2.1),] # Five potential outliers: nestboxes with very small clutch sizes.

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = pm2$father_cond, y = resid) # There may be signs of heteroscedasticity for some predictors:
# "F-metric", "noise_m", and "cumdd_30". Otherwise, seems ok (but, once again, simulated residuals will be
# more useful).
# plot(pmCy_comglmm1, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(pmCy_comglmm1, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmCy_comglmm1, n = 1000, re.form = NULL) # The 're.form'
# argument is to base simulations on the model unconditional of the random effects (and only works for
# {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # The outlier test is significant, but otherwise it is ok!
DHARMa::outliers(simu.resid) # Three potential outliers.
pm2[c(198,233,236),] # They're from "chateau_de_pouilly", "seminaire" and "chateau_de_larrey" and indeed, they
# have surprisingly low clutch sizes with regards to their locations and their adjacent observations. They may
# well be true outliers (whose clutch sizes are function of other processes).

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm2$coord_x, y = pm2$coord_y, plot = TRUE) # Significant spatial
# autocorrelation detected. Add a "site" RE?
performance::check_autocorrelation(pmCy_comglmm1) # Ok.
performance::check_collinearity(pmCy_comglmm1) # Ok-ish, but "urban_intensity" > 3!
stats::vcov(pmCy_comglmm1) # But values of the covariance matrix seem ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_woodyveg)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_Fmetric)
DHARMa::plotResiduals(simu.resid, form = pm2$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$light_pollution)
DHARMa::plotResiduals(simu.resid, form = pm2$noise_m)
DHARMa::plotResiduals(simu.resid, form = pm2$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = pm2$father_cond)
DHARMa::plotResiduals(simu.resid, form = pm2$mother_cond)
DHARMa::plotResiduals(simu.resid, form = pm2$year)
# All these plots are ok, the model seems to better model the dispersion.



### *** 1.2.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
aods3::gof(pmCy_comglmm1)
# The sum of squared Pearson residuals is less than the residual degrees of freedom, it's a known
# sign of underdispersion! We can confirm it by dividing the residual deviance by the number of degrees
# of freedom: 65.7/245=0.27 (<< 1), so underdispersion!
AER::dispersiontest(object = pmCy_glm0, alternative = c("less")) # Significant underdispersion!
DHARMa::testDispersion(simu.resid, alternative = "less") # Ok, but could be better.
# There still may be a lower dispersion than expected.

## Theoretical count distribution:
theo_count <- COMPoissonReg::rcmp(n = nrow(pm), lambda = mean(pm$clutch_size), nu = 1.2) # The 'nu' parameter
# should be chosen by trial-and-errors.
tc_df <- data.frame(theo_count)

ggplot2::ggplot(pm, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that clutch_size coulb be following a COM-Poisson distribution of parameter nu~1.2,
# especially if we consider that the lowest values are outliers!

## Distribution of the predicted counts:
pred_counts <- stats::predict(object = pmCy_comglmm1, type = "response") # Extract the predicted counts.
par(mfrow= c(1,2))
hist(pred_counts)
hist(pm2$clutch_size) # The model's predictions are quite good.

## Zero-inflation:
theo_count_zi <- COMPoissonReg::rzicmp(n = nrow(pm), lambda = mean(pm$clutch_size), nu = 1.2, p = 0.05)
tczi_df <- data.frame(theo_count_zi)

ggplot2::ggplot(pm, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#33CCFF") +
  ggplot2::geom_bar(data = tczi_df, ggplot2::aes(theo_count_zi, fill="#CC3333", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that Y is not following a zero-inflated Poisson distribution (with p=0.01). See also:
DHARMa::testZeroInflation(simu.resid) # Nope. However, to get more conclusive results, I should try to
# fit a zero-inflated (ZI) model and compare it:
pmCy_zicomglmm1 <- glmmTMB::glmmTMB(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                      manag_low + manag_high + light_pollution + noise_m +
                                      cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                    data = pm2, family = glmmTMB::compois(link = "log"),
                                    dispformula = ~1,
                                    ziformula = ~1) # Specifies a null ZI model.
summary(pmCy_zicomglmm1) # Results are rather similar but the RE variance is too low!
simu.resid_zi <- DHARMa::simulateResiduals(fittedModel = pmCy_zicomglmm1, n = 1000)
plot(simu.resid_zi) # Worse than before!



### *** 1.2.2.3. Linearity ----

# IMPORTANT NOTE: I already checked the linearity of log(Y)~Xs, so here I look at the predicted counts!
pm2 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (sqrt)" = sqrt(pmF_d60_beta0),
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (sqrt)" = sqrt(woodyveg_vw),
                "woodyveg_vw (log)" = log10(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind log(Y) and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(Predicted_counts = pred_counts) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -Predicted_counts)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = Predicted_counts, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity seems respected but the patterns are still
# quite surprising. Using spline functions would probably give better results (e.g. GAMM).



### *** 1.2.2.4. Model goodness-of-fit (GOF) and performances ----
# GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(pmCy_comglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(pmCy_comglmm1)) # p = 0.99, indicating that there is no
# significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely complicated
# topic and interpretations are not straightforward.

# Computing a pseudo-R2:
performance::r2_nakagawa(pmCy_comglmm1) # [Additive model]: Marg_R2_glmm = 0.06; Cond_R2_glmm = 0.03.

## Likelihood-ration tests (LRT) of GOF:
# For the random-effects (RE):
pmCy_comglmm0 <- glmmTMB::glmmTMB(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                    manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox) + (1|site),
                                  data = pm2, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1)
summary(pmCy_comglmm0) # The non-mixed model gives AIC = 976 so approximatively equal to the mixed-model with
# "id_nestbox" as RE. The mixed model with "site" as a RE gives AIC = 975.3. The one with both RE gives
# AIC = 976. So it seems like the use of mixed models is here not supported by the data!

## For the whole model:
pmCy_comglmm0 <- glmmTMB::glmmTMB(clutch_size ~ 1,
                                  data = pm2, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1)
res.LRT_null <- stats::anova(object = pmCy_comglmm0, pmCy_comglmm1, test = "LRT")
# The test is significant, confirming that the model is useful to explain the data.




### *** 1.2.2.5. Posterior predictive simulations ----
# Predicted counts:
par(.pardefault)
obsprop <- prop.table(table(pm2$clutch_size))
sims <- stats::simulate(pmCy_comglmm1, nsim = 1000)
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
# These three examples confirm that the model still predicts values that are too dispersed compared to the
# true observed values.





### ** 1.2.3. Inference and predictions ----
# __________________________________________

### *** 1.2.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## Parametric bootstrap to test the additive effect of the connectivity metric:
pmCy_comglmm0 <- stats::update(pmCy_comglmm1, .~. -logged_Fmetric)

tictoc::tic("Parametric bootstrap LRT for the additive effect")
res.LRT_inteff <- DHARMa::simulateLRT(m0 = pmCy_comglmm0, m1 = pmCy_comglmm1, n = 500, seed = 10)
tt <- as.data.frame(cbind(res.LRT_inteff$method,
                          res.LRT_inteff$data.name,
                          res.LRT_inteff$statistic,
                          res.LRT_inteff$p.value))
rownames(tt) <- NULL
tt %>% dplyr::rename("Method" = V1,
                     "Models" = V2,
                     "Log Likelihood (M1/M0)" = V3,
                     "p-value" = V4) -> tt
readr::write_csv2(x = tt, file = here::here("output", "tables", "res.pmCy_LRT_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~3h35 to run!
# The PB-based LRT is non-significant, indicating that our connectivity metric does not improve the
# description of the data here.
# NOTE: initially, normally I would use the more efficient 'pbkrtest::PBmodcomp()' instead of the
# 'DHARMa::simulateLRT()' function, but it doesn't work with {glmmTMB} objects.


## Parametric bootstrap to test the interactive effect of the connectivity metric:
# To save some time, I don"t even bother computing the PB-based LRT for the interaction effect, it won't be
# significant. I should improve my proxies and my models first.

##### TO FINISH RUNNING §§§ ----
# J'ai du stoppé le LRT après ~25H!!!!!!!!!!!
# J'ai du stoppé le LRT après ~25H!!!!!!!!!!!
# J'ai du stoppé le LRT après ~25H!!!!!!!!!!!

### *** 1.2.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for the additive COM-Poisson GLMM parameters")
res.pmCy_addeff_CI_boot <- confint(pmCy_comglmm1, method="boot")
tt <- as.data.frame(res.pmCy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmCy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~2h10 to run!



### *** 1.2.3.3. Conclusion ----

summary(pmCy_comglmm1)
# Our diagnostics show that the use of a COM-Poisson regression strongly improves models predictive accuracy.
# However, improvements are still likely possible as the models still tend to predict a wider count-range
# than the observed one. Possible leads for improvement could be to remove the possible outliers, merge
# observations, and try to improve the modelling of the dispersion (nu) parameter (some exploration showed
# that several predictors could here be significant: e.g. "F_metric", "cumdd_30", "manag_low", "year")!
# As they are, unfortunately, the models do not support our hypotheses and only four predictors turned out
# significant: "manag_high", "cumdd_30", "mother_cond", and "year2021"; while the lowest value of AIC ~ 975.





########################## ************************************************* ###############################
# -------------------------------------- #
##### 2. Modelling hatching success #####
# -------------------------------------- #

##### * 2.1. Hatching success: Binomial GLMM ------------------------------------
# ---------------------------------------------------------------------------- #
### ** 2.1.1. Initial model fit ----
# __________________________________

## Fitting a regular binomial GLM:
pmHSy_glm1 <- stats::glm(brood_size/clutch_size ~ logged_woodyveg + logged_Fmetric +
                           urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                           cumdd_30 + father_cond + mother_cond + year,
                         weights = clutch_size, # Prior weights!
                         data = pm2, family = "binomial") # Weights should not be forgotten. Otherwise, the
# formulation should be: cbind(brood_size, clutch_size-brood_size)! This models gives AIC = 968.6.

## Fitting an additive GLMM:
pmHSy_glmm1 <- lme4::glmer(brood_size/clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                             manag_low + manag_high + light_pollution + noise_m +
                             cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                           weights = clutch_size, data = pm2, family = "binomial")
# As there are convergence issues, I change the optimizer and increase iterations:
ss <- lme4::getME(pmHSy_glmm1, c("theta", "fixef"))
pmHSy_glmm2 <- stats::update(pmHSy_glmm1, start=ss,
                             control=lme4::glmerControl(optimizer="bobyqa",
                                                        optCtrl=list(maxfun=2e5))) # Convergence ok. AIC = 712.5.

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
                                                      optCtrl=list(maxfun=2e5))) # Yields an AIC = 713.7.
# IMPORTANT NOTE: as will be seen further down the diagnostic process, the true model should probably be
# zero-inflated (AIC = 563.4)!

# # Test by removing possible overly influential observations:
# pm2_wo <- pm2[-c(188,185,86,50,186,27,70,87,187,58,45,151),]
# pmHSy_glmm3_wo <- stats::update(pmHSy_glmm3, data=pm2_wo)
# summary(pmHSy_glmm3)$AIC
# summary(pmHSy_glmm3_wo)$AIC # Strongly improved AIC, BIC, and deviance. However, it yields lower R2_glmm!





### ** 2.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 2.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
plot(pmHSy_glmm3, id = 0.05, idLabels = ~.obs) # Strange pattern.
pm2[c(86,185,188),] # Nestboxes with the highest residuals = ~100% hatching success!
pm2[c(187,186,58,151,16,87,45,70,50,27),] # Nestboxes with the lowest residuals = ~0% hatching success!
# Interestingly, they may belong to the same nestbox, suggesting a strong year effect.
performance::check_outliers(pmHSy_glmm3) # Obs. 58 and 70 as possible outliers (but warnings)!

# To further investigate patterns, I can plot the residuals against some predictors:
resid <- stats::resid(pmHSy_glmm3, type = 'deviance')
plot(x = pm2$noise_m, y = resid) # Only "light_pollution" and "noise_m" show slightly strange patterns.
# Possibly because "noise_m" should be transformed (standardisation?). Otherwise, most predictors only
# show a higher variability at medium values.
plot(pmHSy_glmm3, id_nestbox~stats::resid(.)) # Justification for the nestbox random effect (RE).
plot(pmHSy_glmm3, site~stats::resid(.)) # Interestingly, there are not that much among-sites variance.

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmHSy_glmm3, n = 1000, seed = 37)
plot(simu.resid) # The QQplot is ok but there are significant quantile deviations detected with most
# simulated residuals > 0.5 for the lowest predictions, meaning that the model is consistently predicting
# lower values than the lowest observed proportions (unless I'm mistaken). This could be due to the ZI
# in "brood_size", i.e. the numerator of the modelled proportion.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm2$coord_x, y = pm2$coord_y, plot = TRUE) # Ok-ish.
performance::check_autocorrelation(pmHSy_glmm3) # Significant autocorrelation, but the Durbin-Watson test
# is known to be very sensitive.
performance::check_collinearity(pmHSy_glmm3) # Ok-ish, but "urban_intensity" > 4, and 2 others > 3!
stats::vcov(pmHSy_glmm2) # Mostly ok, but there are some high covariances with the intercept.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_woodyveg)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_Fmetric)
DHARMa::plotResiduals(simu.resid, form = pm2$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$light_pollution)
DHARMa::plotResiduals(simu.resid, form = pm2$noise_m)
DHARMa::plotResiduals(simu.resid, form = pm2$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = pm2$father_cond) # Signs of heteroscedasticity!
DHARMa::plotResiduals(simu.resid, form = pm2$mother_cond) # U-shaped: lacking quadratic effect?
DHARMa::plotResiduals(simu.resid, form = pm2$year) # Signs of heteroscedasticity!
# There are rather clear signs of heteroscedasticity that are possibly linked to the zero-inflation.



### *** 2.1.2.2. Distribution and dispersion ----
## Assessing over or under-dispersion:
aods3::gof(pmHSy_glmm3) # Note that this test is meant for count data!
# The sum of squared Pearson residuals is less than the residual degrees of freedom, it's a known
# sign of underdispersion! We can confirm it by dividing the residual deviance by the number of degrees
# of freedom: 190.1/244=0.78 (<< 1), so underdispersion!
DHARMa::testDispersion(simu.resid) # Nope (but see the help page).
performance::check_overdispersion(x = pmHSy_glmm3) # Ok-ish.

## Distribution of the predicted probabilities:
probabilities <- stats::predict(object = pmHSy_glmm3, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities)
hist(pm2$brood_size/pm2$clutch_size) # The model seems to fit the observed proportions rather nicely although
# it over-predicts total success and under-predicts the failures.

## Zero-inflation:
DHARMa::testZeroInflation(simu.resid) # Significant inflation detected!
# Fitting a zero-inflated (ZI) model and compare it:
pmHSy_ziglmm1 <- glmmTMB::glmmTMB(brood_size/clutch_size ~
                                    logged_woodyveg + logged_Fmetric +
                                    urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                  weights = clutch_size,
                                  data = pm2, family = binomial,
                                  ziformula = ~1) # Specifies a null ZI model.
summary(pmHSy_ziglmm1) # Seems to yield better results (e.g. RE variance). AIC = 563.4 (without ZI, AIC ~ 713)!
simu.resid_zi <- DHARMa::simulateResiduals(fittedModel = pmHSy_ziglmm1, n = 1000)
plot(simu.resid_zi) # The use of the ZI-binomial model seem to have solved the problems. To verify, I will
# look at the predictions from this new model:
probabilities <- stats::predict(object = pmHSy_ziglmm1, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities)
hist(pm2$brood_size/pm2$clutch_size) # The model seems to better fit the observed proportions, even though it
# still under-predicts the proportion of failures.



### *** 2.1.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
pm2 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (sqrt)" = sqrt(pmF_d60_beta0),
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (sqrt)" = sqrt(woodyveg_vw),
                "woodyveg_vw (log)" = log10(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(logit = log(probabilities/(1-probabilities))) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -logit)
# Create scatterplot:
ggplot2::ggplot(mydata, ggplot2::aes(y = logit, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Ok.



### *** 2.1.2.4. Goodness-of-fit (GOF) and performances ----
## GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(pmHSy_ziglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(pmHSy_ziglmm1)) # p = 0, likely a mistake?

## Computing a pseudo-R2:
performance::r2_nakagawa(pmHSy_glmm2) # [Additive model]: Marg_R2_glmm = 0.12; Cond_R2_glmm = 0.73.
performance::r2_nakagawa(pmHSy_ziglmm1) # [Additive model]: Marg_R2_glmm = 0.09; Cond_R2_glmm = 0.35 (but
# strange warning, so probably a mistake).

## Likelihood-ration tests (LRT) of GOF:
# Importance of the "id_nestbox" random-effect (RE):
pmHSy_ziglmm0 <- glmmTMB::glmmTMB(brood_size/clutch_size ~
                                    logged_woodyveg + logged_Fmetric +
                                    urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year,
                                  weights = clutch_size,
                                  data = pm2, family = binomial,
                                  ziformula = ~1)
summary(pmHSy_ziglmm0) # The non-mixed model gives AIC = 589.5 while the mixed-model gave AIC = 563.4, so the
# the use of the mixed model seems warranted by the data!

# # Importance of the fixed effects:
# pmHSy_ziglmm0 <- lme4::glmer(brood_size/clutch_size ~ 1 + (1|id_nestbox),
#                            weights = clutch_size, data = pm2, family = "binomial",
#                            control=lme4::glmerControl(optimizer="bobyqa",
#                                                       optCtrl=list(maxfun=2e5)))
# tictoc::tic("Parametric bootstrap LRT")
# res.LRT_re <- DHARMa::simulateLRT(m0 = pmHSy_ziglmm0, m1 = pmHSy_ziglmm1, n = 500, seed = 85)
# tictoc::toc() # DISCLAIMER: took ~1h45 to run!
# # The LRT is highly significant, suggesting that M1 better describes the data than M0!





### ** 2.1.3. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ ----

### *** 2.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## Parametric bootstrap to test the additive effect of the connectivity metric:
pmHSy_ziglmm0 <- stats::update(pmHSy_ziglmm1, .~. -logged_Fmetric)

tictoc::tic("Parametric bootstrap LRT for the interaction model")
res.LRT_inteff <- DHARMa::simulateLRT(m0 = pmHSy_ziglmm0, m1 = pmHSy_ziglmm1, n = 500, seed = 21)
tt <- as.data.frame(cbind(res.LRT_inteff$method,
                          res.LRT_inteff$data.name,
                          res.LRT_inteff$statistic,
                          res.LRT_inteff$p.value))
rownames(tt) <- NULL
tt %>% dplyr::rename("Method" = V1,
                     "Models" = V2,
                     "Log Likelihood (M1/M0)" = V3,
                     "p-value" = V4) -> tt
readr::write_csv2(x = tt, file = here::here("output", "tables", "res.pmHSy_LRT_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~3h35 to run!
# NOTE: initially, normally I would use the more efficient 'pbkrtest::PBmodcomp()' instead of the
# 'DHARMa::simulateLRT()' function, but it doesn't work with {glmmTMB} objects.


## Parametric bootstrap to test the interactive effect of the connectivity metric:
# To save some time, I don"t even bother computing the PB-based LRT for the interaction effect, it won't be
# significant. I should improve my proxies and my models first.



### *** 2.1.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive GLMM parameters")
res.pmHSy_addeff_CI_boot <- confint(pmHSy_ziglmm1, method="boot")
tt <- as.data.frame(res.pmHSy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmHSy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~2h10 to run!



### *** 2.1.3.3. Conclusion ----

summary(pmHSy_glmm2)
# Our models fit the data quite nicely but, unfortunately, our hypotheses were not validated and only one
# variable turned out significant: "father_cond".





########## *-------------------------------------------------------* ###########
##### * 2.2. Brood size: ZICOM-Poisson GLMM ------------------------------------
# ---------------------------------------------------------------------------- #
### ** 2.2.1. Initial model fit ----
# __________________________________

# Given the look of the response variable and my previous diagnostics, I chose to directly compare various
# structural model types before even starting the dedicated diagnostics.

## Fitting a regular Poisson GLM:
pmBSy_glm1 <- stats::glm(brood_size ~ logged_woodyveg + logged_Fmetric +
                           urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                           cumdd_30 + father_cond + mother_cond + year,
                         data = pm2, family = "poisson")

## Fitting a regular Poisson GLMM:
pmBSy_glmm1 <- glmmTMB::glmmTMB(brood_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                  manag_low + manag_high + light_pollution + noise_m +
                                  cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                data = pm2, family = "poisson")

## Fitting a Zero-Inflated (ZI) Poisson GLMM:
pmBSy_ziglmm1 <- glmmTMB::glmmTMB(brood_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                    manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                  data = pm2, family = "poisson",
                                  ziformula = ~1)

## Fitting a Zero-Inflated (ZI) COM-Poisson GLMM:
pmBSy_zicomglmm1 <- glmmTMB::glmmTMB(brood_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                       manag_low + manag_high + light_pollution + noise_m +
                                       cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                     data = pm2, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~1,
                                     ziformula = ~1) # Rather long to fit.

summary(pmBSy_glm1) # AIC = 1293.3
summary(pmBSy_glmm1) # AIC = 1295.3
summary(pmBSy_ziglmm1) # AIC = 1180.3
summary(pmBSy_zicomglmm1) # AIC = 1072.2
# It seems that, if the inclusion of a random effect (RE) did not improve the fit, accounting for both the
# zero-inflation and the likely underdispersion strongly improved the fit! I will thus carry on with the last
# model to the diagnostic part and assess whether the use of the RE is truly justified or not and if the
# model behaves as expected.
pmBSy_zicomglmm2 <- glmmTMB::glmmTMB(brood_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                       manag_low + manag_high + light_pollution + noise_m +
                                       cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                     data = pm2, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~father_cond,
                                     ziformula = ~father_cond+cumdd_30) # Rather long to fit.
summary(pmBSy_zicomglmm2) # AIC = 1065.9 vs 1072.2, so slightly better.
# NOTE: After a few attempts to tune the model, I found that this specification slightly improved the model.
# But as it is a kind of data-dredging, I prefer carry on with my initially intended model. Still, I will
# perform diagnostics on both models. This 2nd model will be sometimes called ""exploratory improved model".

# # The interactive (mediated) model:
# pmBSy_zicomglmm3 <- glmmTMB::glmmTMB(brood_size ~
#                                        scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
#                                        urban_intensity + manag_low + manag_high + light_pollution + noise_m +
#                                        cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
#                                      data = pm2, family = glmmTMB::compois(link = "log"),
#                                      dispformula = ~1,
#                                      ziformula = ~1) # Rather long to fit.
# summary(pmBSy_zicomglmm3) # AIC = 1074.2, doesn't seem supported by the data.





### ** 2.2.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 2.2.2.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(pmBSy_zicomglmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Ok-ish but there are a few potential outliers.
# performance::check_outliers(pmBSy_zicomglmm1) # Does not work for this type of model.
pm2[which(resid < -4),] # As could have been guessed from the plot, the 2nd group (with the lowest residuals)
# consist of all the observations where "brood_size" ~ 0. It could be a sign that the zero-part of the model
# should indeed be modelled with relevant predictors. However, we do not seem to have the relevant predictors
# since the patterns still exist even with the "exploratory improved model" (i.e. 'pmBSy_zicomglmm2').

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = pm2$logged_Fmetric, y = resid) # There may be signs of heteroscedasticity for the "F-metric".
# Otherwise, it seems ok (but, once again, simulated residuals will be more useful).
# plot(pmBSy_zicomglmm1, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(pmBSy_zicomglmm1, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmBSy_zicomglmm1, n = 1000, re.form = NULL) # The
# 're.form' argument is to base simulations on the model unconditional of the random effects (and only works
# for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Quantile deviation detected (but better for 'pmBSy_zicomglmm2')!
DHARMa::outliers(simu.resid) # None.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm2$coord_x, y = pm2$coord_y, plot = TRUE) # Slightly significant
# spatial autocorrelation detected. Add a "site" RE?
performance::check_autocorrelation(pmBSy_zicomglmm1) # Ok-ish!
performance::check_collinearity(pmBSy_zicomglmm1) # Ok-ish, but "urban_intensity" > 3!
stats::vcov(pmBSy_zicomglmm1) # But values of the covariance matrix seem ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_woodyveg)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_Fmetric)
DHARMa::plotResiduals(simu.resid, form = pm2$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$light_pollution)
DHARMa::plotResiduals(simu.resid, form = pm2$noise_m)
DHARMa::plotResiduals(simu.resid, form = pm2$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = pm2$father_cond)
DHARMa::plotResiduals(simu.resid, form = pm2$mother_cond)
DHARMa::plotResiduals(simu.resid, form = pm2$year)
# All these plots are ok, so the slight quantile deviations may perhaps be due to: i) spatial autocorrelation
# (and insufficient RE), ii) missing variables, or iii) the misspecification of the zero-response?



### *** 2.2.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
aods3::gof(pmBSy_zicomglmm1) # Does not work for this type of model!
AER::dispersiontest(object = pmBSy_zicomglmm1, alternative = c("less")) # Does not work for this model type!
DHARMa::testDispersion(simu.resid) # Ok.

## Theoretical count distribution:
theo_count <- COMPoissonReg::rzicmp(n = nrow(pm2), lambda = mean(pm2$brood_size),
                                    nu = 1.1,  # The 'nu' parameter should be chosen by trial-and-errors.
                                    p = 0.05) # And so does the probability of 0.
tc_df <- data.frame(theo_count)

ggplot2::ggplot(pm, ggplot2::aes(brood_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that brood_size could be following a COM-Poisson distribution of parameter nu~1.1,
# especially if we consider that the zero-inflation (ZI) is generated by another process.

## Distribution of the predicted counts:
pred_counts <- stats::predict(object = pmBSy_zicomglmm2, type = "response") # Extract the predicted counts.
par(mfrow= c(1,2))
hist(pred_counts)
hist(pm2$brood_size) # Compared to the predictions from "pmBSy_zicomglmm1", the ones from "pmBSy_zicomglmm2"
# seem more accurate but predictions are still too narrow and the model still fails to predict small brood
# sizes!

## Zero-inflation (ZI):
simu.resid_woZI <- DHARMa::simulateResiduals(fittedModel = pmBSy_glmm1, n = 1000) # Model without ZI.
DHARMa::testZeroInflation(simu.resid) # Nope.
DHARMa::testZeroInflation(simu.resid_woZI) # Yes, so there truly is a ZI. Yet, this model accounts for it but
# cannot predict it.
# I made a few attempts at improving the ZI part of the model, but I mostly failed (cf. 'pmBSy_zicomglmm2').



### *** 2.2.2.3. Linearity ----
## Plotting the response on the log scale against predictors:
pm2 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (sqrt)" = sqrt(pmF_d60_beta0),
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (sqrt)" = sqrt(woodyveg_vw),
                "woodyveg_vw (log)" = log10(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind log(Y) and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(log_y = log(pm2$brood_size+1)) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity seems respected.



### *** 2.2.2.4. Model goodness-of-fit (GOF) and performances ----
# GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(pmBSy_zicomglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(pmBSy_zicomglmm1)) # p = 0.42, indicating that there is no
# significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely complicated
# topic and interpretations are not straightforward.

# Computing a pseudo-R2:
performance::r2_nakagawa(pmBSy_zicomglmm1) # [Additive model]: Marg_R2_glmm = 0.03; Cond_R2_glmm = 0.05.
performance::r2(pmBSy_zicomglmm2) # Yields NA.

## Likelihood-ration tests (LRT) of GOF:
# For the random-effects (RE):
pmBSy_zicomglm1 <- glmmTMB::glmmTMB(brood_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                      manag_low + manag_high + light_pollution + noise_m +
                                      cumdd_30 + father_cond + mother_cond + year,
                                    data = pm2, family = glmmTMB::compois(link = "log"),
                                    dispformula = ~1,
                                    ziformula = ~1)
summary(pmBSy_zicomglm1) # The non-mixed model gives AIC = 1072.9 while the mixed-model with "id_nestbox"
# as RE gives AIC = 963.2. The one with both RE gives something similar, so it seems like the use of only
# one RE is warranted by the data. The same is true for the "exploratory improved model" (not shown here).

## For the whole model:
pmBSy_comglm0 <- glmmTMB::glmmTMB(brood_size ~ 1,
                                  data = pm2, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~ cumdd_30, # Specifies a more complex dispersion model.
                                  ziformula = ~ 1)
res.LRT_null <- stats::anova(object = pmBSy_comglm0, pmBSy_zicomglm1, test = "LRT")
# The test is significant, confirming that the model is useful to explain the data.




### *** 2.2.2.5. Posterior predictive simulations ----
# Predicted counts:
par(.pardefault)
obsprop <- prop.table(table(pm2$brood_size))
sims <- stats::simulate(pmBSy_zicomglmm1, nsim = 1000)

nsim0 <- colSums(sims == 0) # Number of zeros (min obs value)
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim0)),
     ylab="Probability", xlab="Number of zeros")
(obs0 <- sum(pm2$brood_size == 0))
points(obs0, 0.06, col="red", pch=16, cex=2) # See the y (0.06) values in 'obsprop'!

nsim8 <- colSums(sims == 8) # Number of eights (modal obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim8)),
     ylab="Probability", xlab="Number of eights")
(obs8 <- sum(pm2$brood_size == 8))
points(obs8, 0.24, col="red", pch=16, cex=2)

nsim12 <- colSums(sims == 12) # Number of twelves (max obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim12)),
     ylab="Probability", xlab="Number of twelves")
(obs12 <- sum(pm2$brood_size == 12))
points(obs12, 0.004, col="red", pch=16, cex=2)
# These three examples indicate that the model(s) is not that bad. The range of zero predictions are in line
# with observations, yet the mode could be better predicted and so does the right-tail of the distribution.



### ** 2.2.3. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ -----

### *** 2.2.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## Parametric bootstrap to test the additive effect of the connectivity metric:
pmBSy_zicomglmm0 <- stats::update(pmBSy_zicomglmm1, .~. -logged_Fmetric)

tictoc::tic("Parametric bootstrap LRT for the additive effect")
res.LRT_inteff <- DHARMa::simulateLRT(m0 = pmBSy_zicomglmm0, m1 = pmBSy_zicomglmm1, n = 500, seed = 107)
tt <- as.data.frame(cbind(res.LRT_inteff$method,
                          res.LRT_inteff$data.name,
                          res.LRT_inteff$statistic,
                          res.LRT_inteff$p.value))
rownames(tt) <- NULL
tt %>% dplyr::rename("Method" = V1,
                     "Models" = V2,
                     "Log Likelihood (M1/M0)" = V3,
                     "p-value" = V4) -> tt
readr::write_csv2(x = tt, file = here::here("output", "tables", "res.pmBSy_LRT_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~??? to run!
# The PB-based LRT is non-significant, indicating that our connectivity metric does not improve the
# description of the data here.
# NOTE: initially, normally I would use the more efficient 'pbkrtest::PBmodcomp()' instead of the
# 'DHARMa::simulateLRT()' function, but it doesn't work with {glmmTMB} objects.


## Parametric bootstrap to test the interactive effect of the connectivity metric:
# To save some time, I don"t even bother computing the PB-based LRT for the interaction effect, it won't be
# significant. I should improve my proxies and my models first.



### *** 2.2.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for the additive COM-Poisson GLMM parameters")
res.pmBSy_addeff_CI_boot <- confint(pmBSy_zicomglmm1, method="boot")
tt <- as.data.frame(res.pmBSy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmBSy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~2h10 to run!



### *** 2.2.3.3. Conclusion ----

summary(pmBSy_zicomglmm1)
summary(pmBSy_zicomglmm2)
# Our diagnostics show that the use of a ZICOM-Poisson regression strongly improves models predictive
# accuracy, and our models seem to fit the data fairly well. However, there is still room for improvements
# as the models still tend to predict a higher count-range than the observed one and the mode is not
# satisfyingly predicted. Interestingly, for this response variable, the use of RE does not seem useful.
# Possible leads for improvement could be to remove the zeros (as it may stem from a distinct process), merge
# observations, and try to improve the modelling of the dispersion (nu) parameter, although previous attempts
# were not very conclusive: still, the "exploratory improved model" showed that "father_cond" and "cumdd_30"
# could be useful to better model the ZI and the dispersion (cf. 'pmBSy_zicomglmm2').
# As they are, unfortunately, the models do not support our hypotheses and only four predictors turned out
# significant: "cumdd_30", "manag_high", "year2022" and either "mother_cond" or "woodyveg", depending on the
# model specification; while the lowest value of AIC = 1065.9. Here, our current "F-metric" is useless!





########################## ************************************************* ###############################
# ------------------------------------- #
##### 3. Modelling fledging success #####
# ------------------------------------- #

##### * 3.1. Fledging success: Binomial GLMM ------------------------------------
# ---------------------------------------------------------------------------- #
### ** 3.1.1. Initial model fit ----
# __________________________________

## Fitting a binomial GLM:
pmFSy_glm1 <- stats::glm(fledgling_nb/clutch_size ~ logged_woodyveg + logged_Fmetric +
                           urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                           cumdd_30 + father_cond + mother_cond + year,
                         weights = clutch_size, # Prior weights!
                         data = pm2, family = "binomial") # Weights should not
# be forgotten. Otherwise, the formulation should be: cbind(fledgling_nb, clutch_size-fledgling_nb)!

## Fitting an additive GLMM:
pmFSy_glmm1 <- lme4::glmer(fledgling_nb/clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                             manag_low + manag_high + light_pollution + noise_m +
                             cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                           weights = clutch_size, data = pm2, family = "binomial")
# As there are convergence issues, I change the optimizer and increase iterations:
ss <- lme4::getME(pmFSy_glmm1, c("theta", "fixef"))
pmFSy_glmm2 <- stats::update(pmFSy_glmm1, start=ss,
                             control=lme4::glmerControl(optimizer="bobyqa",
                                                        optCtrl=list(maxfun=2e5))) # Convergence ok.

# # *** FURTHER TESTS *** #
# # So I'll try using the Gauss-Hermite quadrature (GHQ) for estimation:
# pmFSy_glmm2_bGHQ <- stats::update(pmFSy_glmm2, nAGQ = 10)
# summary(pmFSy_glmm2_bGHQ) # Same!
# # Remember that GHQ compute things differently!
# # Try all optimizers:
# pmFSy_glmm2_all <- lme4::allFit(pmFSy_glmm2)
# summary(pmFSy_glmm2_all) # All optimizers converge but give rather similar results, even though Nelder-
# # Mead appears to disagree.

# ## Fitting interactive (mediated) GLMMs:
# pmFSy_glmm3 <- lme4::glmer(fledgling_nb/clutch_size ~
#                              scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
#                              urban_intensity + manag_low + manag_high + light_pollution + noise_m +
#                              cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
#                            weights = clutch_size, data = pm2, family = "binomial",
#                            control=lme4::glmerControl(optimizer="bobyqa",
#                                                       optCtrl=list(maxfun=2e5))) # Doesn't seem supported
# by the data.
summary(pmFSy_glm1) # AIC = 1569.9.
summary(pmFSy_glmm2) # AIC = 1248.4.

# # Test by removing possible overly influential observations:
# pm2_wo <- pm2[-c(49,58,50,143),]
# pmFSy_glmm3_wo <- stats::update(pmFSy_glmm3, data=pm2_wo)
# summary(pmFSy_glmm3)$AIC
# summary(pmFSy_glmm3_wo)$AIC # Strongly improved AIC, BIC, and deviance. However, it yields lower R2_glmm!





### ** 3.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 3.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
plot(pmFSy_glmm2, id = 0.05, idLabels = ~.obs) # Strange pattern. Many extreme residuals.
pm2[c(45,49,234),] # Nestboxes with the highest residuals = ~100% fledging success!
pm2[c(50,58,143),] # Nestboxes with the lowest residuals = ~0% fledging success!
# Interestingly, they may belong to the same nestbox, suggesting a strong year effect.
performance::check_outliers(pmFSy_glmm2) # Detected 25 potential outliers.

# To further investigate patterns, I can plot the residuals against some predictors:
resid <- stats::resid(pmFSy_glmm2, type = 'deviance')
plot(x = pm2$noise_m, y = resid) # Only "light_pollution" and "noise_m" show slightly strange patterns.
# Possibly because "noise_m" should be transformed (standardisation?). Otherwise, most predictors only
# show a higher variability at medium values.
plot(pmFSy_glmm2, id_nestbox~stats::resid(.)) # Justification for the nestbox random effect (RE).
plot(pmFSy_glmm2, site~stats::resid(.)) # Interestingly, there are not that much among-sites variance.

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmFSy_glmm2, n = 1000, seed = 37)
plot(simu.resid) # Significant deviations detected! There seems to be a clear zero-inflation!

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm2$coord_x, y = pm2$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(pmFSy_glmm2) # Significant autocorrelation, but the Durbin-Watson test
# is known to be very sensitive.
performance::check_collinearity(pmFSy_glmm2) # Ok-ish, but "urban_intensity" > 3!
stats::vcov(pmFSy_glmm2) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_woodyveg)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_Fmetric)
DHARMa::plotResiduals(simu.resid, form = pm2$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$light_pollution)
DHARMa::plotResiduals(simu.resid, form = pm2$noise_m)
DHARMa::plotResiduals(simu.resid, form = pm2$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = pm2$father_cond) # Signs of heteroscedasticity?
DHARMa::plotResiduals(simu.resid, form = pm2$mother_cond)
DHARMa::plotResiduals(simu.resid, form = pm2$year) # Signs of heteroscedasticity?
# There are rather clear signs of heteroscedasticity that are possibly linked to the zero-inflation.



### *** 3.1.2.2. Distribution and dispersion ----
## Assessing over or under-dispersion:
aods3::gof(pmFSy_glmm2) # Note that this test is meant for count data! Overdispersion?
DHARMa::testDispersion(simu.resid, alternative = "greater") # Significant overdispersion detected!
performance::check_overdispersion(x = pmFSy_glmm2) # Same here!

## Distribution of the predicted probabilities:
probabilities <- stats::predict(object = pmFSy_glmm2, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities)
hist(pm2$fledgling_nb/pm2$clutch_size) # The model does not fit the data well! It over-predicts average
# probabilities and strongly under-predicts the number of failures!

## Zero-inflation:
DHARMa::testZeroInflation(simu.resid) # Significant inflation detected!
# Fitting a zero-inflated (ZI) model and compare it:
pmFSy_ziglmm1 <- glmmTMB::glmmTMB(fledgling_nb/clutch_size ~
                                    logged_woodyveg + logged_Fmetric +
                                    urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                  weights = clutch_size,
                                  data = pm2, family = binomial,
                                  ziformula = ~ 1) # Specifies a null ZI model.
summary(pmFSy_glmm2) # AIC = 1248.4.
summary(pmFSy_ziglmm1) # AIC = 970.4!
simu.resid_zi <- DHARMa::simulateResiduals(fittedModel = pmFSy_ziglmm1, n = 1000)
plot(simu.resid_zi) # The use of the ZI-binomial model seem to have solved the problems. To verify, I will
# look at the predictions from this new model:
probabilities <- stats::predict(object = pmFSy_ziglmm1, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities)
hist(pm2$fledgling_nb/pm2$clutch_size) # Surprisingly, the model's predictions are perhaps even worse: it
# seems unable to predict extreme events, whether total successes or failures.



### *** 3.1.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
pm2 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (sqrt)" = sqrt(pmF_d60_beta0),
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (sqrt)" = sqrt(woodyveg_vw),
                "woodyveg_vw (log)" = log10(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(logit = log(probabilities/(1-probabilities))) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -logit)
# Create scatterplot:
ggplot2::ggplot(mydata, ggplot2::aes(y = logit, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Ok.



### *** 3.1.2.4. Goodness-of-fit (GOF) and performances ----
## GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(pmFSy_ziglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(pmFSy_ziglmm1)) # p = 0, likely a mistake?

## Computing a pseudo-R2:
performance::r2_nakagawa(pmFSy_glmm2) # [Additive model]: Marg_R2_glmm = 0.11; Cond_R2_glmm = 0.48.
performance::r2_nakagawa(pmFSy_ziglmm1) # [Additive model]: Marg_R2_glmm = 0.10; Cond_R2_glmm = 0.2 (but
# strange warning, so probably a mistake).

## Likelihood-ration tests (LRT) of GOF:
# Importance of the "id_nestbox" random-effect (RE):
pmFSy_ziglmm0 <- glmmTMB::glmmTMB(fledgling_nb/clutch_size ~
                                    logged_woodyveg + logged_Fmetric +
                                    urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year,
                                  weights = clutch_size,
                                  data = pm2, family = binomial,
                                  ziformula = ~1)
summary(pmFSy_ziglmm0) # The non-mixed model gives AIC = 990.9 while the mixed-model gave AIC = 970.4, so the
# the use of the mixed model seems warranted by the data!

# # Importance of the fixed effects:
# pmFSy_ziglmm0 <- lme4::glmer(fledgling_nb/clutch_size ~ 1 + (1|id_nestbox),
#                              weights = clutch_size, data = pm2, family = "binomial",
#                              control=lme4::glmerControl(optimizer="bobyqa",
#                                                         optCtrl=list(maxfun=2e5)))
# tictoc::tic("Parametric bootstrap LRT")
# res.LRT_re <- DHARMa::simulateLRT(m0 = pmFSy_ziglmm0, m1 = pmFSy_ziglmm1, n = 500, seed = 85)
# tictoc::toc() # DISCLAIMER: took ~1h45 to run!
# # The LRT is highly significant, suggesting that M1 better describes the data than M0!





### ** 3.1.3. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ -----

### *** 3.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
pmFSy_ziglmm0 <- stats::update(pmFSy_ziglmm1, .~. -logged_Fmetric)

res.LRT_addeff <- pbkrtest::PBmodcomp(pmFSy_ziglmm1, pmFSy_ziglmm0, nsim = 500, seed = 56) # Took ~??? to run!
readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
                                                             "res.pmFSy_LRT_addeff.csv"))
# The LRT is not significant, indicating that our connectivity metric does not improve the description of
# the data here.

## For the interaction effect:
# Since even the additive model is NOT SIGNIFICANT, there is no point in testing the effect of the
# interactive (mediated) model.



### *** 3.1.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive GLMM parameters")
res.pmFSy_addeff_CI_boot <- confint(pmFSy_ziglmm1, method="boot")
tt <- as.data.frame(res.pmFSy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmFSy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~1h45 to run!



### *** 3.1.3.3. Conclusion ----

summary(pmFSy_ziglmm1) # AIC = 970.4!
# Our models does not fit the data adequately. Moreover, our hypotheses were not validated and only three
# variables turned out significant: "mother_cond", "year2020" and "year2022" (but "woodyveg" could be too).
# The best model so far gave an AIC = 970.4.
# ALSO I should redo full-diags for the ZI model to find improvements??? AND/OR model fledg/brood!!!???





##### *-----------------------------------------------------------------* ######
##### * 3.2. Fledgling number: ZICOM-Poisson GLMM ------------------------------
# ---------------------------------------------------------------------------- #
### ** 3.2.1. Initial model fit ----
# __________________________________

# Given the look of the response variable and my previous diagnostics, I chose to directly compare various
# structural model types before even starting the dedicated diagnostics.

## Fitting a regular Poisson GLM:
pmFNy_glm1 <- stats::glm(fledgling_nb ~ logged_woodyveg + logged_Fmetric +
                           urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                           cumdd_30 + father_cond + mother_cond + year,
                         data = pm2, family = "poisson")

## Fitting a regular Poisson GLMM:
pmFNy_glmm1 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                  manag_low + manag_high + light_pollution + noise_m +
                                  cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                data = pm2, family = "poisson")

## Fitting a Zero-Inflated (ZI) Poisson GLMM:
pmFNy_ziglmm1 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                    manag_low + manag_high + light_pollution + noise_m +
                                    cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                  data = pm2, family = "poisson",
                                  ziformula = ~1)

## Fitting a Zero-Inflated (ZI) COM-Poisson GLMM:
pmFNy_zicomglmm1 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                       manag_low + manag_high + light_pollution + noise_m +
                                       cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                     data = pm2, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~1,
                                     ziformula = ~1) # Rather long to fit.

summary(pmFNy_glm1) # AIC = 1430.8.
summary(pmFNy_glmm1) # AIC = 1408.4.
summary(pmFNy_ziglmm1) # AIC = 1182.4.
summary(pmFNy_zicomglmm1) # AIC = 1151.9.
# It seems that all sequential additional specifications improved the fit, albeit the inclusion of the
# COM-Poisson distribution only produced a mild effect. I will still use that model for diagnostics and see
# how it behaves.
pmFNy_zicomglmm2 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                       manag_low + manag_high + light_pollution + noise_m +
                                       cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                                     data = pm2, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~logged_Fmetric,
                                     ziformula = ~manag_low) # Rather long to fit.
summary(pmFNy_zicomglmm2) # AIC = 1144.8 vs 1151.9.
# NOTE: After a few attempts to tune the model, I found that this specification slightly improved the model.
# But as it is a kind of data-dredging, I prefer carry on with my initially intended model. Still, I will
# perform diagnostics on both models. This 2nd model will be sometimes called "exploratory improved model".

# # The interactive (mediated) model:
# pmFNy_zicomglmm3 <- glmmTMB::glmmTMB(fledgling_nb ~
#                                        scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
#                                        urban_intensity + manag_low + manag_high + light_pollution + noise_m +
#                                        cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
#                                      data = pm2, family = glmmTMB::compois(link = "log"),
#                                      dispformula = ~1,
#                                      ziformula = ~1) # Rather long to fit.
# summary(pmFNy_zicomglmm3) # AIC = 1152, doesn't seem supported by the data.





### ** 3.2.2. Diagnostics and assumption checks ----
# __________________________________________________

# IMPORTANT NOTE: all diagnostics have been performed on both ZICOM_GLMMs (1 and 2)!

### *** 3.2.2.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(pmFNy_zicomglmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Ok-ish but there are a few potential outliers.
# performance::check_outliers(pmFNy_zicomglmm1) # Does not work for this type of model.
pm2[which(resid < -4),] # As could have been guessed from the plot, the 2nd group (with the lowest residuals)
# consist of all the observations where "fledgling_nb" ~ 0. It could be a sign that the zero-part of the model
# should indeed be modelled with more relevant predictors. But even the "exploratory improved model" does not
# erase this pattern.

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = pm2$logged_Fmetric, y = resid) # There may be signs of heteroscedasticity for the "F-metric".
# Otherwise, it seems ok (but, once again, simulated residuals will be more useful).
# plot(pmFNy_zicomglmm1, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(pmFNy_zicomglmm1, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmFNy_zicomglmm1, n = 1000, re.form = NULL)
simu.resid2 <- DHARMa::simulateResiduals(fittedModel = pmFNy_zicomglmm2, n = 1000, re.form = NULL)
# The 're.form' argument is to base simulations on the model unconditional of the random effects (and only
# works for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted
# eventually.
plot(simu.resid) # Ok_ish (but very slight deviation?).
DHARMa::outliers(simu.resid) # None.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm2$coord_x, y = pm2$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(pmFNy_zicomglmm1) # Ok.
performance::check_collinearity(pmFNy_zicomglmm1) # Ok-ish, but "urban_intensity" > 4!
stats::vcov(pmFNy_zicomglmm1) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_woodyveg)
DHARMa::plotResiduals(simu.resid, form = pm2$logged_Fmetric)
DHARMa::plotResiduals(simu.resid, form = pm2$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = pm2$light_pollution)
DHARMa::plotResiduals(simu.resid, form = pm2$noise_m) # Slight quantile deviation (also true for "model2")!
DHARMa::plotResiduals(simu.resid, form = pm2$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = pm2$father_cond) # Slight quantile deviation!
DHARMa::plotResiduals(simu.resid, form = pm2$mother_cond)
DHARMa::plotResiduals(simu.resid, form = pm2$year)



### *** 3.2.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
aods3::gof(pmFNy_zicomglmm1) # Does not work for this type of model!
AER::dispersiontest(object = pmFNy_zicomglmm1, alternative = c("less")) # Does not work for this model type!
DHARMa::testDispersion(simu.resid) # Ok.

## Theoretical count distribution:
theo_count <- COMPoissonReg::rzicmp(n = nrow(pm2), lambda = mean(pm2$fledgling_nb),
                                    nu = 0.95,  # The 'nu' parameter should be chosen by trial-and-errors.
                                    p = 0.15) # And so does the probability of 0.
tc_df <- data.frame(theo_count)

ggplot2::ggplot(pm, ggplot2::aes(fledgling_nb)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that fledgling_nb could be following a COM-Poisson distribution of parameter nu~0.95,
# although that means that the dispersion is approximately equal to that of a regular Poisson.

## Distribution of the predicted counts:
pred_counts <- stats::predict(object = pmFNy_zicomglmm1, type = "response") # Extract the predicted counts.
par(mfrow= c(1,2))
hist(pred_counts)
hist(pm2$fledgling_nb) # The "pmFNy_zicomglmm2" model yields better predictions than the simpler model but
# neither fit is truly satisfactory. Both models overestimate lower counts but fail to properly model the ZI!

## Zero-inflation (ZI):
simu.resid_woZI <- DHARMa::simulateResiduals(fittedModel = pmFNy_glmm1, n = 1000) # Model without ZI.
DHARMa::testZeroInflation(simu.resid) # Nope.
DHARMa::testZeroInflation(simu.resid_woZI) # Yes, so there truly is a ZI. Yet, this model accounts for it but
# cannot predict it.



### *** 3.2.2.3. Linearity ----
## Plotting the response on the log scale against predictors:
pm2 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (sqrt)" = sqrt(pmF_d60_beta0),
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (sqrt)" = sqrt(woodyveg_vw),
                "woodyveg_vw (log)" = log10(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind log(Y) and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(log_y = log(pm2$fledgling_nb+1)) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # The linearity assumptions seems violated by several
# predictors: both "parental condition" variables and "noise_m"! But this violation does not seem responsible
# for the lack of fit.



### *** 3.2.2.4. Model goodness-of-fit (GOF) and performances ----
# GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(pmFNy_zicomglmm2, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(pmFNy_zicomglmm2)) # p ~ 0.32 for both models, indicating that
# there is no significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely
# complicated topic and interpretations are not straightforward.

# Computing a pseudo-R2:
performance::r2_nakagawa(pmFNy_zicomglmm1) # [Additive model]: Marg_R2_glmm = 0.04; Cond_R2_glmm = 0.04.
performance::r2(pmFNy_zicomglmm2) # Yields NA.

## Likelihood-ration tests (LRT) of GOF:
# For the random-effects (RE):
pmFNy_zicomglm2 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                                      manag_low + manag_high + light_pollution + noise_m +
                                      cumdd_30 + father_cond + mother_cond + year,
                                    data = pm2, family = glmmTMB::compois(link = "log"),
                                    dispformula = ~logged_Fmetric,
                                    ziformula = ~manag_low) # Rather long to fit.
summary(pmFNy_zicomglm2) # AIC = 1143 vs 1144.8, so the inclusion of the RE does not seem warranted by the
# data. Still, we will keep using it as it is the originally intended model.

## For the whole model:
pmFNy_zicomglm0 <- glmmTMB::glmmTMB(fledgling_nb ~ 1,
                                    data = pm2, family = glmmTMB::compois(link = "log"),
                                    dispformula = ~logged_Fmetric,
                                    ziformula = ~manag_low)
res.LRT_null <- stats::anova(object = pmFNy_zicomglm0, pmFNy_zicomglm2, test = "LRT")
# The test is significant, confirming that the model is useful to explain the data.



### *** 3.2.2.5. Posterior predictive simulations ----
# Predicted counts:
par(.pardefault)
obsprop <- prop.table(table(pm2$fledgling_nb))
sims <- stats::simulate(pmFNy_zicomglmm2, nsim = 1000)

nsim0 <- colSums(sims == 0) # Number of zeros (min obs value)
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim0)),
     ylab="Probability", xlab="Number of zeros")
(obs0 <- sum(pm2$fledgling_nb == 0))
points(obs0, 0.16, col="red", pch=16, cex=2) # See the y (0.06) values in 'obsprop'!

nsim5 <- colSums(sims == 5) # Number of fives (second modal obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim5)),
     ylab="Probability", xlab="Number of fives")
(obs5 <- sum(pm2$fledgling_nb == 5))
points(obs5, 0.11, col="red", pch=16, cex=2)

nsim11 <- colSums(sims == 11) # Number of elevens (max obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim11)),
     ylab="Probability", xlab="Number of elevens")
(obs11 <- sum(pm2$fledgling_nb == 11))
points(obs11, 0.004, col="red", pch=16, cex=2)
# These three examples confirm that the model still predicts values that are too wide and overestimated
# compared to the true observed values. Small successes are not correctly predicted!



### ** 3.2.3. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ -----

### *** 3.2.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## Parametric bootstrap to test the additive effect of the connectivity metric:
pmFNy_zicomglmm0 <- stats::update(pmFNy_zicomglmm1, .~. -logged_Fmetric)

tictoc::tic("Parametric bootstrap LRT for the additive effect")
res.LRT_inteff <- DHARMa::simulateLRT(m0 = pmFNy_zicomglmm0, m1 = pmFNy_zicomglmm1, n = 500, seed = 97)
tt <- as.data.frame(cbind(res.LRT_inteff$method,
                          res.LRT_inteff$data.name,
                          res.LRT_inteff$statistic,
                          res.LRT_inteff$p.value))
rownames(tt) <- NULL
tt %>% dplyr::rename("Method" = V1,
                     "Models" = V2,
                     "Log Likelihood (M1/M0)" = V3,
                     "p-value" = V4) -> tt
readr::write_csv2(x = tt, file = here::here("output", "tables", "res.pmFNy_LRT_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~??? to run!
# The PB-based LRT is XXX, indicating that our connectivity metric (does not?) improve the
# description of the data here.
# NOTE: initially, normally I would use the more efficient 'pbkrtest::PBmodcomp()' instead of the
# 'DHARMa::simulateLRT()' function, but it doesn't work with {glmmTMB} objects.


## Parametric bootstrap to test the interactive effect of the connectivity metric:
# To save some time, I don"t even bother computing the PB-based LRT for the interaction effect, it won't be
# significant. I should improve my proxies and my models first.



### *** 3.2.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for the additive COM-Poisson GLMM parameters")
res.pmFNy_addeff_CI_boot <- confint(pmFNy_zicomglmm1, method="boot")
tt <- as.data.frame(res.pmFNy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmFNy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~??? to run!



### *** 3.2.3.3. Conclusion ----

summary(pmFNy_zicomglmm1)
summary(pmFNy_zicomglmm2) # Exploratory improved model!
# The "pmFNy_zicomglmm2" model yielded better results than the simpler model but neither fit is truly
# satisfying. Both models overestimate lower counts but fail to properly model the ZI (it could be a sign of
# missing predictors)! Moreover, the linearity assumptions seems violated by several predictors: i.e. both
# "parental condition" variables and "noise_m"!
# If the use of a ZICOM-Poisson regression strongly improved models predictive accuracy, it seems clear that
# the ZI is caused by another process. Possible leads for improvement could be to remove the possible outliers,
# merge observations, and try to improve the modelling of the dispersion (nu) and ZI parameters (or remove the
# zeros).
# As they are, unfortunately, the models do not support our hypotheses and only two predictors turned out
# significant in "pmFNy_zicomglmm1": "cumdd_30", and "year2022" (with AIC = 1151.9). However, the exploratory
# improved model suggest that our "F-metric" could be important to model the ZI and that "manag_low" could be
# important to model the dispersion (AIC = 1144.8 with "woodyveg" also significant)!





########################## ************************************************* ###############################
# ----------------------------------------------- #
##### 4. Modelling the morphometric variables #####
# ----------------------------------------------- #

##### * 4.1. Morphometry: LMM ---------------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 4.1.1. Initial model fit ----
# __________________________________

## Creating a synthetic morphometric variable:
pm2 %>% dplyr::filter(is.na(wing_length) == FALSE) -> pm3 # Only 141 observations left.
pm3 %>% dplyr::select(id_nestbox, site, mass, tarsus_length, wing_length) -> xxx

# Normed-PCA:
res.pca <- FactoMineR::PCA(X = xxx[, 3:ncol(xxx)], scale.unit = TRUE, graph = FALSE)
# To plot results:
morpho_pm.varplot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                              repel = TRUE)
morpho_pm.indplot <- plot(res.pca, choix = "ind", autoLab = "yes")
#gridExtra::grid.arrange(morpho_pm.varplot, morpho_pm.indplot, ncol = 2)

# As the first axis (PC) of my PCA satisfactorily synthesizes a large amount of the variance (84.5%)
# of my three variables, we can use the coordinates of observations on this axis as a synthetic variable:
zzz <- res.pca$ind$coord[,1]
pm3$morphometry <- zzz # This variable opposes nestboxes that host "big" (potentially well-fed) nestlings.


## Fitting a regular linear model:
pmMMy_lm1 <- stats::lm(morphometry ~ logged_woodyveg + logged_Fmetric +
                         urban_intensity + cumdd_30 + father_cond + mother_cond + year, data = pm3)
# pmMMy_lm2 <- stats::lm(morphometry ~ scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
#                            urban_intensity + year, data = pm3) # Interaction not significant!


## Fitting an additive LMM:
pmMMy_lmm1 <- lme4::lmer(morphometry ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                           cumdd_30 + year + (1|id_nestbox), data = pm3)
# Gives a singular fit (RE variance = 0). I'll thus try setting a weak prior on the variance:
pmMMy_blmm1 <- blme::blmer(morphometry ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                             cumdd_30 + year + (1|id_nestbox), data = pm3)
# As there are convergence issues, I change the optimizer and increase iterations:
pmMMy_blmm2 <- stats::update(pmMMy_blmm1, control=lme4::lmerControl(optimizer="bobyqa",
                                                                    optCtrl=list(maxfun=2e5)))
# # Try all optimizers:
# pmMMy_blmm2_all <- lme4::allFit(pmMMy_blmm2)
# summary(pmMMy_blmm2_all) # Two optimizers failed to converge but give rather similar results, except the
# # "nloptwrap.NLOPT_LN_BOBYQA" optimizer that computes a larger RE variance and thus, lower coefficient
# # estimates. We will thus stick with "bobyqa".

## Fitting interactive (mediated) LMMs:
pmMMy_blmm3 <- blme::blmer(morphometry ~
                             scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
                             urban_intensity + cumdd_30 + year + (1|id_nestbox), data = pm3,
                           control=lme4::lmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=2e5)))
# # Test by removing possible overly influential observations:
# pm2_wo <- pm2[-c(49,58,50,143),]
# pmFSy_glmm3_wo <- stats::update(pmFSy_glmm3, data=pm2_wo)
# summary(pmFSy_glmm3)$AIC
# summary(pmFSy_glmm3_wo)$AIC # Strongly improved AIC, BIC, and deviance. However, it yields lower R2_glmm!





### ** 4.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 4.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Extracting residuals (with the {redres}):
raw_cond <- redres::compute_redres(pmMMy_blmm3) # Computes the raw conditional residuals (conditional on
# the random effects (RE)).
pearson_mar <- redres::compute_redres(pmMMy_blmm3, type = "pearson_mar") # Computes the Pearson marginal
# (not accounting for the RE) residuals.
std_cond <- redres::compute_redres(pmMMy_blmm3, type = "std_cond") # Computes the studentised cond. ones.
# Joins the residuals to the paprika data:
xxx <- cbind(pm3, raw_cond, pearson_mar, std_cond)

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmMMy_blmm3, n = 1000, plot = FALSE)
par(.pardefault)
plot(simu.resid) # Ok.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm3$coord_x, y = pm3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(pmMMy_blmm3) # Ok.
performance::check_collinearity(pmMMy_blmm3) # Ok.
stats::vcov(pmMMy_blmm3) # Ok, but "cumdd_30" has a slightly high covariance with the intercept.



### *** 4.1.2.2. Distribution and homoscedasticity ----
## Assessing the normality of the residuals:
stats::shapiro.test(xxx$raw_cond) # Significant deviation from normality detected, but the Shapiro test is
# known to be extremely sensitive. So plotting would be better:
xxx %>%
  tidyr::gather(key = "type", value = "residual", 30:32) %>%
  ggplot2::ggplot(ggplot2::aes(x = residual)) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::facet_grid(. ~ type, scales = "free") +
  ggplot2::theme_bw() # The residuals are indeed slightly left-skewed and possibly lack kurtosis but that
# could be acceptable.
redres::plot_resqq(pmMMy_blmm3) # As expected, the plot shows a substantial departure from Normality at the
# extreme ends of the quantiles, that is at the border of the parameters space. Overall, as almost all
# points stay within the 95% CI, we can say it is ok-ish.

## Assessing the normality if the random effect:
redres::plot_ranef(pmMMy_blmm3) # Same thing here.

## Assessing homogeneity of variance and influential observations:
plot(pmMMy_blmm3, type=c("p","smooth"), col.line = 2, id = 0.05, idLabels = ~.obs) # A slight curvature
# seem to exist and there are 5 possible outliers. Otherwise, there is no clear heteroscedasticity.
pm3[c(10,86,46,97,61),] # RAS.

# Residuals vs leverage:
plot(pmMMy_blmm3, stats::rstudent(.) ~ stats::hatvalues(.))
cd <- stats::cooks.distance(pmMMy_blmm3)
plot(cd)
pm3[which(cd>0.4),] # Ok, all observations are < 0.5, so no overly influential points.
pm3[which(cd>0.15),] # If we pick very conservative values, we find the same obs as before.

## Residuals vs predictors:
redres::plot_redres(pmMMy_blmm3, xvar = "scale(logged_Fmetric, scale = F)") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Residual vs F-metric (log scale)") # Plotting residuals against the predictors
# does not give much odd results, but emphasized the incomplete sampling of the predictor
# space for some variables such as the F-metric.
# plot(pm3$logged_Fmetric, stats::residuals(pmMMy_blmm3)) # Same plot (I should create a custom function).
redres::plot_redres(pmMMy_blmm3, type = "raw_mar", xvar = "year")

## Distribution of the predicted values:
par(.pardefault)
predictions <- stats::predict(object = pmMMy_blmm3, type = "response") # Extract the predicted values.
par(mfrow= c(1,2))
hist(predictions)
plot(ecdf(predictions))
fitdistrplus::plotdist(data = pm3$morphometry, histo = TRUE, demp = TRUE) # Ok-ish...



### *** 4.1.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
pm3 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (log)" = log10(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind 'morphometry' and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(morphometry = pm3$morphometry) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -morphometry)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = morphometry, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # We can see that the slight curvature seems to come
# from "urban_intensity" and it seems that log-transforming "woodyveg_vw" might not be such a good idea here
# as it enables an abnormal distortion effect of its lowest value (try without?). Otherwise ok!



### *** 4.1.2.4. Goodness-of-fit (GOF) and performances ----
## Computing a pseudo-R2:
performance::r2_nakagawa(pmMMy_blmm2) # [Additive model]: Marg_R2_lmm = 0.31; Cond_R2_lmm = 0.38.
performance::r2_nakagawa(pmMMy_blmm3) # [Interact. model]: Marg_R2_lmm = 0.31; Cond_R2_lmm = 0.38.

## Likelihood-ration tests (LRT) of GOF:
# Importance of the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- DHARMa::simulateLRT(m0 = pmMMy_lm1, m1 = pmMMy_blmm2, n = 500, seed = 24)
tictoc::toc() # Took ~23s to run.
# The LRT is significant, suggesting that M1 better describes the data than M0, supporting the importance of
# the random effect!

# Importance of the fixed effects (only using the LM):
pmMMy_lm0 <- stats::lm(morphometry ~ 1, data = pm3)
res.LRT_null <- stats::anova(object = pmMMy_lm0, pmMMy_lm1, test = "LRT")
# The test is highly significant, confirming that the model is useful to explain the data.





### ** 4.1.3. Inference and predictions ----
# __________________________________________

### *** 4.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
pmMMy_blmm1 <- stats::update(pmMMy_blmm2, .~. -logged_Fmetric)

res.LRT_addeff <- pbkrtest::PBmodcomp(pmMMy_blmm2, pmMMy_blmm1, nsim = 1000, seed = 399) # Took ~85s to run!
readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
                                                             "res.pmMMy_LRT_addeff.csv"))
# The LRT is significant, indicating that our connectivity metric does improve the description of the data.


## For the interaction effect:
res.LRT_inteff <- pbkrtest::PBmodcomp(pmMMy_blmm3, pmMMy_blmm2, nsim = 1000, seed = 428) # Took ~67s to run!
readr::write_csv2(x = res.LRT_inteff$test, file = here::here("output", "tables",
                                                             "res.pmMMy_LRT_inteff.csv"))
# The LRT is NOT significant, indicating that our hypothesis of an interaction effect is not supported
# by the data.



### *** 4.1.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive LMM parameters")
res.pmMMy_addeff_CI_boot <- confint(pmMMy_blmm2, method="boot")
tt <- as.data.frame(res.pmMMy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmMMy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~17,7s to run!



### *** 4.1.3.3. Conclusion ----

summary(pmMMy_blmm2)
# Our models only moderately fit the observed data. Nonetheless, the importance of the additive effect of
# connectivity is supported by both the parametric bootstrap LRT and the CI on the parameters.
# Diagnostics indicated that removing some influential observations as well as the lowest "woodyveg_vw"
# value (or un-loging it) could perhaps have an impact on the results.
# REMINDER: there are less variables because the sample size is smaller!





######################### *----------------------* #############################
##### * 4.2. Mass: LMM ---------------------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 4.2.1. Initial model fit ----
# __________________________________

## Fitting a regular linear model:
pm2 %>% dplyr::filter(is.na(mass) == FALSE) -> pm3 # Only 213 observations left.

pmMAy_lm1 <- stats::lm(mass ~ logged_woodyveg + logged_Fmetric +
                         urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                         cumdd_30 + father_cond + mother_cond + year, data = pm3)
# pmMAy_lm2 <- stats::lm(mass ~ scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
#                          urban_intensity + manag_low + manag_high + light_pollution + noise_m +
#                          cumdd_30 + father_cond + mother_cond + year, data = pm3)
# # Interaction not significant!


## Fitting an additive LMM:
pmMAy_lmm1 <- lme4::lmer(mass ~ logged_woodyveg + logged_Fmetric +
                           urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                           cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox), data = pm3,
                         control=lme4::lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5))) # Works even without
# increasing the number of iterations or changing the optimizer.
pmMAy_lmm2 <- pmMAy_lmm1 # Only to keep "model2" as my additive model and "model3" as my interactive one.

## Fitting interactive (mediated) LMMs:
pmMAy_lmm3 <- lme4::lmer(mass ~
                           scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
                           urban_intensity + manag_low + manag_high + light_pollution + noise_m +
                           cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox), data = pm3,
                         control=lme4::lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5)))
# # Test by removing possible overly influential observations:
# pm2_wo <- pm2[-c(49,58,50,143),]
# pmFSy_glmm3_wo <- stats::update(pmFSy_glmm3, data=pm2_wo)
# summary(pmFSy_glmm3)$AIC
# summary(pmFSy_glmm3_wo)$AIC # Strongly improved AIC, BIC, and deviance. However, it yields lower R2_glmm!





### ** 4.2.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 4.2.2.1. Residuals extraction, autocorrelation and collinearity ----
## Extracting residuals (with the {redres}):
raw_cond <- redres::compute_redres(pmMAy_lmm3) # Computes the raw conditional residuals (conditional on
# the random effects (RE)).
pearson_mar <- redres::compute_redres(pmMAy_lmm3, type = "pearson_mar") # Computes the Pearson marginal
# (not accounting for the RE) residuals.
std_cond <- redres::compute_redres(pmMAy_lmm3, type = "std_cond") # Computes the studentised cond. ones.
# Joins the residuals to the paprika data:
xxx <- cbind(pm3, raw_cond, pearson_mar, std_cond)

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmMAy_lmm3, n = 1000, plot = FALSE)
par(.pardefault)
plot(simu.resid) # Ok.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm3$coord_x, y = pm3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(pmMAy_lmm3) # Ok.
performance::check_collinearity(pmMAy_lmm3) # Ok-ish but "urban_intensity" has a VIF of 4.2!
stats::vcov(pmMAy_lmm3) # Ok, but "cumdd_30" has a slightly high covariance with the intercept.



### *** 4.2.2.2. Distribution and homoscedasticity ----
## Assessing the normality of the residuals:
stats::shapiro.test(xxx$raw_cond) # Ok. But plotting would be better:
xxx %>%
  tidyr::gather(key = "type", value = "residual", 29:31) %>%
  ggplot2::ggplot(ggplot2::aes(x = residual)) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::facet_grid(. ~ type, scales = "free") +
  ggplot2::theme_bw() # The residuals indeed look ok.
redres::plot_resqq(pmMAy_lmm3) # As expected, the plot shows a substantial departure from Normality at the
# extreme ends of the quantiles, that is at the border of the parameters space. Overall, as almost all
# points stay within the 95% CI, we can say it is ok-ish.

## Assessing the normality if the random effect:
redres::plot_ranef(pmMAy_lmm3) # Same thing here.

## Assessing homogeneity of variance and influential observations:
plot(pmMAy_lmm3, type=c("p","smooth"), col.line = 2, id = 0.05, idLabels = ~.obs) # It's ok but there
# seems to be 8 possible outliers:
pm3[c(22,97),] # High residuals ~= heavy juveniles. RAS.
pm3[c(156, 112, 169, 133, 110, 127),] # Low residuals ~= light juveniles. RAS.

# Residuals vs leverage:
plot(pmMAy_lmm3, stats::rstudent(.) ~ stats::hatvalues(.))
cd <- stats::cooks.distance(pmMAy_lmm3)
plot(cd)
pm3[which(cd>0.4),] # Ok, all observations are < 0.5, so no overly influential points.
pm3[which(cd>0.1),] # Even with very conservative values, we only find obs #127 (DIJ-168_2020).

## Residuals vs predictors:
redres::plot_redres(pmMAy_lmm3, xvar = "mother_cond") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Residual vs predictor") # Ok for most, acceptable for some.
# plot(pm3$logged_Fmetric, stats::residuals(pmMAy_lmm3)) # Same plot (I should create a custom function).
redres::plot_redres(pmMAy_lmm3, type = "raw_mar", xvar = "year") # Ok.

## Distribution of the predicted values:
par(.pardefault)
predictions <- stats::predict(object = pmMAy_lmm3, type = "response") # Extract the predicted values.
par(mfrow= c(1,2))
hist(predictions)
plot(ecdf(predictions))
fitdistrplus::plotdist(data = pm3$mass, histo = TRUE, demp = TRUE) # Rather ok.



### *** 4.2.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
pm3 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, light_pollution, noise_m, cumdd_30,
                      father_cond, mother_cond) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (log)" = log10(woodyveg_vw),
                "woodyveg_vw (sqrt)" = sqrt(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind 'mass' and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(mass = pm3$mass) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -mass)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = mass, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Interestingly, we can see that for several
# predictors, extreme values tend to distort relationships that would perhaps otherwise be significant. It
# also appears here that the log-transformation of "woodyveg" is maybe counter-productive: a spline would
# certainly be better OR, as shown here, a square-root transformation!
# We can also see that "urban_intensity" present a curved relationship with "mass" (and so does "cumdd_30")!




### *** 4.2.2.4. Goodness-of-fit (GOF) and performances ----
## Computing a pseudo-R2:
performance::r2_nakagawa(pmMAy_lmm2) # [Additive model]: Marg_R2_lmm = 0.19; Cond_R2_lmm = 0.23.
performance::r2_nakagawa(pmMAy_lmm3) # [Interact. model]: Marg_R2_lmm = 0.20; Cond_R2_lmm = 0.22.

## Likelihood-ration tests (LRT) of GOF:
# Importance of the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- DHARMa::simulateLRT(m0 = pmMAy_lm1, m1 = pmMAy_lmm2, n = 500, seed = 762)
tictoc::toc() # Took ~11s to run.
# The LRT is significant, suggesting that M1 better describes the data than M0, supporting the importance of
# the random effect!

# Importance of the fixed effects (only using the LM):
pmMAy_lm0 <- stats::lm(mass ~ 1, data = pm3)
res.LRT_null <- stats::anova(object = pmMAy_lm0, pmMAy_lm1, test = "LRT")
# The test is highly significant, confirming that the model is useful to explain the data.





### ** 4.2.3. Inference and predictions ----
# __________________________________________

### *** 4.2.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
pmMAy_lmm1 <- stats::update(pmMAy_lmm2, .~. -logged_Fmetric)

res.LRT_addeff <- pbkrtest::PBmodcomp(pmMAy_lmm2, pmMAy_lmm1, nsim = 1000, seed = 22) # Took ~34s to run!
readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
                                                             "res.pmMAy_LRT_addeff.csv"))
# The LRT is significant, indicating that our connectivity metric does improve the description of the data.


## For the interaction effect:
res.LRT_inteff <- pbkrtest::PBmodcomp(pmMAy_lmm3, pmMAy_lmm2, nsim = 1000, seed = 253) # Took ~31s to run!
readr::write_csv2(x = res.LRT_inteff$test, file = here::here("output", "tables",
                                                             "res.pmMAy_LRT_inteff.csv"))
# The LRT is NOT significant, indicating that our hypothesis of an interaction effect is not supported
# by the data.



### *** 4.2.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive LMM parameters")
res.pmMAy_addeff_CI_boot <- confint(pmMAy_lmm2, method="boot")
tt <- as.data.frame(res.pmMAy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmMAy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~7s to run!



### *** 4.2.3.3. Conclusion ----

summary(pmMAy_lmm2)
# Our models fit the observed data relatively well. The importance of the additive effect of connectivity
# is supported by both the parametric bootstrap LRT and the CI on the parameters. An optimist would even
# say that the PB-based LRT for the interactive effect is close to be meaningful. We also saw, again, that
# there is a clear "year" effect!
# Anyway, diagnostics indicated that there are room for improvements: removing outliers, changing the
# transformation of some variables, accounting for possible curvature effects...





#################### *----------------------------------* ######################
##### * 4.3. Tarsus length: LMM ------------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 4.3.1. Initial model fit ----
# __________________________________

## Fitting a regular linear model:
pm2 %>% dplyr::filter(is.na(tarsus_length) == FALSE) -> pm3 # Only 173 observations left.

pmTLy_lm1 <- stats::lm(tarsus_length ~ logged_woodyveg + logged_Fmetric +
                         urban_intensity + manag_low + manag_high + cumdd_30 + year, data = pm3)
# pmTLy_lm2 <- stats::lm(tarsus_length ~ scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
#                          urban_intensity + manag_low + manag_high + cumdd_30 + year,
#                        data = pm3) # Interaction not significant!


## Fitting an additive LMM:
pmTLy_lmm1 <- lme4::lmer(tarsus_length ~ logged_woodyveg + logged_Fmetric +
                           urban_intensity + manag_low + manag_high + cumdd_30 + year +
                           (1|id_nestbox), data = pm3,
                         control=lme4::lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5))) # Works even without
# increasing the number of iterations or changing the optimizer.
pmTLy_lmm2 <- pmTLy_lmm1 # Only to keep "model2" as my additive model and "model3" as my interactive one.

## Fitting interactive (mediated) LMMs:
pmTLy_lmm3 <- lme4::lmer(tarsus_length ~
                           scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
                           urban_intensity + manag_low + manag_high + cumdd_30 + year +
                           (1|id_nestbox), data = pm3,
                         control=lme4::lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5)))
# # Test by removing possible overly influential observations:
# pm2_wo <- pm2[-c(49,58,50,143),]
# pmFSy_glmm3_wo <- stats::update(pmFSy_glmm3, data=pm2_wo)
# summary(pmFSy_glmm3)$AIC
# summary(pmFSy_glmm3_wo)$AIC # Strongly improved AIC, BIC, and deviance. However, it yields lower R2_glmm!





### ** 4.3.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 4.3.2.1. Residuals extraction, autocorrelation and collinearity ----
## Extracting residuals (with the {redres}):
raw_cond <- redres::compute_redres(pmTLy_lmm3) # Computes the raw conditional residuals (conditional on
# the random effects (RE)).
pearson_mar <- redres::compute_redres(pmTLy_lmm3, type = "pearson_mar") # Computes the Pearson marginal
# (not accounting for the RE) residuals.
std_cond <- redres::compute_redres(pmTLy_lmm3, type = "std_cond") # Computes the studentised cond. ones.
# Joins the residuals to the paprika data:
xxx <- cbind(pm3, raw_cond, pearson_mar, std_cond)

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmTLy_lmm3, n = 1000, plot = FALSE)
par(.pardefault)
plot(simu.resid) # Slight deviation and outliers detected!

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm3$coord_x, y = pm3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(pmTLy_lmm3) # Ok.
performance::check_collinearity(pmTLy_lmm3) # Ok-ish but "urban_intensity" has a VIF of 3.8!
stats::vcov(pmTLy_lmm3) # Ok.



### *** 4.3.2.2. Distribution and homoscedasticity ----
## Assessing the normality of the residuals:
stats::shapiro.test(xxx$raw_cond) # Significant deviation from normality detected, but the Shapiro test is
# known to be extremely sensitive. So plotting would be better:
xxx %>%
  tidyr::gather(key = "type", value = "residual", 29:31) %>%
  ggplot2::ggplot(ggplot2::aes(x = residual)) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::facet_grid(. ~ type, scales = "free") +
  ggplot2::theme_bw() # The residuals are indeed a bit left-skewed, but it may be acceptable.
redres::plot_resqq(pmTLy_lmm3) # As expected, the plot shows a substantial departure from Normality at the
# extreme ends of the quantiles, that is at the border of the parameters space. While it is acceptable for
# the upper quantiles, the departure is quite worrisome for the lower ones.

## Assessing the normality if the random effect:
redres::plot_ranef(pmTLy_lmm3) # Ok-ish.

## Assessing homogeneity of variance and influential observations:
plot(pmTLy_lmm3, type=c("p","smooth"), col.line = 2, id = 0.05, idLabels = ~.obs) # It's ok but there
# seems to be 3 possible outliers:
pm3[c(13,129,82),] # Low residuals ~= short juveniles. RAS.

# Residuals vs leverage:
plot(pmTLy_lmm3, stats::rstudent(.) ~ stats::hatvalues(.))
cd <- stats::cooks.distance(pmTLy_lmm3)
plot(cd)
pm3[which(cd>0.4),] # Ok, all observations are < 0.5, so no overly influential points.
pm3[which(cd>0.1),] # Even with very conservative values, we find the same three observations.

## Residuals vs predictors:
redres::plot_redres(pmTLy_lmm3, xvar = "scale(logged_woodyveg, scale = F)") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Residual vs predictor") # Ok for most, acceptable for some.
# plot(pm3$logged_Fmetric, stats::residuals(pmTLy_lmm3)) # Same plot (I should create a custom function).
redres::plot_redres(pmTLy_lmm3, type = "raw_mar", xvar = "year") # Ok.

## Distribution of the predicted values:
par(.pardefault)
pm3$predictions <- stats::predict(object = pmTLy_lmm3, type = "response") # Extract the predicted values.
fitdistrplus::plotdist(data = pm3$predictions, histo = TRUE, demp = TRUE)
fitdistrplus::plotdist(data = pm3$tarsus_length, histo = TRUE, demp = TRUE) # Could be better.



### *** 4.3.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
pm3 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, cumdd_30) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (log)" = log10(woodyveg_vw),
                "woodyveg_vw (sqrt)" = sqrt(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind 'tarsus_length' and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(tarsus_length = pm3$tarsus_length) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -tarsus_length)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = tarsus_length, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Interestingly, we can see that for several
# predictors, extreme values tend to distort relationships that would perhaps otherwise be significant. It
# also appears here that the log-transformation of "woodyveg" is maybe counter-productive: a spline would
# certainly be better OR, as shown here, a square-root transformation!
# We can also see that "urban_intensity" present a curved relationship with "tarsus_length"!




### *** 4.3.2.4. Goodness-of-fit (GOF) and performances ----
## Computing a pseudo-R2:
performance::r2_nakagawa(pmTLy_lmm2) # [Additive model]: Marg_R2_lmm = 0.2; Cond_R2_lmm = 0.23.
performance::r2_nakagawa(pmTLy_lmm3) # [Interact. model]: Marg_R2_lmm = 0.2; Cond_R2_lmm = 0.23.

## Likelihood-ration tests (LRT) of GOF:
# Importance of the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- DHARMa::simulateLRT(m0 = pmTLy_lm1, m1 = pmTLy_lmm2, n = 500, seed = 762)
tictoc::toc() # Took ~10s to run.
# The LRT is significant, suggesting that M1 better describes the data than M0, supporting the importance of
# the random effect!

# Importance of the fixed effects (only using the LM):
pmTLy_lm0 <- stats::lm(tarsus_length ~ 1, data = pm3)
res.LRT_null <- stats::anova(object = pmTLy_lm0, pmTLy_lm1, test = "LRT")
# The test is highly significant, confirming that the model is useful to explain the data.





### ** 4.3.3. Inference and predictions ----
# __________________________________________

### *** 4.3.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
pmTLy_lmm1 <- stats::update(pmTLy_lmm2, .~. -logged_Fmetric)

res.LRT_addeff <- pbkrtest::PBmodcomp(pmTLy_lmm2, pmTLy_lmm1, nsim = 1000, seed = 129) # Took ~42s to run!
readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
                                                             "res.pmTLy_LRT_addeff.csv"))
# The LRT is ALMOST significant, indicating that our connectivity metric might be useful to improve the
# description of the data but that our models are not powerful enough to confirm it.


## For the interaction effect:
res.LRT_inteff <- pbkrtest::PBmodcomp(pmTLy_lmm3, pmTLy_lmm2, nsim = 1000, seed = 20) # Took ~32s to run!
readr::write_csv2(x = res.LRT_inteff$test, file = here::here("output", "tables",
                                                             "res.pmTLy_LRT_inteff.csv"))
# The LRT is NOT significant, indicating that our hypothesis of an interaction effect is not supported
# by the data.



### *** 4.3.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive LMM parameters")
res.pmTLy_addeff_CI_boot <- confint(pmTLy_lmm2, method="boot")
tt <- as.data.frame(res.pmTLy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmTLy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~7s to run!



### *** 4.3.3.3. Conclusion ----

# Our models does not fit the observed data very well, perhaps because it could be slightly overfitted.
# Our hypotheses are not supported by the models and no significant predictors came out.
# Anyway, diagnostics indicated that there are room for improvements: removing outliers, changing the
# transformation of some variables, accounting for possible curvature effects...





##################### *--------------------------------* #######################
##### * 4.4. Wing length: LMM --------------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 4.4.1. Initial model fit ----
# __________________________________

## Fitting a regular linear model:
pm2 %>% dplyr::filter(is.na(wing_length) == FALSE) -> pm3 # Only 141 observations left.

pmWLy_lm1 <- stats::lm(wing_length ~ logged_woodyveg + logged_Fmetric +
                         urban_intensity + manag_low + cumdd_30 + year, data = pm3)
# pmWLy_lm2 <- stats::lm(wing_length ~ scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
#                          urban_intensity + manag_low + cumdd_30 + year,
#                        data = pm3) # Interaction not significant!


## Fitting an additive LMM:
pmWLy_lmm1 <- lme4::lmer(wing_length ~ logged_woodyveg + logged_Fmetric +
                           urban_intensity + manag_low + cumdd_30 + year +
                           (1|id_nestbox), data = pm3,
                         control=lme4::lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5)))
# Gives a singular fit (RE variance = 0). I'll thus try setting a weak prior on the variance:
pmWLy_blmm1 <- blme::blmer(wing_length ~ logged_woodyveg + logged_Fmetric +
                             urban_intensity + manag_low + cumdd_30 + year +
                             (1|id_nestbox), data = pm3,
                           control=lme4::lmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=2e5))) # Convergence issue.
# Try all optimizers:
pmWLy_blmm1_all <- lme4::allFit(pmWLy_blmm1)
summary(pmWLy_blmm1_all) # Two optimizers failed to converge but give rather similar results, except the
# "nloptwrap.NLOPT_LN_BOBYQA" optimizer that computes a larger RE variance and thus, lower coefficient
# estimates. We will thus stick with "bobyqa".
pmWLy_blmm2 <- pmWLy_blmm1 # Only to keep "model2" as my additive model and "model3" as my interactive one.

## Fitting interactive (mediated) LMMs:
pmWLy_blmm3 <- blme::blmer(wing_length ~
                             scale(logged_woodyveg, scale = F) * scale(logged_Fmetric, scale = F) +
                             urban_intensity + manag_low + cumdd_30 + year +
                             (1|id_nestbox), data = pm3,
                           control=lme4::lmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=2e5))) # Convergence issue.

# # Test by removing possible overly influential observations:
# pm2_wo <- pm2[-c(49,58,50,143),]
# pmFSy_glmm3_wo <- stats::update(pmFSy_glmm3, data=pm2_wo)
# summary(pmFSy_glmm3)$AIC
# summary(pmFSy_glmm3_wo)$AIC # Strongly improved AIC, BIC, and deviance. However, it yields lower R2_glmm!





### ** 4.4.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 4.4.2.1. Residuals extraction, autocorrelation and collinearity ----
## Extracting residuals (with the {redres}):
raw_cond <- redres::compute_redres(pmWLy_blmm3) # Computes the raw conditional residuals (conditional on
# the random effects (RE)).
pearson_mar <- redres::compute_redres(pmWLy_blmm3, type = "pearson_mar") # Computes the Pearson marginal
# (not accounting for the RE) residuals.
std_cond <- redres::compute_redres(pmWLy_blmm3, type = "std_cond") # Computes the studentised cond. ones.
# Joins the residuals to the paprika data:
xxx <- cbind(pm3, raw_cond, pearson_mar, std_cond)

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = pmWLy_blmm3, n = 1000, plot = FALSE)
par(.pardefault)
plot(simu.resid) # Ok.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = pm3$coord_x, y = pm3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(pmWLy_blmm3) # Ok.
performance::check_collinearity(pmWLy_blmm3) # Ok-ish but "urban_intensity" has a VIF of 3.5!
stats::vcov(pmWLy_blmm2) # Some quite high covariances!



### *** 4.4.2.2. Distribution and homoscedasticity ----
## Assessing the normality of the residuals:
stats::shapiro.test(xxx$raw_cond) # Significant deviation from normality detected, but the Shapiro test is
# known to be extremely sensitive. So plotting would be better:
xxx %>%
  tidyr::gather(key = "type", value = "residual", 29:31) %>%
  ggplot2::ggplot(ggplot2::aes(x = residual)) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::facet_grid(. ~ type, scales = "free") +
  ggplot2::theme_bw() # The residuals are indeed a bit left-skewed, but it may be acceptable.
redres::plot_resqq(pmWLy_blmm3) # Surprisingly, there are not much departure from the Normal quantiles in
# the upper part of the residuals, even though there is a quite strong departure in the lower one.

## Assessing the normality if the random effect:
redres::plot_ranef(pmWLy_blmm3) # Same thing here!

## Assessing homogeneity of variance and influential observations:
plot(pmWLy_blmm3, type=c("p","smooth"), col.line = 2, id = 0.05, idLabels = ~.obs) # It's ok-ish even though
# the fitted values are devided in 2 and there seems to be 7 possible outliers:
pm3[c(8),] # High residuals ~= long-winged juveniles. RAS.
pm3[c(10,110,46,97,61,137),] # Low residuals ~= rather short-winged juveniles. RAS.

# Residuals vs leverage:
plot(pmWLy_blmm3, stats::rstudent(.) ~ stats::hatvalues(.))
cd <- stats::cooks.distance(pmWLy_blmm3)
plot(cd)
pm3[which(cd>0.4),] # Ok, all observations are < 0.5, so no overly influential points.
pm3[which(cd>0.1),] # Even with very conservative values, we find the same three observations.

## Residuals vs predictors:
redres::plot_redres(pmWLy_blmm3, xvar = "scale(logged_woodyveg, scale = F)") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Residual vs predictor") # Ok.
redres::plot_redres(pmWLy_blmm3, type = "raw_mar", xvar = "year") # Ok.

## Distribution of the predicted values:
par(.pardefault)
pm3$predictions <- stats::predict(object = pmWLy_blmm3, type = "response") # Extract the predicted values.
fitdistrplus::plotdist(data = pm3$predictions, histo = TRUE, demp = TRUE)
fitdistrplus::plotdist(data = pm3$wing_length, histo = TRUE, demp = TRUE) # Very bad: clear bimodal
# prediction range and reduced total range!



### *** 4.4.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
pm3 %>% dplyr::select(woodyveg_vw, pmF_d60_beta0, urban_intensity, cumdd_30) %>%
  dplyr::mutate("Fmetric" = pmF_d60_beta0,
                "Fmetric (log)" = log10(pmF_d60_beta0),
                "woodyveg_vw" = woodyveg_vw,
                "woodyveg_vw (log)" = log10(woodyveg_vw),
                "woodyveg_vw (sqrt)" = sqrt(woodyveg_vw), .keep = "unused") -> mydata
predictors <- colnames(mydata)
# Bind 'wing_length' and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(wing_length = pm3$wing_length) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -wing_length)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = wing_length, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # As before, we can see a few non-linearities that
# are caused by some extreme values.




### *** 4.4.2.4. Goodness-of-fit (GOF) and performances ----
## Computing a pseudo-R2:
performance::r2_nakagawa(pmWLy_blmm2) # [Additive model]: Marg_R2_lmm = 0.36; Cond_R2_lmm = 0.43.
performance::r2_nakagawa(pmWLy_blmm3) # [Interact. model]: Marg_R2_lmm = 0.36; Cond_R2_lmm = 0.43.

## Likelihood-ration tests (LRT) of GOF:
# Importance of the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- DHARMa::simulateLRT(m0 = pmWLy_lm1, m1 = pmWLy_blmm2, n = 500, seed = 762)
tictoc::toc() # Took ~34s to run.
# The LRT is significant, suggesting that M1 better describes the data than M0, supporting the importance of
# the random effect!

# Importance of the fixed effects (only using the LM):
pmWLy_lm0 <- stats::lm(wing_length ~ 1, data = pm3)
res.LRT_null <- stats::anova(object = pmWLy_lm0, pmWLy_lm1, test = "LRT")
# The test is highly significant, confirming that the model is useful to explain the data.





### ** 4.4.3. Inference and predictions ----
# __________________________________________

### *** 4.4.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
pmWLy_blmm1 <- stats::update(pmWLy_blmm2, .~. -logged_Fmetric)

res.LRT_addeff <- pbkrtest::PBmodcomp(pmWLy_blmm2, pmWLy_blmm1, nsim = 1000, seed = 455) # Took ~104s to run!
readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
                                                             "res.pmWLy_LRT_addeff.csv"))
# The LRT is significant, indicating that our connectivity metric might be useful to improve the
# description of the data.


## For the interaction effect:
res.LRT_inteff <- pbkrtest::PBmodcomp(pmWLy_blmm3, pmWLy_blmm2, nsim = 1000, seed = 99) # Took ~32s to run!
readr::write_csv2(x = res.LRT_inteff$test, file = here::here("output", "tables",
                                                             "res.pmWLy_LRT_inteff.csv"))
# The LRT is NOT significant, indicating that our hypothesis of an interaction effect is not supported
# by the data.



### *** 4.4.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive LMM parameters")
res.pmWLy_addeff_CI_boot <- confint(pmWLy_blmm2, method="boot")
tt <- as.data.frame(res.pmWLy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.pmWLy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~27s to run!



### *** 4.4.3.3. Conclusion ----

# Our models does not fit the observed data very well, perhaps because it could be slightly overfitted.
# Once again, only the additive effect of the connectivity metric was supported by the data, but results
# could be different with a more powerful set-up and by removing some possible outliers.





########################## ************************************************* ###############################
par(.pardefault)
############################################ TO DO LIST ####################################################
# - Refaire tourner tous les modèles avec NTITS
# - Explorer chaque modèle pour voir qu'est-ce qui marche le mieux et s'il y a une spécification unique qui
#   va bien!!! ATTENTION à la gamme des variables explorées (transformations???????)!!!!
# - Si rien de bien concluant, essayer en fusionnant les années pour supprimer les RE nichoirs.

# 3) Re-diagsnose models with inputs from the DHARMa vignette (including conditional simu)!!! Then:
# 4) Improve sub-conclusions! Saying that quasi is not a good option??? Or try anyway with a package that
#    does it OR BETA-BINOMIAL? --> But too many parameters (ZI+OI)!!! I should wait for the MERGE!
#    Or simply try modelling the counts with ZI+Comreg avec RE??? And/or PM+CC and/or MERGING years and
#    nestboxes and hope that it alleviates these problems?????
# 5) Move on to another Y: i) LMM_morpho; ii) LMM_mass, wing...; iii) COM-Poisson pour clutch_size; iv) ZI!
# 6) Try the Conway-Maxwell Poisson (Shmueli et al, 2005*) avec {COMPoissonReg} (Sellers & Lotze, 2015*),
#    but search first if that exist in Mixed Model! (Same for Zero-Inflated models)! Or quasi-likelihood,
#    or generalised-Poisson (but {VGAM} does not do it anymore). I could also try it without REs...
#    The problem with all that is that quasi-MLE, GP or COM-Poisson prevent mixed-models and possibly
#    LR-tests, right? At least the quasi-MLE does. Is bird breeding a Poisson process???

# 7) Reunite PM & CC while keeping other predictors (for exploratory research)! But beware of coding,
#    what does the intercept mean? Which reference group?

# 1) Consider removing overly influential observations?
# 2) Consider merging years and averaging nestboxes??? Some of the GLMs gave interesting results, sometimes
#    even more when "year" was removed.
# 8) Compare all methods of estimation and inference (cf. inference bolker example) for the **same
#    model** to see if any is making a difference?
# 9) Explore other predictors (and different scales)! E.g. AICc-based model selection?
########################## ************************************************* ###############################
