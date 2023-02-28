########################## *--------------------------------------------------* ########################
########################## Inferential modelling for all tit nestlings (ntits)  ########################
########################## *--------------------------------------------------* ########################

# The functions of this R file are meant to wrap all formal inference modelling related to both species
# together, as opposed to per-species models, "preparation modelling" (cf. previous scripts) and
# "exploratory modelling" that will perhaps be made afterwards; i.e. after we have formally tested our
# research hypotheses in a robust inferential framework. The so-called "exploratory modelling" is related
# to additional questions and perspectives that will have, by nature, far less support as the data have
# already been used for formal testing (so type-I error rates cannot be guaranteed anymore)!

# For now, this script should be run after having sourced the EDA script (because no function yet)!!!





# -------------------------------- #
##### 1. Modelling clutch size #####
# -------------------------------- #

##### * 1.1. Clutch size: COM-Poisson GLMM -------------------------------------
# ---------------------------------------------------------------------------- #
### ** 1.1.1. Initial model fit ----
# __________________________________

## To remove probable outliers (see initial 'ttCy_comglmm1' diagnostics below):
ntits3 <- ntits2[-c(111,156,170,181,210,227,326,362,374,379),]

## Fitting a regular Poisson regression:
ttCy_glm1 <- stats::glm(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                          urban_intensity + manag_intensity +
                          light_pollution + noise_m + traffic +
                          cumdd_30 + year,
                        data = ntits3, family = "poisson")

## Fitting a regular Poisson GLMM:
ttCy_glmm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                 urban_intensity + manag_intensity +
                                 light_pollution + noise_m + traffic +
                                 cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = "poisson")


## Fitting a regular Conway-Maxwell (COM) Poisson regression (GLM):
ttCy_comglm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + year,
                                 data = ntits3, family = glmmTMB::compois(link = "log"),
                                 dispformula = ~1) # Intercept only 'nu' (default).
# # OR:
# ttCy_comglm1b <- COMPoissonReg::glm.cmp(formula.lambda =
#                                           clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
#                                           urban_intensity + manag_intensity +
#                                           light_pollution + noise_m + traffic +
#                                           cumdd_30 + year,
#                                      data = ntits3, formula.nu = ~1) # Intercept only 'nu' (default).

## Fitting a regular Conway-Maxwell (COM) Poisson mixed model (GLMM):
ttCy_comglmm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_30 + year + (1|id_nestbox),
                                 data = ntits3, family = glmmTMB::compois(link = "log"),
                                 dispformula = ~1) # Rather long to fit.
ttCy_comglmm1_cent <- glmmTMB::glmmTMB(clutch_size ~ c.log_patch_area + c.log_F_metric_d2b1 + species +
                                         urban_intensity + manag_low + manag_high +
                                         light_pollution + c.noise_m + c.traffic +
                                         c.cumdd_30 + year + (1|id_nestbox),
                                 data = ntits3, family = glmmTMB::compois(link = "log"),
                                 dispformula = ~1) # Rather long to fit.

## Fitting the interaction model (COM-Poisson GLMM):
ttCy_comglmm2 <- glmmTMB::glmmTMB(clutch_size ~
                                    scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                    species +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_30 + year + (1|id_nestbox),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1) # Rather long to fit.
ttCy_comglmm2_cent <- glmmTMB::glmmTMB(clutch_size ~ c.log_patch_area * c.log_F_metric_d2b1 + species +
                                         urban_intensity + manag_low + manag_high +
                                         light_pollution + c.noise_m + c.traffic +
                                         c.cumdd_30 + year + (1|id_nestbox),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1) # Rather long to fit.
summary(ttCy_glm1) # AIC = 1711.2 (vs 1811.4 with the outliers).
summary(ttCy_glmm1) # AIC = 1713.2 (vs 1813.4 with the outliers).
summary(ttCy_comglm1) # AIC = 1408.6 (vs 1669.5 with the outliers).
# summary(ttCy_comglm1b) # AIC = 1411.8 (so it's not exactly the same?! REML?).
summary(ttCy_comglmm1) # AIC = 1407.1 (vs 1626.7 with the outliers).
summary(ttCy_comglmm1_cent) # AIC = 1407.1.
summary(ttCy_comglmm2) # AIC = 1409 (vs 1673.5 with the outliers), so the interaction worsen the fit ! And
# note that using the "Rr_metric" is even worse!
summary(ttCy_comglmm2_cent) # AIC = 1409.
# It seems that, if the inclusion of a random effect (RE) did not improve the fit but accounting for
# the likely underdispersion quite strongly improved the fit! I will thus carry on with 'ttCy_comglmm1'
# to the diagnostic part and assess whether the use of the RE is truly justified or not and if the
# model behaves as expected.
# UPDATE: diagnostics ran for 'ttCy_comglmm1' (initial model) indicated that the model fit the data
# relatively well. However, they also showed that the model was slightly off likely because there was true
# outliers in the data as the very low clutch sizes observed were probably generated by another process.
# Therefore, in a second step, I removed them and re-run diagnostics (which yielded green lights). Then I also
# explored a few reasonable variations of the same model by trying different proxies of the same variable
# (e.g. different connectivity metrics, "woody_area" instead of "patch_area"), adding or using "site" as RE,
# and slightly tuning the (nu) dispersion model (see below).
# However, explorations were not very conclusive.





### ** 1.1.2. Exploratory modelling ----
# ______________________________________

## Using "id_patch" as RE:
ttCy_comglmm1b <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
## Only using "site" as RE:
ttCy_comglmm1c <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|site),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
## Using two RE:
ttCy_comglmm1ac <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox) + (1|site),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
ttCy_comglmm1bc <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch) + (1|site),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
summary(ttCy_comglmm1b) # AIC = 1408.3 vs 1407.1.
summary(ttCy_comglmm1c) # AIC = 1406.9.
summary(ttCy_comglmm1ac) # AIC = 1407.6.
summary(ttCy_comglmm1bc) # AIC = 1408.8.
# So switching the random effects or using two of them does not seem to change things much.


## Tuning the NU parameter (dispersion model):
ttCy_comglmm1d <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_patch_area+log_F_metric_d2b1+cumdd_30+year)
ttCy_comglmm1e <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_patch_area+log_F_metric_d2b1+min_t_before+year)
ttCy_comglmm1f <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_F_metric_d2b1+urban_intensity+cumdd_30+year)
ttCy_comglmm1g <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_F_metric_d2b1+cumdd_30+year)
ttCy_comglmm1h <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~cumdd_30+year)
ttCy_comglmm1i <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_F_metric_d2b1+year)
ttCy_comglmm1j <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_F_metric_d2b1+cumdd_30)
ttCy_comglmm1k <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~min_t_before+cumdd_30)
summary(ttCy_comglmm1d) # AIC = 1416.3 vs 1407.1.
summary(ttCy_comglmm1e) # AIC = 1416.3.
summary(ttCy_comglmm1f) # AIC = 1416.3.
summary(ttCy_comglmm1g) # AIC = 1414.4.
summary(ttCy_comglmm1h) # AIC = 1413.2.
summary(ttCy_comglmm1i) # AIC = 1412.7.
summary(ttCy_comglmm1j) # AIC = 1408.7.
summary(ttCy_comglmm1k) # AIC = 1409.5.

## Test of the effect of patch_edge:patch_area:
ntits3 %>% dplyr::mutate(edgarea_ratio = patch_perim/patch_area) -> xx
hist(xx$edgarea_ratio)
hist(log10(xx$edgarea_ratio))
summary(xx$edgarea_ratio)

zzz <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species + edgarea_ratio +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = xx, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
zzz2 <- glmmTMB::glmmTMB(clutch_size ~ edgarea_ratio + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_nestbox),
                                   data = xx, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
summary(zzz) # AIC = 1409 vs 1407.1 (but neither "patch_area" nor "edarea_ratio" significant).
summary(zzz2) # AIC = 1408.3 (with a significant "edarea_ratio" effect (+)).





### ** 1.1.3. Diagnostics and assumption checks ----
# __________________________________________________

### *** 1.1.3.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(ttCy_comglmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Ok-ish but there are a few low residuals.
# performance::check_outliers(ttCy_comglmm1) # Does not work for this type of model.
ntits3[which(resid < -4),] # Lowest residuals are nestboxes with very small clutch sizes.

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = ntits3$year, y = resid) # Seems rather ok although we once again find patterns linked to the
# sometimes odd distribution of some predictors. However, be reminded that simulated residuals will be
# more useful).
# plot(ttCy_comglmm1b, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(ttCy_comglmm1b, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttCy_comglmm1, n = 1000, re.form = NULL) # The
# 're.form' argument is to base simulations on the model unconditional of the random effects (and only works
# for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Significant deviations and outliers detected!
DHARMa::outliers(simu.resid) # No potential outliers.
# ntits2[c(156,170,181,210,227,314,362,367),] # They have surprisingly low clutch sizes with regards to their
# locations and their adjacent observations. They may well be true outliers (whose clutch sizes are function
# of other processes).

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Nope!
performance::check_autocorrelation(ttCy_comglmm1) # Ok.
performance::check_collinearity(ttCy_comglmm1) # Ok-ish, but "Fmetric" > 4.
stats::vcov(ttCy_comglmm1) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_F_metric_d2b1)
DHARMa::plotResiduals(simu.resid, form = ntits3$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$light_pollution)
DHARMa::plotResiduals(simu.resid, form = ntits3$noise_m)
DHARMa::plotResiduals(simu.resid, form = ntits3$traffic)
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = ntits3$species)
DHARMa::plotResiduals(simu.resid, form = ntits3$year)
# All these plots are ok!



### *** 1.1.3.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
AER::dispersiontest(object = ttCy_glm1, alternative = c("less")) # Significant underdispersion!
DHARMa::testDispersion(simu.resid, alternative = "less") # Ok.

## Theoretical count distribution:
theo_count <- COMPoissonReg::rcmp(n = nrow(ntits3), lambda = mean(ntits3$clutch_size), nu = 1.05) # The 'nu'
# parameter should be chosen by trial-and-errors.
tc_df <- data.frame(theo_count)

ggplot2::ggplot(ntits3, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that clutch_size could be following a COM-Poisson distribution of parameter nu~1.1!

## Distribution of the predicted counts:
pred_counts <- stats::predict(object = ttCy_comglmm1, type = "response") # Extract the predicted counts.
par(mfrow= c(1,2))
hist(pred_counts, main = "Predicted counts", xlab = "Number of laid eggs")
hist(ntits3$clutch_size, main = "Observed counts", xlab = "Number of laid eggs") # The models' predictions
# are very similar and relatively acceptable (although too narrow).



### *** 1.1.3.3. Linearity ----
# For the sake of further exploration, I also plot variants of our predictors:
ntits3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
                         log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
                         log_F_metric_d2b1,
                         Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
                         urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
                         light_pollution, noise_m,
                         cumdd_30, min_t_before) -> mydata
predictors <- colnames(mydata)
# Bind log(Y) and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(log_y = log(ntits3$clutch_size)) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity seems respected.



### *** 1.1.3.4. Model goodness-of-fit (GOF) and performances ----
# GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(ttCy_comglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(ttCy_comglmm1)) # p = 0.65, indicating that there is no
# significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely complicated
# topic and interpretations are not straightforward.

# Computing a pseudo-R2:
performance::r2_nakagawa(ttCy_comglmm1) # [Additive model]: Marg_R2_glmm = 0.09; Cond_R2_glmm = 0.1.
# Does not work for C and D models because it cannot account for the dispersion model.

## Likelihood-based evaluation of effects inclusion:
# For the "site" random-effects (RE):
ttCy_comglmm2 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_30 + year + (1|id_patch) + (1|site),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1) # Rather long to fit (~3-4 min)!
summary(ttCy_comglmm2) # AIC = 1408.8.
# The non-mixed model gives AIC = 1426 so worse than the mixed-model (AIC = 1408.6) with only "id_patch" as
# RE. The one with both RE gives AIC = 1408.8, so it seems like the use of a mixed model is supported by the
# data but not of that with "site" as an additional RE.

## For the whole model:
ttCy_comglmm0 <- glmmTMB::glmmTMB(clutch_size ~ 1 + (1|id_patch),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1)
res.LRT_null <- stats::anova(object = ttCy_comglmm0, ttCy_comglmm1, test = "LRT")
# The test is significant, confirming that the model is useful to explain the data.



### *** 1.1.3.5. Posterior predictive simulations ----
# Predicted counts:
par(.pardefault)
obsprop <- prop.table(table(ntits3$clutch_size))
sims <- stats::simulate(ttCy_comglmm1, nsim = 1000)
nsim4 <- colSums(sims == 4) # Number of fours (min obs value)
par(las=4,bty="l")
plot(pt <- prop.table(table(nsim4)),
     ylab="Probability", xlab="Number of fours (true == 1)")
(obs4 <- sum(ntits3$clutch_size == 4))
points(obs4, 0.002, col="red", pch=16, cex=2) # See y values in obsprop!

nsim9 <- colSums(sims == 9) # Number of nines (modal obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim9)),
     ylab="Probability", xlab="Number of nines (true == 86)")
(obs9 <- sum(ntits3$clutch_size == 9))
points(obs9, 0.22, col="red", pch=16, cex=2)

nsim14 <- colSums(sims == 14) # Number of fourteens (max obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim14)),
     ylab="Probability", xlab="Number of fourteens (true == 5)")
(obs14 <- sum(ntits3$clutch_size == 14))
points(obs14, 0.013, col="red", pch=16, cex=2)
# These three examples confirm that the model tends to overpredict a bit.





### ** 1.1.4. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ ----

### *** 1.1.4.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## Parametric bootstrap to test the additive effect of the connectivity metric:
ttCy_comglmm0 <- stats::update(ttCy_comglmm1, .~. -log_F_metric_d2b1)
summary(ttCy_comglmm0)$AIC # AIC = 1407.2 vs 1408.3 (hypothesis likely not validated)!

# I do not run PB-based LRT for now as they take too long to run.

# tictoc::tic("Parametric bootstrap LRT for the additive effect")
# res.LRT_inteff <- DHARMa::simulateLRT(m0 = ttCy_comglmm0, m1 = ttCy_comglmm1b, n = 500, seed = 10)
# tt <- as.data.frame(cbind(res.LRT_inteff$method,
#                           res.LRT_inteff$data.name,
#                           res.LRT_inteff$statistic,
#                           res.LRT_inteff$p.value))
# rownames(tt) <- NULL
# tt %>% dplyr::rename("Method" = V1,
#                      "Models" = V2,
#                      "Log Likelihood (M1/M0)" = V3,
#                      "p-value" = V4) -> tt
# readr::write_csv2(x = tt, file = here::here("output", "tables", "res.ttCy_LRT_addeff.csv"))
# tictoc::toc() # DISCLAIMER: took >25h to run!!!!
# The PB-based LRT is ...
# NOTE: initially, normally I would use the more efficient 'pbkrtest::PBmodcomp()' instead of the
# 'DHARMa::simulateLRT()' function, but it doesn't work with {glmmTMB} objects.


## Parametric bootstrap to test the interactive effect of the connectivity metric:
# To save some time, I don"t even bother computing the PB-based LRT for the interaction effect, it won't be
# significant. I should improve my proxies and my models first.



### *** 1.1.4.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for the additive COM-Poisson GLMM parameters")
res.ttCy_addeff_CI_boot <- confint(ttCy_comglmm1b, method="boot")
tt <- as.data.frame(res.ttCy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttCy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~2h10 to run!



### *** 1.1.4.3. Conclusion ----
# For the initial model:
summary(ttCy_comglmm1) # AIC = 1408.3 and Marg_R2_glmm = 0.09; Cond_R2_glmm = 0.1.
# Diagnostics ran for 'ttCy_comglmm1' (initial model) indicated that the model fit the data relatively well.
# There are no major assumption violations and the use of "id_patch" as RE was warranted by the data, and so
# did the use of a COM-Poisson distribution.
# Still, the model tends to over-predict a bit.
## Significant variables: patch_area (-), speciesCC (+), urban_intensity (+), noise_m (+), cumdd_30 (-),
# year2020 (-) and year2021 (-).
## Almost significant variables: traffic (-).
## Hypothesis 1 likely not validated (AIC = 1407.2 vs 1408.3), and hypothesis 2 neither!


# For the exploratory models:
summary(ttCy_comglmm1b) # AIC = 1391.3 and Marg_R2_glmm = 0.09; Cond_R2_glmm = 0.1.
## Significant variables: woodyveg (-), speciesCC (+), light_pollution (-), noise_iq (+), cumdd_30 (-), and
# year2021 (-).
## Almost significant variables: Fmetric_d2b1 (+), year2020 (-).
## Hypothesis 1 likely not validated (AIC = 1392 vs 1391.3)!
summary(ttCy_comglmm1c) # AIC = 1390.5 and no R2 for dispersion models.
## Significant variables: woodyveg (-), speciesCC (+), light_pollution (-), noise_iq (+), cumdd_30 (-),
# year2020 (-), year2021 (-).
## Almost significant variables: Fmetric_d2b1 (+).
## Hypothesis 1 likely not validated (AIC = 1391.6 vs 1390.5)!
summary(ttCy_comglmm1d) # AIC = 1388.1 and no R2 for dispersion models.
## Significant variables: woody_area (-), Fmetric_d2b1 (+), speciesCC (+), noise_iq (+), cumdd_30 (-),
# year2020 (-), year2021 (-).
## Almost significant variables: light_pollution (-).
## Hypothesis 1 likely validated (AIC = 1392.4 vs 1388.1)! While hypothesis 2 (interaction) seems rejected for
# all model specifications.

# Diagnostics for these models were mostly ok, there was no outliers, deviations or residual autocorrelation.
# However, as could be expected, some VIF values for the D model ('ttCy_comglmm1d') were rather high even
# though the covariance values were ok.
# It also appears that the data could be following a COM-Poisson distribution with a nu ~ 1.1.
# Predictions are fairly ok but still too narrow (and extremely similar between B, C, D)!





########################## ************************************************* ###############################
# -------------------------------------- #
##### 2. Modelling hatching success #####
# -------------------------------------- #

##### * 2.1. Hatching success: Binomial GLMM -----------------------------------
# ---------------------------------------------------------------------------- #
### ** 2.1.1. Initial model fit ----
# __________________________________

## To remove probable outliers (see diagnostics with them):
ntits3 <- ntits2[-c(which(ntits2$brood_size == 0)),] # I delete the 28 observations for which no eggs
# hatched as they were likely generated by another process than the one controlling overall hatching
# success (e.g. desertion, predation).

## Fitting a regular binomial GLM:
ttHSy_glm1 <- stats::glm(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                           urban_intensity + manag_intensity +
                           light_pollution + noise_m + traffic +
                           cumdd_30 + year,
                         weights = clutch_size, # Prior weights!
                         data = ntits3, family = "binomial") # Weights should not be forgotten. Otherwise, the
# formulation should be: cbind(brood_size, clutch_size-brood_size)!

## Fitting a binomial GLMM:
ttHSy_glmm1 <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                  urban_intensity + manag_intensity +
                                  light_pollution + noise_m + traffic +
                                  cumdd_30 + year + (1|id_nestbox),
                           weights = clutch_size, data = ntits3, family = "binomial")

## Fitting an interactive binomial GLMM:
ttHSy_glmm2 <- glmmTMB::glmmTMB(brood_size/clutch_size ~
                                  scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                  species +
                                  urban_intensity + manag_intensity +
                                  light_pollution + noise_m + traffic +
                                  cumdd_30 + year + (1|id_nestbox),
                                weights = clutch_size, data = ntits3, family = "binomial")
summary(ttHSy_glm1) # AIC = 775.7.
summary(ttHSy_glmm1) # AIC = 734.
summary(ttHSy_glmm2) # AIC = 736, so the interaction does not seem supported by the data!
# It seems that, if the inclusion of a random effect (RE) improves the fit! I will thus carry on with the
# last model to the diagnostic part and assess whether the use of the RE is truly justified or not and if
# the model behaves as expected.
# UPDATE: diagnostics ran for 'ttHSy_glmm1' (the initial models) indicated that the models fit the data
# relatively well although some modelling assumptions were not fully met and the models under-predicted.
# Overall, diagnostics suggested that observations with brood sizes of 0 were likely outliers generated
# by another process than the one driving hatching rate. Incidentally, they were mostly the same as the
# outliers of clutch_size!
# Therefore, in a second step, I removed them and re-run diagnostics (which yielded green lights). Then
# I also explored a few reasonable variations of the same model by trying different proxies of the same
# variable (e.g. different connectivity metrics, "woody_area" instead of "patch_area"), adding or using
# "site" as RE.
# However, explorations were not very conclusive.





### ** 2.1.2. Exploratory modelling ----
# ______________________________________

## Using "site" as RE:
ttHSy_glmm1a <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + year + (1|site),
                                 weights = clutch_size, data = ntits3, family = "binomial")

## Using "id_patch" as RE:
ttHSy_glmm1b <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + year + (1|id_patch),
                                 weights = clutch_size, data = ntits3, family = "binomial")
summary(ttHSy_glmm1a) # AIC = 773.5 vs 734 (but "patch_area" is significant (+))!
summary(ttHSy_glmm1b) # AIC = 739.5.
# So the use of these RE does not seem warranted by the data.


## Deleting "traffic":
ttHSy_glmm1c <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m +
                                   cumdd_30 + year + (1|id_nestbox),
                                 weights = clutch_size, data = ntits3, family = "binomial")
## Changing the connectivity metric:
ttHSy_glmm1d <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + Rr_metric_d2c1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + year + (1|id_nestbox),
                                 weights = clutch_size, data = ntits3, family = "binomial")
## Using "cumdd_60":
ttHSy_glmm1e <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_60 + year + (1|id_nestbox),
                                 weights = clutch_size, data = ntits3, family = "binomial")
## Adding "min_t_before":
ttHSy_glmm1f <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + min_t_before + year + (1|id_nestbox),
                                 weights = clutch_size, data = ntits3, family = "binomial")
summary(ttHSy_glmm1c) # AIC = 732.5 vs 734.
summary(ttHSy_glmm1d) # AIC = 734.1.
summary(ttHSy_glmm1e) # AIC = 733.3.
summary(ttHSy_glmm1f) # AIC = 735.
# Not very conclusive.





### ** 2.1.3. Diagnostics and assumption checks ----
# __________________________________________________

### *** 2.1.3.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(ttHSy_glmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Very strange distribution and clear outliers!
performance::check_outliers(ttHSy_glm1) # Ok (note: does not work for glmmTMB models).
ntits3[which(resid < -0.2),] # Nestboxes with the lowest residuals are either those with the lowest brood
# sizes or with quite strong decline in counts between clutch and brood sizes (so low HS)!

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = ntits3$log_patch_perim, y = resid) # Seems rather ok although we once again find patterns linked to the
# sometimes odd distribution of some predictors. However, be reminded that simulated residuals will be
# more useful).
# plot(ttHSy_glmm1, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(ttHSy_glmm1, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttHSy_glmm1, n = 1000, re.form = NULL) # The
# 're.form' argument is to base simulations on the model unconditional of the random effects (and only works
# for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Ok.
DHARMa::outliers(simu.resid) # Ok.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(ttHSy_glmm1) # Ok-ish.
performance::check_collinearity(ttHSy_glmm1) # Ok-ish but VIF > 4 for the F-metric!
stats::vcov(ttHSy_glmm1) # But values of the covariance matrix seem ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_perim)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_woody_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_woody_vw)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_F_metric_d2b1)
DHARMa::plotResiduals(simu.resid, form = ntits3$Rr_metric_d2c1)
DHARMa::plotResiduals(simu.resid, form = ntits3$species)
DHARMa::plotResiduals(simu.resid, form = ntits3$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_herb_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$sqrt_built_vol)
DHARMa::plotResiduals(simu.resid, form = ntits3$light_pollution)
DHARMa::plotResiduals(simu.resid, form = ntits3$noise_m)
DHARMa::plotResiduals(simu.resid, form = ntits3$traffic)
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_60)
DHARMa::plotResiduals(simu.resid, form = ntits3$min_t_before)
DHARMa::plotResiduals(simu.resid, form = ntits3$year)
# All these plots are ok.



### *** 2.1.3.2. Distribution and dispersion ----
## Assessing over or under-dispersion:
DHARMa::testDispersion(simu.resid) # Ok.
performance::check_overdispersion(x = ttHSy_glmm1) # Ok.

## Distribution of the predicted probabilities:
probabilities <- stats::predict(object = ttHSy_glmm1, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities, main = "Predicted proportions", xlab = "Hatching success")
hist(ntits3$brood_size/ntits3$clutch_size, main = "Observed proportions", xlab = "Hatching success")
# The model fits the data pretty well but tends to over-predict.



### *** 2.1.3.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
ntits3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
                         log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
                         log_F_metric_d2b1,
                         Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
                         urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
                         light_pollution, noise_m,
                         cumdd_30, min_t_before) -> mydata
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
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Ok-ish.



### *** 2.1.3.4. Goodness-of-fit (GOF) and performances ----
## GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(ttHSy_glmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(ttHSy_glmm1)) # p = 0.55 so not significant lack of fit!

## Computing a pseudo-R2:
performance::r2_nakagawa(ttHSy_glmm1) # [Additive model]: Marg_R2_glmm = 0.07; Cond_R2_glmm = 0.26.

## Likelihood-based evaluation of effects inclusion:
# Importance of the random-effects (RE):
ttHSy_glmm1c <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + year + (1|id_nestbox) + (1|site),
                                 weights = clutch_size, data = ntits3, family = "binomial")
summary(ttHSy_glmm1c) # The non-mixed model gives AIC = 775.7 while the mixed-model with "id_patch" gave
# AIC = 739.5; the one with only "site" as RE gave AIC = 773.5; the one with only "id_nestbox" as RE gave
# AIC = 734; the one with both "id_patch" and "site" gave AIC = 741.5; and the one with both "id_nestbox"
# and "site" gave AIC = 736.
# The use of a mixed model seems warranted by the data.

# Importance of the fixed effects:
ttHSy_glmm0 <- glmmTMB::glmmTMB(brood_size/clutch_size ~ 1 + (1|id_patch),
                                  weights = clutch_size, data = ntits3, family = "binomial")
summary(ttHSy_glmm0) # AIC = 758.6 vs 739.5, so the model is slightly better but not that much!





### ** 2.1.4. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ ----

### *** 2.1.4.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## Parametric bootstrap to test the additive effect of the connectivity metric:
ttHSy_glmm0 <- stats::update(ttHSy_glmm1, .~. -log_F_metric_d2b1)
summary(ttHSy_glmm0) # AIC = 737.6 vs 739.5 (hypothesis likely not validated)!
# I do not run PB-based LRT for now as they take too long to run.

# tictoc::tic("Parametric bootstrap LRT for the interaction model")
# res.LRT_inteff <- DHARMa::simulateLRT(m0 = ttHSy_ziglmm0, m1 = ttHSy_glmm1, n = 500, seed = 21)
# tt <- as.data.frame(cbind(res.LRT_inteff$method,
#                           res.LRT_inteff$data.name,
#                           res.LRT_inteff$statistic,
#                           res.LRT_inteff$p.value))
# rownames(tt) <- NULL
# tt %>% dplyr::rename("Method" = V1,
#                      "Models" = V2,
#                      "Log Likelihood (M1/M0)" = V3,
#                      "p-value" = V4) -> tt
# readr::write_csv2(x = tt, file = here::here("output", "tables", "res.ttHSy_LRT_addeff.csv"))
# tictoc::toc() # DISCLAIMER: took ~3h35 to run!
# # NOTE: initially, normally I would use the more efficient 'pbkrtest::PBmodcomp()' instead of the
# # 'DHARMa::simulateLRT()' function, but it doesn't work with {glmmTMB} objects.


## Parametric bootstrap to test the interactive effect of the connectivity metric:
# To be run (retrieve code from previous scripts)!



### *** 2.1.4.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive GLMM parameters")
res.ttHSy_addeff_CI_boot <- confint(ttHSy_glmm1, method="boot")
tt <- as.data.frame(res.ttHSy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttHSy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~2h10 to run!



### *** 2.1.4.3. Conclusion ----
# For the initial model:
summary(ttHSy_glmm1) # AIC = 739.5 and Marg_R2_glmm = 0.07; Cond_R2_glmm = 0.26.
# Diagnostics ran for 'ttHSy_glmm1' (initial model) indicated that the model fit the data relatively well
# even though we could see that:
# - The model over-predicts high hatching rates.
# - Interestingly, "species" was not found significant. Yet, as here we modelled a proportion and not raw
#   counts, it is possible.
# On the other hand, diagnostics also indicated that the use of a RE was appropriate.
## Significant variables: all 3 years (+).
## Almost significant variables: none.
## Hypothesis 1 likely not validated (AIC = 737.6 vs 739.5)!


# For the exploratory models:
summary(ttHSy_glmm1d) # AIC = 700.4 and Marg_R2_glmm = 0.11; Cond_R2_glmm = 0.31.
## Significant variables: woody_area (---), F-metric_d2b1 (++), all 3 years (++).
## Almost significant variables: noise_iq (-).
## Hypothesis 1 likely not validated (AIC = 702.8 vs 700.4)!
summary(ttHSy_glmm1e) # AIC = 701.2 and Marg_R2_glmm = 0.11; Cond_R2_glmm = 0.31.
## Significant variables: woody_area (---), F-metric (++), noise_iq (-), all 3 years (++).
## Almost significant variables: speciesCC (+).
## Hypothesis 1 likely not validated (AIC = 703.5 vs 701.2)!
summary(ttHSy_glmm1f) # AIC = 698.3 and Marg_R2_glmm = 0.12; Cond_R2_glmm = 0.31.
## Significant variables: woody_area (---), Fmetric (++), speciesCC (+), all 3 years (++), and the
# INTERACTION EFFECT (++)!
## Almost significant variables: noise_iq (-).
## Hypothesis 2 (and thus 1 as well) possibly validated!
summary(zzz) # AIC = 697.8 and Marg_R2_glmm = 0.14; Cond_R2_glmm = 0.33.
## Significant variables: woody_area (---), Fmetric_d2b1 (++), all 3 years (++), and the INTERACTION
# EFFECT (++)!
## Almost significant variables: speciesCC (+), noise_iq (-).
## Hypothesis 2 (and thus 1 as well) possibly validated!

# Diagnostics for these models were mostly ok, there was no outliers, deviations, dispersion or
# distributional problems, even though there was some residual autocorrelation. However, there was some
# worrying collinearity for model E and F, and clearly problematic for model 'ttHSy_glmm1d' (and 'zzz').
# Importantly, results for hypothesis 2 seem consistent whether we use "herby_area", "urban_intensity" or
# none of these 2 variables, while the latter improves both VIF and AIC values.
# Predictions are fairly ok but the models still under-predict low hatching rates (and predictions were
# extremely similar among models)!
# As for clutch_size, it is noteworthy that the models are not that better compared to a true null model!





########################## ************************************************* ###############################
# ------------------------------------- #
##### 3. Modelling fledging success #####
# ------------------------------------- #

##### * 3.1. Fledging success: Beta-binomial GLMM -----------------------------------
# ---------------------------------------------------------------------------- #
### ** 3.1.1. Initial model fit ----
# __________________________________

# In accordance with previous diagnostics, I remove observations with brood_size = 0 since they likely are
# true outliers (see comments in the previous sections):
ntits3 <- ntits2[-c(which(ntits2$brood_size == 0)),]
# Preliminary diagnostics for the initial models (not shown here but easily reproducible) also identified
# significant overdispersion for fledging success. To account for that, Harrison (2014 - links:
# https://doi.org/10.7717/peerj.616) recommends to compare the use of an observation-level random effect
# (OLRE) with the use of a beta-binomial model, so that's what we did (see below):
ntits3$id_obs <- as.factor(1:nrow(ntits3)) # To create an observation-level RE (OLRE).

# Note also that in these models, we included "clutch_size" as a predictor because it is likely an important
# predictor of fledging success. However, as could have been expected, it lead to pretty strong
# multicollinearity issues with "species" as it is one of the strongest predictors of tits' clutch size. We
# thus removed "species" of the list of predictors for the benefit of "clutch_size" as the removal of "species"
# had a lesser effect to reduce collinearity.
# Finally, note also that we replaced "cumdd_between" instead of "cumdd_30".


## Fitting a regular binomial GLM:
ttFS_bin_glm1 <- stats::glm(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                           clutch_size +
                           urban_intensity + manag_intensity +
                           light_pollution + noise_m + traffic +
                           cumdd_between + year,
                         weights = brood_size, # Prior weights!
                         data = ntits3, family = "binomial") # Weights should not be forgotten. Otherwise,
# the formulation should be: Y = cbind(fledgling_nb, brood_size-fledgling_nb)!

## Fitting a beta-binomial GLM:
ttFS_bbin_glm1 <- stats::glm(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                               clutch_size +
                               urban_intensity + manag_intensity +
                               light_pollution + noise_m + traffic +
                               cumdd_between + year,
                             weights = brood_size, # Prior weights!
                             data = ntits3,
                             family = glmmTMB::betabinomial(link = "logit"))


## Fitting a binomial GLMM:
ttFS_bin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                  clutch_size +
                                  urban_intensity + manag_intensity +
                                  light_pollution + noise_m + traffic +
                                  cumdd_between + year + (1|id_nestbox),
                                weights = brood_size, data = ntits3,
                                family = "binomial")

## Fitting a binomial GLMM with an OLRE:
ttFS_bin_glmm1_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                  clutch_size +
                                  urban_intensity + manag_intensity +
                                  light_pollution + noise_m + traffic +
                                  cumdd_between + year + (1|id_obs) + (1|id_nestbox),
                                weights = brood_size, data = ntits3,
                                family = "binomial")

## Fitting a beta-binomial GLMM:
ttFS_bbin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                  clutch_size +
                                  urban_intensity + manag_intensity +
                                  light_pollution + noise_m + traffic +
                                  cumdd_between + year + (1|id_nestbox),
                                weights = brood_size, data = ntits3,
                                family = glmmTMB::betabinomial(link = "logit"))



## Fitting a zero-inflated (ZI) binomial GLMM:
ttFS_zibin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                    clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year + (1|id_nestbox),
                                  weights = brood_size, data = ntits3,
                                  family = "binomial",
                                  ziformula = ~1) # Intercept only.

## Fitting a zero-inflated (ZI) binomial GLMM with OLRE:
ttFS_zibin_glmm1_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                    clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year + (1|id_obs) + (1|id_nestbox),
                                  weights = brood_size, data = ntits3,
                                  family = "binomial",
                                  ziformula = ~1) # Intercept only.

## Fitting a zero-inflated (ZI) beta-binomial GLMM:
ttFS_zibbin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                    clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year + (1|id_nestbox),
                                  weights = brood_size, data = ntits3,
                                  family = glmmTMB::betabinomial(link = "logit"),
                                  ziformula = ~1) # Intercept only.
ttFS_zibbin_glmm1_cent <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ c.log_patch_area + c.log_F_metric_d2b1 +
                                             c.clutch_size +
                                             urban_intensity + manag_low + manag_high +
                                             light_pollution + c.noise_m + c.traffic +
                                             c.cumdd_between + year + (1|id_nestbox),
                                  weights = brood_size, data = ntits3,
                                  family = glmmTMB::betabinomial(link = "logit"),
                                  ziformula = ~1) # Intercept only.
## Fitting a zero-inflated (ZI) beta-binomial GLM:
ttFS_zibbin_glm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                    clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year,
                                  weights = brood_size, data = ntits3,
                                  family = glmmTMB::betabinomial(link = "logit"),
                                  ziformula = ~1) # Intercept only.


## Fitting an interactive (mediated) ZI-binomial GLMM:
ttFS_zibin_glmm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                    scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                    clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year + (1|id_nestbox),
                                  weights = brood_size, data = ntits3,
                                  family = "binomial",
                                  ziformula = ~1)

## Fitting an interactive (mediated) ZI-binomial GLMM with OLRE:
ttFS_zibin_glmm2_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                    scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                    clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year + (1|id_obs) + (1|id_nestbox),
                                  weights = brood_size, data = ntits3,
                                  family = "binomial",
                                  ziformula = ~1)

## Fitting an interactive (mediated) ZI-beta-binomial GLMM:
ttFS_zibbin_glmm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                    scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                    clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year + (1|id_nestbox),
                                  weights = brood_size, data = ntits3,
                                  family = glmmTMB::betabinomial(link = "logit"),
                                  ziformula = ~1)
## Fitting an interactive (mediated) ZI-beta-binomial GLMM:
ttFS_zibbin_glm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                    scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                    clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year,
                                  weights = brood_size, data = ntits3,
                                  family = glmmTMB::betabinomial(link = "logit"),
                                  ziformula = ~1)
ntits3 %>% dplyr::mutate(c.clutch_size = clutch_size-stats::median(clutch_size)) -> ntits3
ttFS_zibbin_glm2_cent <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ c.log_patch_area * c.log_F_metric_d2b1 +
                                            c.clutch_size +
                                            urban_intensity + manag_low + manag_high +
                                            light_pollution + c.noise_m + c.traffic +
                                            c.cumdd_between + year,
                                  weights = brood_size, data = ntits3,
                                  family = glmmTMB::betabinomial(link = "logit"),
                                  ziformula = ~1)
summary(ttFS_bin_glm1) # AIC = 2105.7.
summary(ttFS_bbin_glm1) # AIC = NA (why?).
summary(ttFS_bin_glmm1) # AIC = 1680.1.
summary(ttFS_bin_glmm1_olre) # AIC = 1473.3.
summary(ttFS_bbin_glmm1) # AIC = 1474.9.
summary(ttFS_zibin_glmm1) # AIC = 1412.1.
summary(ttFS_zibin_glmm1_olre) # AIC = 1381.6.
summary(ttFS_zibbin_glmm1) # AIC = 1380.2.
summary(ttFS_zibbin_glmm1_cent) # AIC = 1380.2.
summary(ttFS_zibbin_glm1) # AIC = 1378.2 (estimates are similar to the GLMM = same models).
summary(ttFS_zibin_glmm2) # AIC = 1407.9 and significant interaction!
summary(ttFS_zibin_glmm2_olre) # AIC = 1379.5 and significant interaction!
summary(ttFS_zibbin_glmm2) # AIC = 1377.2 and significant interaction!
summary(ttFS_zibbin_glm2) # AIC = 1375.2 and significant interaction  (estimates are similar to the GLMM too)!
summary(ttFS_zibbin_glm2_cent)
# It seems that, if the inclusion of a random effect (RE) strongly improved the fit when the ZI and
# overdispersion were not accounted for, but not when they were. Accounting for the ZI and for overdispersion
# in the response, whether by using an OLRE or using a beta-binomial distribution, further improved the fit
# but it is not clear which one is best. Differences will have to be assessed through diagnoses. I will thus
# diagnose them all but, for the sake of brevity, I will not always show the code and comments for all.

# UPDATE: Diagnostics showed that using a beta-binomial distribution was the correct way to go and not using
# an OLRE. Since, as before, we explored a few reasonable variations of the same model by trying different
# proxies of the same variable or by tuning the ZI component of the model, we only try it for the beta-
# binomial specifications of the model, and mostly without RE to avoid convergence issues.
# Below, code and comments may be related to these exploratory models (models H), but you can re-run the
# diagnostics for the initial models by replacing the model name in the code chunks.





### ** 3.1.2. Exploratory modelling ----
# ______________________________________

## Using the "Rr metric":
ttFS_zibbin_glm1a <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + Rr_metric_d2c1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1) # Intercept only.

## Using "woodyveg_vw" or "woody_area":
ttFS_zibbin_glm1b <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_woody_vw + log_F_metric_d2b0 +
                                         clutch_size +
                                         urban_intensity + manag_intensity +
                                         light_pollution + noise_m + traffic +
                                         cumdd_between + year,
                                       weights = brood_size, data = ntits3,
                                       family = glmmTMB::betabinomial(link = "logit"),
                                       ziformula = ~1) # Intercept only.

## Using both:
ttFS_zibbin_glm1c <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_woody_area + log_F_metric_d2b0 +
                                         clutch_size +
                                         urban_intensity + manag_intensity +
                                         light_pollution + noise_m + traffic +
                                         cumdd_between + year,
                                       weights = brood_size, data = ntits3,
                                       family = glmmTMB::betabinomial(link = "logit"),
                                       ziformula = ~1) # Intercept only.

## Adding "min_t_between":
ttFS_zibbin_glm1d <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + min_t_between + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1) # Intercept only.
summary(ttFS_zibbin_glm1a) # AIC = 1381.2 vs 1378.2.
summary(ttFS_zibbin_glm1b) # AIC = 1379.2.
summary(ttFS_zibbin_glm1c) # AIC = 1378.5.
summary(ttFS_zibbin_glm1d) # AIC = 1379.5.
# These models do not improve things much.

## Tuning the ZI part of the model:
ttFS_zibbin_glm1e <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~min_t_between) # Intercept only.

ttFS_zibbin_glm1f <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~log_F_metric_d2b1) # Intercept only.

ttFS_zibbin_glm1g <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~log_patch_area) # Intercept only.

ttFS_zibbin_glm1h <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~min_t_between+urban_intensity+year) # Intercept only.
ttFS_zibbin_glmm1h <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + year + (1|site),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~min_t_between+urban_intensity+year) # Intercept only.

ttFS_zibbin_glm1i <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~min_t_between+log_F_metric_d2b1+year) # Intercept only.
summary(ttFS_zibbin_glm1e) # AIC = 1375.1 vs 1378.2.
summary(ttFS_zibbin_glm1f) # AIC = 1375.8.
summary(ttFS_zibbin_glm1g) # AIC = 1377.5.
summary(ttFS_zibbin_glm1h) # AIC = 1365.5.
summary(ttFS_zibbin_glmm1h) # AIC = 1367.4.
summary(ttFS_zibbin_glm1i) # AIC = 1369.3.
# I also tried removing "traffic" and it doesn't change things much. When we use the "F-metric" along with
# "urban_intensity" to model the ZI-part of the model, the effect of the "F-metric" utterly disappear
# suggesting that connectivity does not influence total fledging failures and its effect in "I models"
# is actually a surrogate effect from "urban_intensity". So I'll only diagnose the "H models".
# Similarly, the surprising effects of the temperature proxies might actually be linked to humidity, a
# variable we could not measure.

ttFS_zibbin_glm2h <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                       scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                       clutch_size +
                                       urban_intensity + manag_intensity +
                                       light_pollution + noise_m + traffic +
                                       cumdd_between + year,
                                     weights = brood_size, data = ntits3,
                                     family = glmmTMB::betabinomial(link = "logit"),
                                     ziformula = ~min_t_between+urban_intensity+year)
ttFS_zibbin_glmm2h <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                       scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                       clutch_size +
                                       urban_intensity + manag_intensity +
                                       light_pollution + noise_m + traffic +
                                       cumdd_between + year + (1|site),
                                     weights = brood_size, data = ntits3,
                                     family = glmmTMB::betabinomial(link = "logit"),
                                     ziformula = ~min_t_between+urban_intensity+year)

ttFS_zibbin_glm2i <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                       scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                       clutch_size +
                                       urban_intensity + manag_intensity +
                                       light_pollution + noise_m + traffic +
                                       cumdd_between + year,
                                     weights = brood_size, data = ntits3,
                                     family = glmmTMB::betabinomial(link = "logit"),
                                     ziformula = ~min_t_between+log_F_metric_d2b1+year)
summary(ttFS_zibbin_glm2h) # AIC = 1362.7 vs 1375.2.
summary(ttFS_zibbin_glmm2h) # AIC = 1364.7.
summary(ttFS_zibbin_glm2i) # AIC = 1366.4.





### ** 3.1.3. Diagnostics and assumption checks ----
# __________________________________________________

### *** 3.1.3.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(ttFS_zibbin_glmm1h, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Strange distribution of residuals with 2 groups.
# performance::check_outliers(ttFS_zibbin_glmm1) # Does not work for this type of model.
ntits3[which(resid < -0.4),] # Nestboxes with the lowest residuals = ~0% fledging success!

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = ntits3$noise_m, y = resid) # Seems rather ok although we once again find patterns linked to the
# sometimes odd distribution of some predictors. However, be reminded that simulated residuals will be
# more useful).
# plot(ttHSy_ziglmm1, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(ttHSy_ziglmm1, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttFS_zibbin_glmm1h, n = 1000, re.form = NULL)
simu.resid2 <- DHARMa::simulateResiduals(fittedModel = ttFS_zibbin_glmm2h, n = 1000, re.form = NULL)
simu.resido <- DHARMa::simulateResiduals(fittedModel = ttFS_zibin_glmm1_olre,
                                             n = 1000, re.form = NULL)
simu.resido2 <- DHARMa::simulateResiduals(fittedModel = ttFS_zibin_glmm2_olre,
                                             n = 1000, re.form = NULL) # The 're.form' argument is to base
# simulations on the model unconditional of the random effects (and only works for {lme4} formulations). It
# is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Ok.
plot(simu.resid2) # Ok.
DHARMa::outliers(simu.resid) # Ok.
DHARMa::outliers(simu.resido) # Ok.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid2,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Spatial auto-
# correlation detected for the OLRE models and for the exploratory models as well.
performance::check_autocorrelation(ttFS_zibbin_glmm2h) # Ok-ish.
performance::check_collinearity(ttFS_zibbin_glmm2h) # Ok for the beta-binomial models (highest VIF value for
# the first ZIBBIN = 3.56 for the F-metric while it was 4.02 for the second model). For the OLRE models,
# however, alarming multicollinearity issues have been detected with a moderate correlation linked to
# "clutch_size" being present, several VIF values > 4 and even 8.5 for the "F-metric" (even though things
# were a bit better for the interactive model).
# Interestingly, no multicollinearity was detected for the BBIN or the BIN_OLRE models either.
stats::vcov(ttFS_zibbin_glmm1h) # But values of the covariance matrix seem ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_perim)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_woody_area) # Deviation detected (w/o OLRE)!
DHARMa::plotResiduals(simu.resid, form = ntits3$log_woody_vw)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_F_metric_d2b1)
DHARMa::plotResiduals(simu.resid, form = ntits3$Rr_metric_d2c1)
DHARMa::plotResiduals(simu.resid, form = ntits3$species)
DHARMa::plotResiduals(simu.resid, form = ntits3$clutch_size)
DHARMa::plotResiduals(simu.resid, form = ntits3$urban_intensity) # Deviation detected (for model1)!
DHARMa::plotResiduals(simu.resid, form = ntits3$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_herb_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$sqrt_built_vol) # Deviation detected (for model1)!
DHARMa::plotResiduals(simu.resid, form = ntits3$light_pollution)
DHARMa::plotResiduals(simu.resid, form = ntits3$noise_m)
DHARMa::plotResiduals(simu.resid, form = ntits3$traffic)
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_30) # Deviation, but likely normal as this predictor
# should be correlated with "cumdd_between"!
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_between)
DHARMa::plotResiduals(simu.resid, form = ntits3$min_t_between)
DHARMa::plotResiduals(simu.resid, form = ntits3$year)
# For the ZIBBIN models ('ttFS_zibbin_glmm1' and 'ttFS_zibbin_glmm2'), no deviations were detected except
# for "cumdd_30" which is not included in the models. For the models with OLRE, worrying deviations have been
# found for "urban_intensity" (linked to the built volume) as well as for "cumdd_30", but only for the
# additive model ('ttFS_zibin_glmm1_olre').
# For the exploratory models, everything seems fine.



### *** 3.1.3.2. Distribution and dispersion ----
## Assessing over or under-dispersion:
DHARMa::testDispersion(simu.resid) # Ok.
performance::check_overdispersion(x = ttFS_zibbin_glmm1h) # Overdispersion detected! However, note that this test
# is known to be inaccurate for ZI-models (see the function's help page). If I run it on the non-ZI model
# (i.e. 'ttFS_bbin_glmm1'), the dispersion ratio is much lower, yet we may still need to account for that.
# ATTENTION: Classical overdispersion tests cannot be used to detect overdispersion when OLRE is used to
# account for it, and the same is likely true for beta-binomial models. So I'll ignore it now.

## Distribution of the predicted probabilities:
probabilities <- stats::predict(object = ttFS_zibbin_glmm1h, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities, main = "Predicted proportions", xlab = "Fledging success")
hist(ntits3$fledgling_nb/ntits3$brood_size, main = "Observed proportions", xlab = "Fledging rate")
par(.pardefault)
# The ZIBBIN models globally fail to correctly predict the data. Prediction ranges are too narrow, the mode is
# around 0.7 instead of 1 and models do not predict total successes or failures (even though the interaction
# model does a little better). The models with OLRE predict slightly better than the ZIBBIN (mode at 0.8) but
# it's still not very satisfactory.
# Interestingly, the overdispersed binomial GLMM without ZI ('ttFS_bin_glmm1') does a better job at predicting
# the observed data than any other model, and the binomial GLMM without ZI but with OLRE ('ttFS_bin_glmm1_olre')
# predicts the data very well (see below)!

## Zero-inflation:
DHARMa::testZeroInflation(simu.resid_nozi) # Significant for the non-ZI models!
# Testing with the non-ZI GLMM:
simu.resid_nozi <- DHARMa::simulateResiduals(fittedModel = ttFS_bin_glmm1_olre, n = 1000)
plot(simu.resid_nozi) # Slight or strong deviations detected for the non-ZI models (depending on the model)!
noziprobabilities <- stats::predict(object = ttFS_bin_glmm1_olre, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(noziprobabilities, main = "Predicted proportions (wo ZI)", xlab = "Fledging success")
hist(ntits3$fledgling_nb/ntits3$brood_size, main = "Observed proportions", xlab = "Fledging rate")
# Surprisingly, the model seems to better fit the observed proportions, even though it still under-predicts
# the proportion of failures.



### *** 3.1.3.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
ntits3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
                         log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
                         log_F_metric_d2b1,
                         Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
                         clutch_size,
                         urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
                         light_pollution, noise_m,
                         cumdd_between, min_t_between) -> mydata
predictors <- colnames(mydata)
probabilities <- stats::predict(object = ttFS_zibbin_glmm1, type = "response") # Extract probabilities!
# Bind the logit and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(logit = log(probabilities/(1-probabilities))) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -logit)
# Create scatterplot:
ggplot2::ggplot(mydata, ggplot2::aes(y = logit, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity is globally respected except for
# "min_t_between" which is U-shaped (but is currently not in the models).



### *** 3.1.3.4. Goodness-of-fit (GOF) and performances ----
## GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(ttFS_zibbin_glmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(ttFS_zibbin_glmm1)) # p = 0 (mistake?).

## Computing a pseudo-R2:
performance::r2_nakagawa(ttFS_zibbin_glmm1h, tolerance = 0.00000001)
# [Additive model]: Marg_R2_glmm = 0.8; Cond_R2_glmm = 0.8 (but may be unreliable due to difficulties to
# compute RE variances)!
performance::r2_nakagawa(ttFS_zibin_glmm1_olre, tolerance = 0.00000001)
# [Additive model]: Marg_R2_glmm = 0.17; Cond_R2_glmm = 0.17 (but may be unreliable due to difficulties to
# compute RE variances)!
performance::r2_nakagawa(ttFS_zibbin_glmm2h, tolerance = 0.00000001)
# [Interactive model]: Marg_R2_glmm = 0.81; Cond_R2_glmm = 0.81 (but may be unreliable due to difficulties to
# compute RE variances)!
performance::r2_nakagawa(ttFS_zibin_glmm2_olre, tolerance = 0.00000001)
# [Interactive model]: Marg_R2_glmm = 0.16; Cond_R2_glmm = 0.16 (but may be unreliable due to difficulties to
# compute RE variances)!

## Likelihood-based evaluation of effects inclusion:
zzz <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        cumdd_between + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1) # Intercept only.
zzz2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                            clutch_size +
                                            urban_intensity + manag_intensity +
                                            light_pollution + noise_m + traffic +
                                            cumdd_between + year + (1|id_obs),
                                          weights = brood_size, data = ntits3,
                                          family = "binomial",
                                          ziformula = ~1) # Intercept only.
summary(zzz) # Beta-binomial models: The non-mixed model gives AIC = 1378.2 while the mixed-model gave an AIC
# of 1380.2 (with "id_nestbox"), or 1380.2 (with "id_patch"), or 1380 (with "site"), or 1382 (with "site"
# and "id_nestbox") or 1382.2 (with "id_patch" and "id_nestbox")! It thus appears that the the use of
# RE is not useful here!
summary(zzz2) # OLRE models: The model without other RE than the OLRE gives AIC = 1379.6 while the RE-model
# gave an AIC of 1381.6 (with "id_nestbox"), or 1381.6 (with "id_patch"), or 1381.6 (with "site"), or 1383.6
# (with "site" and "id_nestbox") or 1383.6 (with "id_patch" and "id_nestbox")! It thus appears that the the use
# of additional RE is not useful here!

# Importance of the fixed effects:
ttFS_zibbin_glmm0 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ 1 + (1|id_nestbox),
                                 weights = brood_size, data = ntits3,
                                 family = glmmTMB::betabinomial(link = "logit"),
                                 ziformula = ~1)
ttFS_zibin_glmm0_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ 1 + (1|id_nestbox) + (1|id_obs),
                                      weights = brood_size, data = ntits3,
                                      family = "binomial",
                                      ziformula = ~1)
summary(ttFS_zibbin_glmm0) # AIC = 1462.9 vs 1380.2, so the full model is clearly far better!
summary(ttFS_zibin_glmm0_olre) # AIC = 1473.5 vs 1381.6, so the full model is clearly far better!





### ** 3.1.4. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ -----

### *** 3.1.4.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
ttFS_zibbin_glm0 <- stats::update(ttFS_zibbin_glm1, .~. -log_F_metric_d2b1) # The GLMM fails to converge!
summary(ttFS_zibbin_glm0) # AIC = 1381.3 vs 1378.2 (hypothesis 1 likely validated)!
# I do not run PB-based LRT for now as they take too long to run.

# res.LRT_addeff <- pbkrtest::PBmodcomp(ttFS_ziglmm1,
#                                       ttFS_ziglmm0, nsim = 500, seed = 56) # Took ~??? to run!
# readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
#                                                              "res.ntitsFSy_LRT_addeff.csv"))
# # The LRT is not significant, indicating that our connectivity metric does not improve the description of
# # the data here.

## For the interaction effect:
# Since even the additive model is NOT SIGNIFICANT, there is no point in testing the effect of the
# interactive (mediated) model.



### *** 3.1.4.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive GLMM parameters")
res.ntitsFSy_addeff_CI_boot <- confint(ntitsFSy_ziglmm1, method="boot")
tt <- as.data.frame(res.ntitsFSy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ntitsFSy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~1h45 to run!



### *** 3.1.4.3. Conclusion ----
# For the initial model:
summary(ttFS_zibbin_glmm1) # AIC = 1380.2 and both R2_glmm = 0.8!
summary(ttFS_zibbin_glm1) # AIC = 1378.2 and NA for R2 (performance thinks it's a mixed model) but as the model
# is basically similar to the GLMM, we can consider its R2 as well.
summary(ttFS_zibbin_glmm2) # AIC = 1377.2 and both R2_glmm = 0.81!
# Diagnostics ran for these initial models indicated that the model fit the data relatively well although
# prediction ranges are too narrow and the models fail to predict total failures and successes! Interestingly,
# and inexplicably, some of the models without ZI (especially the binomial GLMM with OLRE) predicted the
# observed data far better but yielded worrying diagnostics and had lower AIC values than the retained models.
# Remember that the retained models use a beta-binomial family as the response displayed overdispersion.
# Models that did not account for this overdispersion also raised other red flags. As recommended by
# Harrison (2014), we compared the beta-binomial models with models including OLRE but the latter models also
# displayed some problematic issues such as concerning multicollinearity, autocorrelation and quantile
# deviations. Diagnostics from the beta-binomial models were all satisfactory if we excluded "species" from
# the models as it is collinear with "clutch_size". We thus chose to keep "clutch_size" as it is a more
# interesting predictor, yielded better diagnostics and results, and as no "species" effect was detected.
# However, the null model 'ttFS_zibbin_glmm1' failed to converge so I consider using the non-mixed models
# ZIBBIN_GLM instead as the RE turned out to be unnecessary.
## Significant variables: F-metric (++), clutch_size (-), manag_high (--), cumdd_between (--), year2020 (++),
# and year2022 (+++).
## For the inteactive (moderated) model, the INTERACTION TERM was also significant (--), but the F-metric and
# cumdd_between had p-values < 0.1 (trends), which could be a sign of a cross-over interaction.
## Hypothesis 1 and 2 possibly validated (AIC = 1381.3 vs 1378.2)!

### Manual prediction attempt:
summary(ttFS_zibbin_glm2)

intercept <- 3.808979
coef_logPA <- -0.066579
coef_logF <- 0.369460
coef_interact <- -0.367882

eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)
# Y = B0 + B1*X1 + B2*X2 + B3*X1*X2

## When both are fixed and rather low:
# For a log_patch_area and a log_F of both -2:
logPA <- -2
logF <- -2
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.8496283 (that is the BASELINE)!

## When only F increases:
# For a log_patch_area of -2 and a log_F of -1.5:
logF <- -1.5
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.9075685
# For a log_patch_area of -2 and a log_F of -1:
logF <- -1
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.9446382
# For a log_patch_area of -2 and a log_F of -0.5:
logF <- -0.5
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.9673756
# Ok, for a fixed PATCH_AREA, an increase of F increases fledging success!

## When patch_area increases:
# For a log_patch_area of -1.5 and a log_F of -2:
logPA <- -1.5
logF <- -2
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.8875825
# For a log_patch_area of -1 and a log_F of -2:
logPA <- -1
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.9168939
# For a log_patch_area of -0.5 and a log_F of -2:
logPA <- -0.5
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.9390873
# PROBLEM: for a fixed F, an increase of PATCH_AREA also increases fledging success!

## When both increase:
# For a log_patch_area and a log_F of both -1.5:
logPA = logF = -1.5
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.9260078
# For a log_patch_area and a log_F of both -1:
logPA = logF = -1
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.9584419
# For a log_patch_area and a log_F of both -0.5:
logPA = logF = -0.5
eq <- intercept + (coef_logPA * logPA) + (coef_logF * logF) + (coef_interact * logPA * logF)

exp(eq)/(1+exp(eq)) # prop = 0.9724969
# Same here.
# Donc, soit je me suis planté dans mes calculs, soit c'est toute mon interprétation des coefs bruts qui
# est fausse. Donc, 3D plot + predictions (avec R)!


### Attempt at 3D plot:

ttFS_zibbin_glm2_cent
summary(ntits3$brood_size)

x_tilde <- expand.grid(c.log_patch_area = seq(-2.2,1.1, length.out=10),
                       c.log_F_metric_d2b1 = seq(-1.8,1.1, length.out=10),
                       c.clutch_size = 0,
                       urban_intensity = 0,
                       manag_low = 0,
                       manag_high = 0,
                       light_pollution = 0,
                       c.noise_m = 0,
                       c.traffic = 0,
                       c.cumdd_between = 0,
                       year = "2021",
                       brood_size = 8)
x_tilde$fledging_rate <- predict(ttFS_zibbin_glm2_cent, x_tilde, type="response")


lattice::trellis.par.set("axis.line", list(col=NA,lty=1,lwd=1))
lattice::wireframe(fledging_rate ~ c.log_patch_area + c.log_F_metric_d2b1, data=x_tilde,
          xlab = "c.log_patch_area",
          ylab = "c.log_F_metric_d2b1",
          main = "Fledging success predictions",
          drape = TRUE,
          colorkey = TRUE,
          scales = list(arrows=FALSE,cex=1, tick.number = 10, z = list(arrows=F), distance = c(1.5, 1.5, 1.5)),
          light.source = c(10,0,10),
          col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = .8),
          screen = list(z = -60, x = -60))





# For the exploratory models:
summary(ttFS_zibbin_glmm1h) # AIC = 1367.4 and both R2_glmm = 0.8.
## Significant variables: F-metric (++), clutch_size (-), manag_high (--), cumdd_between (--), year2020 (++),
# and year2022 (+++); and also [urban_intensity (+), year2021 and year2022 (---) for the ZI component]!
## Almost significant variables: [min_t_between (+) for the ZI component]!
## Hypothesis 1 possibly validated (AIC = 1369.9 vs 1367.4 for the GLMM version (sith "site" as RE), and
# AIC = 1368.6 vs 1365.5 for the GLM version)!
summary(ttFS_zibbin_glmm2h) # AIC = 1364.7 and both R2_glmm = 0.81.
## Significant variables: clutch_size (-), manag_high (--), year2020 and 2022 (+++), and the INTERACTION
# EFFECT (--); and also [urban_intensity (+), year2021 and year2022 (---) for the ZI component]!
## Almost significant variables: the F-metric (+), light_pollution (-), cumdd_between (-) and also
# [min_t_between (+) for the ZI component]!
## Hypothesis 2 likely validated with a possible cross-over interaction!

# Diagnostics for these models were mostly ok, there was no outliers, deviations, dispersion or
# distributional problems, even though there was some residual autocorrelation.
# Predictions are fairly ok but the models still make too narrow predictions and fail to predict correctly
# the total success and failures.

# I also tried removing "traffic" and it doesn't change things much. When we use the "F-metric" along with
# "urban_intensity" to model the ZI-part of the model, the effect of the "F-metric" utterly disappear
# suggesting that connectivity does not influence total fledging failures and its effect in "I models"
# is actually a surrogate effect from "urban_intensity".
# Similarly, the surprising effects of the temperature proxies might actually be linked to humidity, a
# variable we could not measure.





########################## ************************************************* ###############################

# ----------------------------------------------- #
##### 4. Modelling the morphometric variables #####
# ----------------------------------------------- #

##### * 4.1 Mass: LMM ---------------------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 4.1.1. Initial model fit ----
# __________________________________

ntits2 %>% dplyr::filter(is.na(mass) == FALSE) -> ntits3 # Only 317 observations left.

## Fitting a regular linear model:
ttMA_lm1 <- stats::lm(mass ~ log_patch_area + log_F_metric_d2b1 + species + clutch_size +
                         urban_intensity + manag_intensity +
                         light_pollution + noise_m + traffic +
                         cumdd_between + year,
                       data = ntits3)

## Fitting an additive LMM:
ttMA_lmm1 <- glmmTMB::glmmTMB(mass ~ log_patch_area + log_F_metric_d2b1 + species + clutch_size +
                                 urban_intensity + manag_intensity +
                                 light_pollution + noise_m + traffic +
                                 cumdd_between + year + (1|id_nestbox),
                                data = ntits3, family = "gaussian")
ttMA_lmm1b <- lme4::lmer(mass ~ log_patch_area + log_F_metric_d2b1 + species + clutch_size +
                            urban_intensity + manag_intensity +
                            light_pollution + noise_m + traffic +
                            cumdd_between + year + (1|id_nestbox), data = ntits3,
                            control=lme4::lmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5))) # Same model but fitted
# with {lme4} for comparison sake! Note however, that it is fitted by REML and, if not, it is singular!

## Fitting interactive (mediated) LMMs:
ttMA_lmm2 <- glmmTMB::glmmTMB(mass ~
                           scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                             species + clutch_size +
                             urban_intensity + manag_intensity +
                             light_pollution + noise_m + traffic +
                             cumdd_between + year + (1|id_nestbox),
                           data = ntits3, family = "gaussian")
ttMA_lmm2b <- lme4::lmer(mass ~
                            scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                           species + clutch_size +
                           urban_intensity + manag_intensity +
                           light_pollution + noise_m + traffic +
                           cumdd_between + year + (1|id_nestbox), data = ntits3,
                          control=lme4::lmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun=2e5)))
AIC(ttMA_lm1) # AIC = 1159.2.
summary(ttMA_lmm1) # AIC = 1161.2.
summary(ttMA_lmm1b) # REML = 1160.4 (not a true AIC).
summary(ttMA_lmm2) # AIC = 1162.8, does not seem supported by the data!
summary(ttMA_lmm2b) # AIC = 1161.4, does not seem supported by the data!
# Note that for both LMM, RE variance is extremely low (nearly singular fit) which, with the AIC, suggests
# that the use of a mixed-model is not warranted by the data.
# OR hatching_rate/BS????????
# OR hatching_rate/BS????????
# OR hatching_rate/BS????????
# OR hatching_rate/BS????????
# OR hatching_rate/BS????????
# + remove outliers??????????????






### ** 4.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 4.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Extracting residuals (with the {redres}):
raw_cond <- redres::compute_redres(ttMA_lmm1b) # Computes the raw conditional residuals (conditional on
# the random effects (RE)).
pearson_mar <- redres::compute_redres(ttMA_lmm1b, type = "pearson_mar") # Computes the Pearson marginal
# (not accounting for the RE) residuals.
std_cond <- redres::compute_redres(ttMA_lmm1b, type = "std_cond") # Computes the studentised cond. ones.
# Joins the residuals to the data:
xxx <- cbind(ntits3, raw_cond, pearson_mar, std_cond)

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttMA_lmm1b, n = 1000, plot = FALSE)
plot(simu.resid) # Some outliers detected!
DHARMa::outliers(simu.resid) # Individuals 173 (CC), 192, 248 (PM).

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(ttMA_lmm1b) # Ok.
performance::check_collinearity(ttMA_lmm1b) # Ok-ish but some VIF > 4-5 and moderate correlation for "species"
# in the interaction model.
stats::vcov(ttMA_lmm1b) # Ok.



### *** 4.1.2.2. Distribution and homoscedasticity ----
## Assessing the normality of the residuals:
stats::shapiro.test(xxx$raw_cond) # Ok. But plotting would be better:
redres::plot_resqq(ttMA_lmm1b) # As expected, the plot shows a substantial departure from Normality at the
# extreme ends of the quantiles, that is at the border of the parameters space. Overall, as almost all
# points stay within the 95% CI, we can say it is ok-ish.
# Plotting them against each numeric predictor:
xxx %>%
  tidyr::gather(key = "type", value = "residual",
                c(log_patch_area, log_F_metric_d2b1, clutch_size, urban_intensity,
                  light_pollution, noise_m, traffic, cumdd_between)) %>%
  ggplot2::ggplot(ggplot2::aes(x = residual)) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::facet_grid(. ~ type, scales = "free") +
  ggplot2::theme_bw() # Residuals vs predictor plots look rather ok, even though some strange patterns can
# be seen. I would not be too worried about them but they would require some further thinking.

## Assessing the normality in the random effect:
redres::plot_ranef(ttMA_lmm1b) # Ok-ish, but quite a strong departure from normality for the last residual.

## Assessing homogeneity of variance and influential observations:
plot(ttMA_lmm1b, type=c("p","smooth"), col.line = 2, id = 0.05, idLabels = ~.obs,
     ylab = "Pearson's residuals", xlab = "Fitted values") # It's ok but there are a few possible outliers:
ntits3[c(248,11,213,32,177,102,104,155),] # High residuals ~= heavy juveniles. Ok, but it's true that the
# brood number 248 is heavy for CC, perhaps a misidentification?
ntits3[c(26,42,198,170,264,236,173,192),] # Low residuals ~= light juveniles. RAS.

# Residuals vs leverage:
plot(ttMA_lmm1b, stats::rstudent(.) ~ stats::hatvalues(.))
cd <- stats::cooks.distance(ttMA_lmm1b)
plot(cd, ylab = "Cook's distance")
ntits3[which(cd>0.4),] # Ok, all observations are < 0.5, so no overly influential points.
ntits3[which(cd>0.1),] # Even with very conservative values, it's ok!

## Residuals vs predictors:
redres::plot_redres(ttMA_lmm1b, xvar = "cumdd_between") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Residual vs predictor") # Ok-ish.
# plot(ntits3$log_F_metric_d2b1, stats::residuals(ttMA_lmm1b)) # Same plot (I should create a
# custom function).
redres::plot_redres(ttMA_lmm1b, type = "raw_mar", xvar = "year") # Ok.

## Distribution of the predicted values:
par(.pardefault)
predictions <- stats::predict(object = ttMA_lmm1b, type = "response") # Extract the predicted values.
par(mfrow= c(1,2))
hist(predictions, main = "Predicted mass", xlab = "Nestling mass (g)")
plot(ecdf(predictions), main = "Predicted CDF", xlab = "Nestling mass (g)")
fitdistrplus::plotdist(data = ntits3$mass, histo = TRUE, demp = TRUE) # Rather ok.



### *** 4.1.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
ntits3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
                         log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
                         log_F_metric_d2b1,
                         Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
                         clutch_size, brood_size,
                         urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
                         light_pollution, noise_m,
                         cumdd_between, min_t_between) -> mydata
predictors <- colnames(mydata)
# Bind 'mass' and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(mass = ntits3$mass) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -mass)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = mass, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Ok.



### *** 4.1.2.4. Goodness-of-fit (GOF) and performances ----
## Computing a pseudo-R2:
performance::r2_nakagawa(ttMA_lmm1b) # [Additive model]: Marg_R2_lmm = 0.79; Cond_R2_lmm = 0.79.
performance::r2_nakagawa(ttMA_lmm2b) # [Interact. model]: Marg_R2_lmm = 0.79; Cond_R2_lmm = 0.79.

## Likelihood-based evaluation of effects inclusion:
# Importance of the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT for RE inclusion")
res.LRT_re <- DHARMa::simulateLRT(m0 = ttMA_lm1, m1 = ttMA_lmm1b, n = 1000, seed = 682)
tictoc::toc() # Took ~24s to run.
# The LRT is highly significant, suggesting that M1 better describes the data than M0, supporting the
# importance of the random effect!

# Importance of the fixed effects (only using the LM):
ttMA_lm0 <- stats::lm(mass ~ 1, data = ntits3)
res.LRT_null <- stats::anova(object = ttMA_lm0, ttMA_lm1, test = "LRT")
# The test is highly significant, confirming that the model is useful to explain the data.





### ** 4.1.3. Inference and predictions ----
# __________________________________________

### *** 4.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
ttMA_lmm0b <- stats::update(ttMA_lmm1b, .~. -log_F_metric_d2b1)

res.LRT_addeff <- pbkrtest::PBmodcomp(ttMA_lmm1b,
                                      ttMA_lmm0b, nsim = 1000, seed = 854) # Took ~41s to run!
readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
                                                             "res.ttMA_LRT_addeff.csv"))
# The LRT is significant, indicating that our connectivity metric does improve the description of the data.


## For the interaction effect:
res.LRT_inteff <- pbkrtest::PBmodcomp(ttMA_lmm2b,
                                      ttMA_lmm1b, nsim = 1000, seed = 118) # Took ~46s to run!
readr::write_csv2(x = res.LRT_inteff$test, file = here::here("output", "tables",
                                                             "res.ttMA_LRT_inteff.csv"))
# The LRT is NOT significant, indicating that our hypothesis of an interaction effect is not supported
# by the data. Note however that there was a lot of extreme samples.



### *** 4.1.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive LMM parameters")
res.ttMA_addeff_CI_boot <- confint(ttMA_lmm1b, method="boot")
tt <- as.data.frame(res.ttMA_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttMA_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~11s to run!



### *** 4.1.3.3. Conclusion ----
# For the initial model:
summary(ttMA_lmm1b) # AIC = 1160.4 and both R2_lmm = 0.79.
summary(ttMA_lmm2b) # AIC = 1161.4 and both R2_lmm = 0.79.
# Diagnostics ran for 'ttMA_lmm1b' and 'ttMA_lmm2b' (initial models) indicated that the models do fit the
# data relatively well and that there are no major cause for concern:
# - All sorts of residuals are globally fine. Even though some patterns exist that are linked to the
#   imperfect sampling of the predictor space. There's not much we could do about it except, perhaps, try
#   some more advanced variable transformations.
# - Assumptions all seem validated, although there is a mild multicollinearity with some VIF > 4-5 and
#   moderate correlation for "species".
# - No overly influential observations although some potential outliers may exist: individuals 173 (CC),
#   and 192 and 248 (PM). Number 173 might be a misidentified brood.
## Significant variables: F-metric (++), speciesCC (---), clutch_size (-), year2020 and 2022 (+).
## Almost significant variables: light_pollution (-) and manag_high (-).
## Hypothesis 1 is validated (PB-based LRT; p = 0.03) but hypothesis 2 is rejected (p = 0.55)!

# I did not run exploratory models yet, but improvements are likely possible.





########################## ************************************************* ###############################
par(.pardefault)
############################################ MAJJJJJJJ§§§§§§§§ todolist ####################################
# - Tester avec ou sans outliers (noise_m, connectivity???); si ça change beaucoup les résultats, rapporter
#   les deux??? Tester aussi patch_perim + traffic + choisir RE définitifs??? (e.g. id_patch + site???)
#   + tester id_nestbox (3 REs????)???
#   + model laying date???????????????????????????????????????????????????????????????????????
#   + model laying date???????????????????????????????????????????????????????????????????????
#   + model laying date???????????????????????????????????????????????????????????????????????
#   + model laying date???????????????????????????????????????????????????????????????????????
#   + model laying date???????????????????????????????????????????????????????????????????????
#   + model laying date???????????????????????????????????????????????????????????????????????
#   + model laying date???????????????????????????????????????????????????????????????????????
#   + model laying date???????????????????????????????????????????????????????????????????????
# - Refaire tourner tous les modèles avec NTITS --> Utiliser clutch_size et brood_size comme prédicteur, ou
#   la date de ponte??????? ASK JC??????????? Tester????
# +++++ Voir pk EDA_report bug! + Voir si centrage change quelque chose (interprétation interaction)?????
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
