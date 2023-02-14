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

## To remove probable outliers (see initial 'ttCy_comglmm1' diagnostics):
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
                                 cumdd_30 + year + (1|id_patch),
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
                                    cumdd_30 + year + (1|id_patch),
                                 data = ntits3, family = glmmTMB::compois(link = "log"),
                                 dispformula = ~1) # Rather long to fit.
## Fitting the interaction model (COM-Poisson GLMM):
ttCy_comglmm2 <- glmmTMB::glmmTMB(clutch_size ~
                                    scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                    species +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_30 + year + (1|id_patch),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1) # Rather long to fit.
summary(ttCy_glm1) # AIC = 1711.2 (vs 1811.4 with the outliers).
summary(ttCy_glmm1) # AIC = 1713.2 (vs 1813.4 with the outliers).
summary(ttCy_comglm1) # AIC = 1408.6 (vs 1669.5 with the outliers).
summary(ttCy_comglm1b) # AIC = 1411.8 (so it's not exactly the same?! REML?).
summary(ttCy_comglmm1) # AIC = 1408.3 (vs 1626.7 with the outliers).
summary(ttCy_comglmm2) # AIC = 1410.1, so the interaction worsen the fit (vs 1673.5 with the outliers)! Using
# the "Rr_metric_d2c1" proxy instead of the F one is even worse!
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

# Below, code and comments will show the diagnostics of one or several of these improved models, but you
# can re-run the diagnostics for the initial model by replacing the model name in the code chunks and to
# change ntits3 by ntits2 (i.e. the dataset with the deleted outliers).





### ** 1.1.2. Improved model (exploration) ----
# _____________________________________________

##### A FINIR ET NETTOYER§§§ ----
##### A FINIR ET NETTOYER§§§ ----
##### A FINIR ET NETTOYER§§§ ----

## Adding "site" as RE:
ttCy_comglmm1b <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch) + (1|site),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
## Only using "site" as RE:
ttCy_comglmm1c <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|site),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
## Tuning the NU parameter (dispersion model):
ttCy_comglmm1d <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_patch_area+log_F_metric_d2b1+cumdd_30+min_t_before)
ttCy_comglmm1e <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_F_metric_d2b1+cumdd_30+min_t_before)
ttCy_comglmm1f <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~log_F_metric_d2b1+cumdd_30)
ttCy_comglmm1g <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~Rr_metric_d2c1)
ttCy_comglmm1h <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~cumdd_30+Rr_metric_d2c1)
ttCy_comglmm1i <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                     urban_intensity + manag_intensity +
                                     light_pollution + noise_m + traffic +
                                     cumdd_30 + year + (1|id_patch),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~cumdd_30+log_herb_area)
summary(ttCy_comglmm1b) # AIC = 1408.8 vs 1408.3.
summary(ttCy_comglmm1c) # AIC = 1406.9 (while no RE at all gives 1408.6)!
summary(ttCy_comglmm1d) # AIC = 1413.
summary(ttCy_comglmm1e) # AIC = 1411.
summary(ttCy_comglmm1f) # AIC = 1409.2.
summary(ttCy_comglmm1g) # AIC = 1408.3 with only "cumdd_30"; 1409.3 with only "Rr_metric_d2c1";
summary(ttCy_comglmm1h) # AIC = 1409.1 with "cumdd_30"+"species"; 1409.5 same but "Rr_metric_d2c1";
summary(ttCy_comglmm1i) # AIC = 1409.4 (with "urban_intensity"); 1410.2 with "min_t_before"; 1414 with "year";
# 1409.6 with "log_herb_area"!

# Only adding "patch_perim":
zzz <- glmmTMB::glmmTMB(clutch_size ~ log_F_metric_d2b1 + species +
                          urban_intensity + manag_intensity +
                          light_pollution + noise_m + traffic +
                          cumdd_30 + year + (1|id_patch),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1)
summary(zzz) # AIC = 1410.2 with "patch_perim"; 1409.7 with "patch_area/perim"!
# And removing "patch_area" worsen AIC (=1412.7) suggesting that it is indeed useful to model CS!





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

## Likelihood-ration tests (LRT) of GOF:
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


# For the exploratory improved models:
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
                                  cumdd_30 + year + (1|id_patch),
                           weights = clutch_size, data = ntits3, family = "binomial")

## Fitting an interactive binomial GLMM:
ttHSy_glmm2 <- glmmTMB::glmmTMB(brood_size/clutch_size ~
                                  scale(log_patch_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                  species +
                                  urban_intensity + manag_intensity +
                                  light_pollution + noise_m + traffic +
                                  cumdd_30 + year + (1|id_patch),
                                weights = clutch_size, data = ntits3, family = "binomial")
summary(ttHSy_glm1) # AIC = 775.7.
summary(ttHSy_glmm1) # AIC = 739.5.
summary(ttHSy_glmm2) # AIC = 741, so the interaction does not seem supported by the data!
# It seems that, if the inclusion of a random effect (RE) improves the fit! I will thus carry on with the
# last model to the diagnostic part and assess whether the use of the RE is truly justified or not and if
# the model behaves as expected.


##### A FINIR ET NETTOYER§§§ ----
##### A FINIR ET NETTOYER§§§ ----
##### A FINIR ET NETTOYER§§§ ----
# UPDATE: diagnostics ran for 'ttHSy_ziglmm1' and 'ttHSy_glmm1' (the initial models) indicated that the
# models fit the data relatively well although some modelling assumptions were not fully met and the models
# under-predicted. Overall, diagnostics suggested that observations with brood sizes of 0 were likely
# outliers generated by another process than the one driving hatching rate. Incidentally, they were mostly
# the same as the outliers of clutch_size!
# Consequently, in a second step, we removed them and explored a few reasonable variations of the same
# model by trying different proxies of the same variable (see below).
# To test our 2nd hypothesis, I also fitted an interaction model in which the interaction effect turned
# out significant, it will thus be diagnose as well.

# Below, code and comments will show the diagnostics of several of these improved models (D, E, F), but you
# can re-run the diagnostics for the initial models by replacing the model name in the code chunks and to
# change ntits3 by ntits2 (i.e. the dataset with the deleted outliers).





### ** 2.1.2. Improved model (exploration) ----
# _____________________________________________

# Test: other RE (and combinations)! Other proxies!

## Using "site" as RE:
ttHSy_glmm1a <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + year + (1|site),
                                 weights = clutch_size, data = ntits3, family = "binomial")
model0 <- lme4::glmer(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                        urban_intensity + manag_intensity +
                        light_pollution + noise_m + traffic +
                        cumdd_30 + year + (1|site),
                      weights = clutch_size, data = ntits3, family = "binomial")

## Using "id_nestbox" as RE:
ttHSy_glmm1b <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + year + (1|id_nestbox) + (1|site),
                                 weights = clutch_size, data = ntits3, family = "binomial")
model1 <- lme4::glmer(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                        urban_intensity + manag_intensity +
                        light_pollution + noise_m + traffic +
                        cumdd_30 + year + (1|id_nestbox) + (1|site),
                      weights = clutch_size, data = ntits3, family = "binomial")
summary(ttHSy_glmm1a) # AIC = 773.5 vs 739.5 (but "patch_area" is significant (+))!
summary(model0) # AIC = 773.5 vs 739.5 (but "patch_area" is significant (+))!
summary(ttHSy_glmm1b) # AIC = 734 (but PATCH_AREA not significant, even with SITE)!
summary(model1) # AIC = 734 (but PATCH_AREA not significant, even with SITE)!
### SITE va plutôt dans le sens qu'on veut, mais ça nique l'AIC! Pk?????? Incompréhensible tout ça. Try PB-based
# LRT pour voir si ID_NESTBOX améliore vraiment le modèle:
tictoc::tic("Parametric bootstrap LRT for RE inclusion")
res.LRT_inteff <- DHARMa::simulateLRT(m0 = model0, m1 = model1, n = 500, seed = 65)
tt <- as.data.frame(cbind(res.LRT_inteff$method,
                          res.LRT_inteff$data.name,
                          res.LRT_inteff$statistic,
                          res.LRT_inteff$p.value))
rownames(tt) <- NULL
tt %>% dplyr::rename("Method" = V1,
                     "Models" = V2,
                     "Log Likelihood (M1/M0)" = V3,
                     "p-value" = V4) -> tt
readr::write_csv2(x = tt, file = here::here("output", "tables", "res.ttHSy_LRT_RE.site_nest.csv"))
tictoc::toc() # DISCLAIMER: took ~??? to run!



ttHSy_glmm1c <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m +
                                   cumdd_30 + year + (1|site),
                                 weights = clutch_size, data = ntits3, family = "binomial")
ttHSy_glmm1d <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + log_herb_area + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + year + (1|site),
                                 weights = clutch_size, data = ntits3, family = "binomial")
ttHSy_glmm1e <- glmmTMB::glmmTMB(brood_size/clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_60 + year + (1|site),
                                 weights = clutch_size, data = ntits3, family = "binomial")
summary(ttHSy_glmm1c) # AIC = 727.8 avec WOODY_AREA (et "id_nestbox" en RE); 730.4 idem (mais "id_patch") -->
# mais plus de significativité (mais --- pour WOODY_AREA)!
summary(ttHSy_glmm1d) # AIC = 734.1.
summary(ttHSy_glmm1e) # AIC = 732.5.


# Donc:
# - Supprimer TRAFFIC ne change rien.
# - Utiliser d'autres métriques de connectivité ne change pas grand chose. La connectivité ne répond que si on
#   utilise WOODY_AREA (ou quelque chose de proche), impliquant qu'en effet, c'était peut-être une mauvaise
#   idée à la base (car effet négatif --> la connectivité représentait en bonne partie la quantité d'habitat)?
# - Utiliser SITE affaiblit l'AIC mais rend significatif l'effet de PATCH_AREA! Encore plus si REML=TRUE!
# - Utiliser WOODY_AREA baisse l'AIC et donne des trucs significatifs mais durs à expliquer (WOODY---).
# - Quand on utilise WOODY_AREA, il semble vraiment falloir associer SITE et l'un des ID. Cela suggère donc un
#   fort effet de site affectant WOODY_AREA.









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

## Likelihood-ration tests (LRT) of GOF:
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


# For the exploratory improved models:
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
summary(ttFS_bin_glm1) # AIC = 2105.7.
summary(ttFS_bbin_glm1) # AIC = NA (why?).
summary(ttFS_bin_glmm1) # AIC = 1680.1.
summary(ttFS_bin_glmm1_olre) # AIC = 1473.3.
summary(ttFS_bbin_glmm1) # AIC = 1474.9.
summary(ttFS_zibin_glmm1) # AIC = 1412.1.
summary(ttFS_zibin_glmm1_olre) # AIC = 1381.6.
summary(ttFS_zibbin_glmm1) # AIC = 1380.2.
summary(ttFS_zibin_glmm2) # AIC = 1407.9 and significant interaction!
summary(ttFS_zibin_glmm2_olre) # AIC = 1379.5 and significant interaction!
summary(ttFS_zibbin_glmm2) # AIC = 1377.2 and significant interaction!
# It seems that, if the inclusion of a random effect (RE) strongly improved the fit and so does accounting
# for the zero-inflation (ZI)! Accounting for the overdispersion in the response, whether by using
# an OLRE or using a beta-binomial distribution, further improved the fit but it is not clear which one is
# best. Differences will have to be assessed through diagnoses. I will thus diagnose them all but, for the
# sake of brevity, I will not show the code and comments for all.


# AFINIR§§§
# AFINIR§§§
# AFINIR§§§
# AFINIR§§§
# UPDATE: As before, we explored a few reasonable variations of the same model by trying different proxies
# of the same variable (see below).
# Below, code and comments will show the diagnostics of several of these improved models (D and F), but you
# can re-run the diagnostics for the initial models by replacing the model name in the code chunks
# (i.e. the dataset with the deleted outliers).





### ** 3.1.2. Improved model (exploration) ----
# _____________________________________________

## Using the beta1 "F-metric":
ttFS_ziglmm1a <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ logged_woody_area + log_F_metric_d2b1_d2b1 +
                                     clutch_size +
                                     urban_intensity + manag_intensity + light_pollution + noise_iq +
                                     cumdd_30 + year + (1|id_obs) + (1|id_nestbox),
                                  weights = brood_size, data = ntits3, family = "binomial",
                                  ziformula = ~1) # Intercept only.
# NOTE: convergence issues!

## Using "woodyveg_vw":
ttFS_ziglmm1b <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                     clutch_size +
                                     urban_intensity + manag_intensity + light_pollution + noise_iq +
                                     cumdd_30 + year + (1|id_obs) + (1|id_nestbox),
                                   weights = brood_size, data = ntits3, family = "binomial",
                                   ziformula = ~1) # Intercept only.

## Using both:
ttFS_ziglmm1c <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1_d2b1 +
                                     clutch_size +
                                     urban_intensity + manag_intensity + light_pollution + noise_iq +
                                     cumdd_30 + year + (1|id_obs) + (1|id_nestbox),
                                   weights = brood_size, data = ntits3, family = "binomial",
                                   ziformula = ~1) # Intercept only.
# These models do not improve anything.
## Tuning the ZI part of the model:
ttFS_ziglmm1d <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ logged_woody_area + log_F_metric_d2b1 +
                                    clutch_size +
                                    urban_intensity + manag_intensity + light_pollution + noise_iq +
                                    cumdd_30 + year + (1|id_obs) + (1|id_nestbox),
                                  weights = brood_size, data = ntits3, family = "binomial",
                                  ziformula = ~min_t_between + log_F_metric_d2b1)

## With the beta1 "F-metric":
ttFS_ziglmm1e <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ logged_woody_area + log_F_metric_d2b1_d2b1 +
                                    clutch_size +
                                    urban_intensity + manag_intensity + light_pollution + noise_iq +
                                    cumdd_30 + year + (1|id_obs) + (1|id_nestbox),
                                  weights = brood_size, data = ntits3, family = "binomial",
                                  ziformula = ~min_t_between + log_F_metric_d2b1_d2b1)
summary(ttFS_ziglmm1a) # AIC = 1331.9 vs 1331.8.
summary(ttFS_ziglmm1b) # AIC = 1333.8.
summary(ttFS_ziglmm1c) # AIC = 1334.4.
summary(ttFS_ziglmm1d) # AIC = 1327.3
summary(ttFS_ziglmm1e) # AIC = 1326.1.
performance::check_collinearity(ttFS_ziglmm1e)
colnames(ntits3)

ttFS_ziglmm1f <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                    scale(logged_woody_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                                    clutch_size +
                                     urban_intensity + manag_intensity + light_pollution + noise_iq +
                                    cumdd_30 + year + (1|id_obs) + (1|id_nestbox),
                                  weights = brood_size, data = ntits3, family = "binomial",
                                  ziformula = ~min_t_between + log_F_metric_d2b1)
summary(ttFS_ziglmm1f) # AIC = 1325.3.





### ** 3.1.3. Diagnostics and assumption checks ----
# __________________________________________________

### *** 3.1.3.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(ttFS_ziglmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Strange distribution of residuals with 2 groups.
# performance::check_outliers(ttFS_ziglmm1) # Does not work for this type of model.
ntits3[which(resid < -0.4),] # Nestboxes with the lowest residuals = ~0% fledging success!

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = ntits3$noise_m, y = resid) # Seems rather ok although we once again find patterns linked to the
# sometimes odd distribution of some predictors. However, be reminded that simulated residuals will be
# more useful).
# plot(ttHSy_ziglmm1, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(ttHSy_ziglmm1, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttFS_ziglmm1, n = 1000, re.form = NULL)
simu.resid2 <- DHARMa::simulateResiduals(fittedModel = ttFS_ziglmm2, n = 1000, re.form = NULL) # The
# 're.form' argument is to base simulations on the model unconditional of the random effects (and only works
# for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Ok.
plot(simu.resid2) # Ok.
DHARMa::outliers(simu.resid) # Ok.
DHARMa::outliers(simu.resid2) # Ok.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Slight
# autocorrelation detected (but not when we did not use the OLRE)!
performance::check_autocorrelation(ttFS_ziglmm2) # Ok.
performance::check_collinearity(ttFS_ziglmm2) # Bibof, moderate correlation linked to "species" and VIF > 5 for
# the "F-metric"! GROS GROS PB pour les modèles avec OLRE§§§§§
# UPDATE: sans SPECIES: VIF>8.5 (vs 10) pour F dans model1; et VIF=5.4 (vs 6.9) pour model2! Un peu mieux si
# j'utilise SITE plutôt que ID_NESTBOX!
# AFINIR§§§§§
# AFINIR§§§§§
# AFINIR§§§§§
# AFINIR§§§§§
# AFINIR§§§§§
# AFINIR§§§§§
# AFINIR§§§§§
stats::vcov(ttFS_ziglmm1) # But values of the covariance matrix seem ok.

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
# Some deviations detected for non-included predictors, otherwise ok!



### *** 3.1.3.2. Distribution and dispersion ----
## Assessing over or under-dispersion:
DHARMa::testDispersion(simu.resid) # Ok.
performance::check_overdispersion(x = ttFS_ziglmm1) # Overdispersion detected! However, note that this test
# is known to be inaccurate for ZI-models (see the function's help page). If I run it on the non-ZI model
# (i.e. 'ttFS_glmm1'), the dispersion ratio is much lower, yet we may still need to account for that.
# ATTENTION: Classical overdispersion tests cannot be used to detect overdispersion when OLRE is used to
# account for it.

## Distribution of the predicted probabilities:
probabilities <- stats::predict(object = ttFS_ziglmm1, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities, main = "Predicted proportions", xlab = "Fledging success")
hist(ntits3$fledgling_nb/ntits3$brood_size, main = "Observed proportions", xlab = "Fledging rate")
# The model fails to correctly predict the data. Prediction range is too narrow and the model does not
# predict total successes or failures!

## Zero-inflation:
DHARMa::testZeroInflation(simu.resid) # Ok (significant if the non-ZI model is used)!
# Testing with the non-ZI GLMM:
simu.resid_nozi <- DHARMa::simulateResiduals(fittedModel = ttFS_glmm1, n = 1000)
plot(simu.resid_nozi) # Clear deviations detected!
noziprobabilities <- stats::predict(object = ttFS_glmm1, type = "response") # Extract the predicted
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
dat.resid <- sum(stats::resid(ttFS_ziglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(ttFS_ziglmm1)) # p = 0 (mistake?).

## Computing a pseudo-R2:
performance::r2_nakagawa(ttFS_ziglmm1) # [Additive model]: Marg_R2_glmm = 0.18; Cond_R2_glmm = 0.29.
performance::r2_nakagawa(ttFS_ziglmm2) # [Interactive model]: Marg_R2_glmm = 0.18; Cond_R2_glmm = 0.28.

## Likelihood-ration tests (LRT) of GOF:
# Importance of the random-effects (RE):
ttFS_ziglm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                   species + clutch_size +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_between + year,
                                 weights = brood_size, data = ntits3, family = "binomial",
                                 ziformula = ~1)
ttFS_ziglmm1a <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                    species + clutch_size +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_between + year + (1|id_nestbox) + (1|id_patch),
                                  weights = brood_size, data = ntits3, family = "binomial",
                                  ziformula = ~1) # Intercept only.
summary(ttFS_ziglm1) # The non-mixed model gives AIC = 1456.3 while the mixed-model gave AIC = 1414.1 (with
# "id_nestbox"), or AIC = 1440.2 (with "id_patch"), or AIC = 1445.2 (with "site"), or AIC = 1416 (with "site"
# and "id_nestbox") or AIC = 1416.1 (with "id_patch" and "id_nestbox")! It thus appears that the the use of
# the "id_nestbox" as RE is warranted by the data!

# Importance of the fixed effects:
ttFS_ziglmm0 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ 1 + (1|id_nestbox),
                                weights = brood_size, data = ntits3, family = "binomial",
                                ziformula = ~1)
summary(ttFS_ziglmm0) # AIC = 1564.7 vs 1414.1, so the full model is clearly far better!





### ** 3.1.4. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ -----

### *** 3.1.4.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
ttFS_ziglmm0 <- stats::update(ttFS_ziglmm1, .~. -log_F_metric_d2b1)
summary(ttFS_ziglmm0) # AIC = 1415.5 vs 1414.1 (hypothesis 1 likely not validated)!
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
summary(ttFS_ziglmm1) # AIC = 1333.8 and Marg_R2_glmm = 0.15; Cond_R2_glmm = NA
summary(ttFS_ziglmm2) # AIC = 1332.7 and Marg_R2_glmm = 0.15; Cond_R2_glmm = NA
# Diagnostics ran for 'ttFS_ziglmm1' (initial model) indicated that the model fit the data relatively well
# although several problems have been detected:
# - The raw residuals are divided in two groups suggesting the existence of possibly two different
#   processes and thus, possible outliers (albeit formal test did not find any).
# - Initially, there was overdispersion so I added an OLRE.
# - A slight heteroscedasticity has been found.
# - Signs of possibly problematic multicollinearity have been found for model1 (moderately high VIF values),
#   but the interaction model is fine!
# - The model under-predicts total successes and failures.
# On the other hand, diagnostics also indicated that the use of a RE was appropriate.
## Significant variables: F-metric (+), clutch_size (-), manag_high (-), year2022.
## Almost significant variables: cumdd_30, year2020, and the INTERACTION TERM!
## Hypothesis 1 is likely validated (but NA since convergence issues), but not hypothesis 2 (yet almost)!

# MODEL1 --> AIC = 1415.5 vs 1414.1 (hypothesis 1 likely not validated)!
# [Additive model]: Marg_R2_glmm = 0.18; Cond_R2_glmm = 0.29.
# [Int. model]: Marg_R2_glmm = 0.18; Cond_R2_glmm = 0.28.
# The model fails to correctly predict the data. Prediction range is too narrow and the model does not
# predict total successes or failures!
# + Non-ZI better predicts!!!
# + OVERDISPERSION
# + VIF
# Le fait que PATCH_AREA ne réponde pas peut-être lié au fait qu'on ne prend que la tache focale en compte. Ca
# serait peut-être différent avec WOODY_AREA à 50m ????????

# Preliminary diagnostics for the initial models (not shown here but easily reproducible) also identified
# significant overdispersion for fledging success. To account for that, Harrison (2014 - links:
# https://doi.org/10.7717/peerj.616) recommends to compare the use of an observation-level random effect
# (OLRE) with the use of a beta-binomial model, so that's what we did (see below):

# Note also that in these models, we included "clutch_size" as a predictor because it is likely an important
# predictor of fledging success. However, as could have been expected, it lead to pretty strong
# multicollinearity issues with "species" as it is one of the strongest predictors of tits' clutch size. We
# thus removed "species" of the list of predictors for the benefit of "clutch_size".
# I remove SPECIES because of collinearity issues (if I remove CS, it's worse!)!
# When an OLRE is used, the non-ZI binomial model reproduce the data FAR better than the ZI one. However, its
# AIC is worse and SE are larger... It may be a sign that the ZI is not required but that we should use a
# beta-binomial model instead????





# For the exploratory improved models:
summary(ttFS_ziglmm1d) # AIC = 1327.3 and Marg_R2_glmm = 0.16; Cond_R2_glmm = NA.
## Significant variables: F-metric (+), clutch_size (-), manag_high (-), year2020 and 2022. And also
# [min_t_between (++) for dispersion]!
## Almost significant variables: cumdd_30.
## Hypothesis 1 possibly validated (AIC = 1330.3 vs 1327.3)!
summary(ttFS_ziglmm1f) # AIC = 1325.3 and Marg_R2_glmm = 0.15; Cond_R2_glmm = NA.
## Significant variables: clutch_size (-), manag_high (-), year2020 and 2022, and the INTERACTION EFFECT!
# And also [min_t_between (++) for dispersion]!
## Almost significant variables: [F-metric (-) for dispersion]!
## Hypothesis 2 likely validated!

# Diagnostics for these models were mostly ok, there was no outliers, deviations, dispersion or
# distributional problems, even though there was some residual autocorrelation. However, there was some
# worrying collinearity for model D and F.
# Importantly, switching to "traffic" instead of "urban_intensity" could lower VIF values.
# Predictions are fairly ok but the models still under-predict low fledging rates. They all make too narrow
# predictions and fail to predict correctly the ZI, even D and F.
# Also, strange warning saying that "fledgling_nb" could not be found in the data + singular fit + frequent
# convergence issues (also for the initial models)!





##### *-----------------------------------------------------------------* ######
##### * 3.2. Fledgling number: ZICOM-Poisson GLMM ------------------------------
# ---------------------------------------------------------------------------- #
### ** 3.2.1. Initial model fit ----
# __________________________________

# Given the previous diagnostics, I chose to directly use "woody_area" instead of "woodyveg_vw" and to
# delete the observations for which brood_size = 0 since they likely are true outliers (see comments in
# the previous sections):
ntits3 <- ntits2[-c(which(ntits2$brood_size == 0)),]
ntits3 <- ntits3[-c(44,105,249),] # I also delete the 3 outliers found by initial diagnostics.
ntits3$id_obs <- as.factor(1:nrow(ntits3)) # To create an observation-level RE (OLRE) to account for
# overdispersion.

## Fitting a regular Poisson GLM:
ttFNy_glm1 <- stats::glm(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1 +
                           species +
                           urban_intensity + manag_intensity + light_pollution + noise_iq +
                           cumdd_30 + year,
                         data = ntits3, family = "poisson")

## Fitting a regular Poisson GLMM:
ttFNy_glmm1 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1 +
                                  species +
                                  urban_intensity + manag_intensity + light_pollution + noise_iq +
                                  cumdd_30 + year + (1|site),
                                data = ntits3, family = "poisson")

## Fitting a COM-Poisson GLMM:
ttFNy_comglmm1 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1 +
                                     species +
                                     urban_intensity + manag_intensity + light_pollution + noise_iq +
                                     cumdd_30 + year + (1|site),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1) # Rather long to fit.

## Fitting a ZICOM-Poisson GLMM:
ttFNy_zicomglmm1 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1 +
                                     species +
                                     urban_intensity + manag_intensity + light_pollution + noise_iq +
                                     cumdd_30 + year + (1|site),
                                   data = ntits3, family = glmmTMB::compois(link = "log"),
                                   dispformula = ~1,
                                   ziformula = ~1) # Rather long to fit.
summary(ttFNy_glm1) # AIC = 1889.9
summary(ttFNy_glmm1) # AIC = 1888.3
summary(ttFNy_comglmm1) # AIC = 1840.9.
summary(ttFNy_zicomglmm1) # AIC = 1634.6.

## Fitting an interactive (mediated) COM-Poisson GLMM:
ttFNy_zicomglmm2 <- glmmTMB::glmmTMB(fledgling_nb ~
                                       scale(logged_woody_area, scale = F) *
                                       scale(log_F_metric_d2b1, scale = F) +
                                       species +
                                       urban_intensity + manag_intensity + light_pollution + noise_iq +
                                       cumdd_30 + year + (1|site),
                                     data = ntits3, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~1,
                                     ziformula = ~1) # Rather long to fit.
summary(ttFNy_zicomglmm2) # AIC = 1549.4 (almost significant)!
# Overall, it seems that, if the inclusion of a random effect (RE) only mildly improved the fit, accounting
# for the likely underdispersion and the ZI strongly improved the fit! I will thus carry on with the last two
# models to the diagnostic part.
# Note also that I initially tried to include "clutch_size" (CS) or "brood_size" (BS) as a covariate to focus
# on the variation that is not due to variations in the number of eggs laid or hatched but, if that strongly
# lowered the AIC, diagnostics were once again quite bad (e.g. strong deviations from uniformity). Therefore,
# I chose to remove it which means that we are actually modelling a part of CS and BS variability as well.
# Diagnostics also showed that the "site" RE was to be preferred to the "id_nestbox".

# UPDATE: as before, I explored a few variations and, in addition to the initial models ('ttFNy_zicomglmm1'
# and 'ttFNy_zicomglmm2'), I will also diagnose models 1D and 2B (see below).





### ** 3.2.2. Improved model (exploration) ----
# _____________________________________________

ttFNy_zicomglmm1a <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1 +
                           hatching_rate +
                                       urban_intensity + manag_intensity + light_pollution + noise_iq +
                                       cumdd_30 + year + (1|site),
                                     data = ntits3, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~hatching_rate+log_F_metric_d2b1,
                                     ziformula = ~1)
ttFNy_zicomglmm1b <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1_d2b1 +
                           hatching_rate +
                                       urban_intensity + manag_intensity + light_pollution + noise_iq +
                                       cumdd_30 + year + (1|site),
                                     data = ntits3, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~hatching_rate+log_F_metric_d2b1_d2b1,
                                     ziformula = ~1)
ttFNy_zicomglmm1c <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1 +
                           hatching_rate +
                                       urban_intensity + manag_intensity + light_pollution + noise_iq +
                                       cumdd_30 + year + (1|site),
                                     data = ntits3, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~hatching_rate+log_F_metric_d2b1_d2b1,
                                     ziformula = ~1)
ttFNy_zicomglmm1d <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1 +
                           hatching_rate +
                                       urban_intensity + manag_intensity + light_pollution + noise_iq +
                                       cumdd_30 + year + (1|site),
                                     data = ntits3, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~hatching_rate+log_F_metric_d2b1,
                                     ziformula = ~min_t_between+log_F_metric_d2b1)
summary(ttFNy_zicomglmm1a) # AIC = 1601.2 vs 1634.6.
summary(ttFNy_zicomglmm1b) # AIC = 1607.4.
summary(ttFNy_zicomglmm1c) # AIC = 1604.2.
summary(ttFNy_zicomglmm1d) # AIC = 1594.1.

ttFNy_zicomglmm2a <- glmmTMB::glmmTMB(fledgling_nb ~
                                       scale(logged_woody_area, scale = F) *
                                        scale(log_F_metric_d2b1, scale = F) +
                           hatching_rate +
                                       urban_intensity + manag_intensity + light_pollution + noise_iq +
                                       cumdd_30 + year + (1|site),
                                     data = ntits3, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~hatching_rate+log_F_metric_d2b1,
                                     ziformula = ~1)
ttFNy_zicomglmm2b <- glmmTMB::glmmTMB(fledgling_nb ~
                                       scale(logged_woody_area, scale = F) *
                                        scale(log_F_metric_d2b1, scale = F) +
                           hatching_rate +
                                       urban_intensity + manag_intensity + light_pollution + noise_iq +
                                       cumdd_30 + year + (1|site),
                                     data = ntits3, family = glmmTMB::compois(link = "log"),
                                     dispformula = ~hatching_rate+log_F_metric_d2b1,
                                     ziformula = ~min_t_between+log_F_metric_d2b1)
summary(ttFNy_zicomglmm2a) # AIC = 1592.4 vs 1857.1.
summary(ttFNy_zicomglmm2b) # AIC = 1585.3 vs 1857.1.





### ** 3.2.3. Diagnostics and assumption checks ----
# __________________________________________________

### *** 3.2.3.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(ttFNy_zicomglmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Ok-ish but likely two groups.
ntits3[which(resid < -4),] # As could have been guessed from the plot, the 2nd group (with the lowest
# residuals) consist of all the observations where "fledgling_nb" ~ 0 or is low. It could be a sign that
# the zero-part of the model should indeed be modelled with more relevant predictors.

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = ntits3$hatching_rate, y = resid,
     xlab = "Hatching rate", ylab = "Raw residuals") # Heteroscedasticity for HS?

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttFNy_zicomglmm1, n = 1000, re.form = NULL)
simu.resid2 <- DHARMa::simulateResiduals(fittedModel = ttFNy_zicomglmm2, n = 1000, re.form = NULL)
simu.resid1d <- DHARMa::simulateResiduals(fittedModel = ttFNy_zicomglmm1d, n = 1000, re.form = NULL)
simu.resid2b <- DHARMa::simulateResiduals(fittedModel = ttFNy_zicomglmm2b, n = 1000, re.form = NULL)
# The 're.form' argument is to base simulations on the model unconditional of the random effects (and only
# works for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted
# eventually.
plot(simu.resid) # Slight deviation for model2!
DHARMa::outliers(simu.resid) # None.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Nope!
performance::check_autocorrelation(ttFNy_zicomglmm1) # Ok.
performance::check_collinearity(ttFNy_zicomglmm1) # Nope, moderate correlation for "species" for the
# initial models! Ok for models 1D and 2B!
stats::vcov(ttFNy_zicomglmm1) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = ntits3$logged_woody_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_F_metric_d2b1)
DHARMa::plotResiduals(simu.resid, form = ntits3$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$logged_herby_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$light_pollution) # Slight quantile deviation (model1)!
DHARMa::plotResiduals(simu.resid, form = ntits3$noise_iq) # Slight quantile deviation (model1 and 1D)!
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_30) # Slight quantile deviation (model1 and 2)!
DHARMa::plotResiduals(simu.resid, form = ntits3$min_t_between)
DHARMa::plotResiduals(simu.resid, form = ntits3$species)
DHARMa::plotResiduals(simu.resid, form = ntits3$year)
DHARMa::plotResiduals(simu.resid, form = ntits3$clutch_size) # Strong quantile deviation!
DHARMa::plotResiduals(simu.resid, form = ntits3$brood_size) # Strong quantile deviation!
DHARMa::plotResiduals(simu.resid, form = ntits3$hatching_rate) # Strong quantile deviation (models 1 and 2)!
# Clearly, there are quite heavy deviation problems for models 1, 2, and some for model 1D. Model 2B however,
# was fine for all variables!



### *** 3.2.3.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
aods3::gof(ttFNy_zicomglmm1) # Does not work for this type of model!
AER::dispersiontest(object = ttFNy_zicomglmm1, alternative = c("less")) # Does not work for this model type!
DHARMa::testDispersion(simu.resid) # Ok.

## Theoretical count distribution:
theo_count <- COMPoissonReg::rzicmp(n = nrow(ntits3), lambda = mean(ntits3$fledgling_nb),
                                    nu = 1.1,  # The 'nu' parameter should be chosen by trial-and-errors.
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
pred_counts <- stats::predict(object = ttFNy_zicomglmm2b, type = "response") # Extract the predicted counts.
par(mfrow= c(1,2))
hist(pred_counts, main = "Predicted counts", xlab = "Fledgling number")
hist(ntits3$fledgling_nb, main = "Observed counts", xlab = "Fledgling number")
# The "ttFNy_zicomglmm1" model yields quite poor predictions. Exploratory models yield better results but
# all over-predict low counts and fail to properly model the ZI!

## Zero-inflation (ZI):
simu.resid_woZI <- DHARMa::simulateResiduals(fittedModel = ttFNy_comglmm1, n = 1000) # Model without ZI.
DHARMa::testZeroInflation(simu.resid) # Nope.
DHARMa::testZeroInflation(simu.resid_woZI) # Yes, so there truly is a ZI. Yet, this model accounts for it but
# cannot predict it properly.



### *** 3.2.3.3. Linearity ----
## Plotting the response on the log scale against predictors:
ntits3 %>% dplyr::select(log_patch_area, logged_woody_area, logged_woodyvol,
                         log_F_metric_d2b1, log_F_metric_d2b1_d1, log_F_metric_d2b1_d1b1, log_F_metric_d2b1,
                         urban_intensity, logged_herby_area, logged_built_area, logged_traffic,
                         light_pollution, noise_m, noise_iq,
                         cumdd_30, min_t_before, min_t_between,
                         hatching_rate, brood_size, clutch_size) -> mydata
predictors <- colnames(mydata)
# Bind log(Y) and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(log_y = log(ntits3$fledgling_nb+1)) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # The linearity is respected.



### *** 3.2.3.4. Model goodness-of-fit (GOF) and performances ----
# GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(ttFNy_zicomglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(ttFNy_zicomglmm1)) # p ~ 0.4-0.55 for all models, indicating
# that there is no significant lack of fit. Keep in mind though that GOF measures for mixed models is an
# extremely complicated topic and interpretations are not straightforward.

# Computing a pseudo-R2:
performance::r2_nakagawa(ttFNy_zicomglmm1) # [Additive model]: Marg_R2_glmm = 0.03; Cond_R2_glmm = 0.05.
performance::r2_nakagawa(ttFNy_zicomglmm2) # [Interactive model]: Marg_R2_glmm = 0.04; Cond_R2_glmm = 0.05.
performance::r2_nakagawa(ttFNy_zicomglmm1d) # [Additive model]: NA.
performance::r2_nakagawa(ttFNy_zicomglmm2b) # [Interactive model]: NA.


## Likelihood-ration tests (LRT) of GOF:
# For the random-effects (RE):
ttFNy_zicomglm1 <- glmmTMB::glmmTMB(fledgling_nb ~ logged_woody_area + log_F_metric_d2b1 +
                                      species +
                                      urban_intensity + manag_intensity + light_pollution + noise_iq +
                                      cumdd_30 + year,
                                    data = ntits3, family = glmmTMB::compois(link = "log"),
                                    dispformula = ~1,
                                    ziformula = ~1) # Rather long to fit.
summary(ttFNy_zicomglm1) # AIC = 1635 vs 1634.6, so the inclusion of the RE does not seem warranted by the
# data. Still, we will keep using it as it is the originally intended model.

## For the whole model:
ttFNy_zicomglm0 <- glmmTMB::glmmTMB(fledgling_nb ~ 1,
                                    data = ntits3, family = glmmTMB::compois(link = "log"),
                                    dispformula = ~1,
                                    ziformula = ~1)
res.LRT_null <- stats::anova(object = ttFNy_zicomglm0, ttFNy_zicomglm1, test = "LRT")
# The test is significant, confirming that the model is useful to explain the data.



### *** 3.2.3.5. Posterior predictive simulations ----
# Predicted counts:
par(.pardefault)
obsprop <- prop.table(table(ntits3$fledgling_nb))
sims <- stats::simulate(ttFNy_zicomglmm1, nsim = 1000)

nsim0 <- colSums(sims == 0) # Number of zeros (min obs value)
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim0)),
     ylab="Probability", xlab="Number of zeros (true = 42)")
(obs0 <- sum(ntits3$fledgling_nb == 0))
points(obs0, 0.12, col="red", pch=16, cex=2) # See the y (0.06) values in 'obsprop'!

nsim8 <- colSums(sims == 8) # Number of eights (second modal obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim8)),
     ylab="Probability", xlab="Number of eights (true = 63)")
(obs8 <- sum(ntits3$fledgling_nb == 8))
points(obs8, 0.17, col="red", pch=16, cex=2)

nsim12 <- colSums(sims == 12) # Number of twelves (max obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim12)),
     ylab="Probability", xlab="Number of twelves (true = 2)")
(obs12 <- sum(ntits3$fledgling_nb == 12))
points(obs12, 0.005, col="red", pch=16, cex=2)
# These three examples confirm that the model still predicts values that are too wide and overestimated
# compared to the true observed values. Small successes are not correctly predicted!





### ** 3.2.4. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ -----

### *** 3.2.4.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## Parametric bootstrap to test the additive effect of the connectivity metric:
ttFNy_zicomglmm0 <- stats::update(ttFNy_zicomglmm1, .~. -log_F_metric_d2b1)
ttFNy_zicomglmm0d <- stats::update(ttFNy_zicomglmm1d, .~. -log_F_metric_d2b1)
summary(ttFNy_zicomglmm0) # AIC = 1634.3 vs 1634.6 (hypothesis likely not validated)!
summary(ttFNy_zicomglmm0d) # AIC = 1596.5 vs 1594.1 (hypothesis likely not validated)!
# I do not run PB-based LRT for now as they take too long to run.

# tictoc::tic("Parametric bootstrap LRT for the additive effect")
# res.LRT_inteff <- DHARMa::simulateLRT(m0 = ntitsFNy_zicomglmm0, m1 = ntitsFNy_zicomglmm1, n = 500, seed = 97)
# tt <- as.data.frame(cbind(res.LRT_inteff$method,
#                           res.LRT_inteff$data.name,
#                           res.LRT_inteff$statistic,
#                           res.LRT_inteff$p.value))
# rownames(tt) <- NULL
# tt %>% dplyr::rename("Method" = V1,
#                      "Models" = V2,
#                      "Log Likelihood (M1/M0)" = V3,
#                      "p-value" = V4) -> tt
# readr::write_csv2(x = tt, file = here::here("output", "tables", "res.ntitsFNy_LRT_addeff.csv"))
# tictoc::toc() # DISCLAIMER: took ~??? to run!
# # The PB-based LRT is XXX, indicating that our connectivity metric (does not?) improve the
# # description of the data here.
# # NOTE: initially, normally I would use the more efficient 'pbkrtest::PBmodcomp()' instead of the
# # 'DHARMa::simulateLRT()' function, but it doesn't work with {glmmTMB} objects.


## Parametric bootstrap to test the interactive effect of the connectivity metric:
# To save some time, I don"t even bother computing the PB-based LRT for the interaction effect, it won't be
# significant. I should improve my proxies and my models first.



### *** 3.2.4.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for the additive COM-Poisson GLMM parameters")
res.ntitsFNy_addeff_CI_boot <- confint(ntitsFNy_zicomglmm1, method="boot")
tt <- as.data.frame(res.ntitsFNy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ntitsFNy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~??? to run!



### *** 3.2.4.3. Conclusion ----
# For the initial model:
summary(ttFNy_zicomglmm1) # AIC = 1634.6 and Marg_R2_glmm = 0.03; Cond_R2_glmm = 0.05.
summary(ttFNy_zicomglmm2) # AIC = 1633.7 and Marg_R2_glmm = 0.04; Cond_R2_glmm = 0.05.
# Diagnostics ran for 'ttFNy_zicomglmm1' and 2 (initial models) indicated that the models do not fit the data
# very well. Moreover, several problems were detected:
# - The raw residuals are divided in two groups suggesting the existence of possibly two different
#   processes and thus, possible outliers (albeit formal test did not find any).
# - Quite strong deviations from homogeneity have been found in the simulated residuals!
# - Signs of possibly problematic multicollinearity have been found (moderately high VIF values), likely
#   due to species and thus to missing variables such as CS, BS or HS!
# - The models do not make accurate predictions, especially of the ZI.
## Significant variables: speciesCC (+), cumdd_30 (-), year2020 and 2022 (+).
## Almost significant variables: light_pollution and the interaction term (only for model2).
## Hypothesis 1 and 2 are probably not validated!


# For the exploratory improved models:
summary(ttFNy_zicomglmm1d) # AIC = 1594.1 and NA for R2.
## Significant variables: F-metric (-), hatching_rate (++), cumdd_30 (-), year2022 (++). And also
# [min_t_between (+) for ZI; and HS (--) and F-metric (-) for dispersion]!
## Almost significant variables: year2020. And also [F-metric (-) for ZI]!
## Hypothesis 1 may be validated but really NOT SURE (AIC = 1596.5 vs 1594.1)!
summary(ttFNy_zicomglmm2b) # AIC = 1585.3 and NA for R2.
## Significant variables: F-metric (-), hatching_rate (++), manag_high (-), cumdd_30 (-), year2022 (++), and
# the INTERACTION EFFECT (-)! And also [min_t_between (+) for ZI; and HS (--) and F-metric (-) for dispersion]!
## Almost significant variables: year2020, light_pollution (-). And also [F-metric (-) for ZI]!
## Hypothesis 2 likely validated but hard to interpret.

# Diagnostics for these models were mostly ok, there was no outliers or distributional problems, and only a
# slight deviation for model 1D while 2B was fine. VIF values were ok for both models.
# However, predictions were rather poor even though they were slightly better than for the initial models.
# All models over-predicted low counts and fail to properly model the ZI!
# Once again, it may be a sign of the need to better model the ZI or to consider these values as outliers that
# should be removed. That could perhaps explain why the F-metric had a NEGATIVE EFFECT of fledgling numbers!
# It could also be due to missing variables or interactions... Anyway, these models are encouraging but
# not satisfying.





########################## ************************************************* ###############################
# ----------------------------------------------- #
##### 4. Modelling the morphometric variables #####
# ----------------------------------------------- #

##### * 4.1 Mass: LMM ---------------------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 4.1.1. Initial model fit ----
# __________________________________

ntits2 %>% dplyr::filter(is.na(mass) == FALSE) -> ntits3 # Only 309 observations left.

## Fitting a regular linear model:
ttMAy_lm1 <- stats::lm(mass ~ logged_woody_area + log_F_metric_d2b1 +
                         species + clutch_size +
                         urban_intensity + manag_intensity + light_pollution + noise_iq +
                         cumdd_30 + year, data = ntits3)

## Fitting an additive LMM:
ttMAy_lmm1 <- glmmTMB::glmmTMB(mass ~ logged_woody_area + log_F_metric_d2b1 +
                                 species + clutch_size +
                                 urban_intensity + manag_intensity + light_pollution + noise_iq +
                                 cumdd_30 + year + (1|id_nestbox),
                                data = ntits3, family = "gaussian")
ttMAy_lmm1b <- lme4::lmer(mass ~ logged_woody_area + log_F_metric_d2b1 +
                            species + clutch_size +
                            urban_intensity + manag_intensity + light_pollution + noise_iq +
                            cumdd_30 + year + (1|id_nestbox), data = ntits3,
                            control=lme4::lmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5))) # Same model but fitted
# with {lme4} for comparison sake! Note however, that it is fitted by REML and, if not, it is singular!

## Fitting interactive (mediated) LMMs:
ttMAy_lmm2 <- glmmTMB::glmmTMB(mass ~
                           scale(logged_woody_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                           species + clutch_size +
                           urban_intensity + manag_intensity + light_pollution + noise_iq +
                           cumdd_30 + year + (1|id_nestbox),
                           data = ntits3, family = "gaussian")
ttMAy_lmm2b <- lme4::lmer(mass ~
                            scale(logged_woody_area, scale = F) * scale(log_F_metric_d2b1, scale = F) +
                            species + clutch_size +
                            urban_intensity + manag_intensity + light_pollution + noise_iq +
                            cumdd_30 + year + (1|id_nestbox), data = ntits3,
                          control=lme4::lmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun=2e5)))
AIC(ttMAy_lm1) # AIC = 1128.3.
summary(ttMAy_lmm1) # AIC = 1130.3.
summary(ttMAy_lmm1b) # AIC = NA but likely very close.
summary(ttMAy_lmm2) # AIC = 1132.1, does not seem supported by the data!
# Note that for both LMM, RE variance is extremely low (nearly singular fit) which, with the AIC, suggests
# that the use of a mixed-model is not warranted by the data.
# OR hatching_rate/BS????????
# OR hatching_rate/BS????????
# OR hatching_rate/BS????????
# OR hatching_rate/BS????????
# OR hatching_rate/BS????????






### ** 4.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 4.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Extracting residuals (with the {redres}):
raw_cond <- redres::compute_redres(ttMAy_lmm1b) # Computes the raw conditional residuals (conditional on
# the random effects (RE)).
pearson_mar <- redres::compute_redres(ttMAy_lmm1b, type = "pearson_mar") # Computes the Pearson marginal
# (not accounting for the RE) residuals.
std_cond <- redres::compute_redres(ttMAy_lmm1b, type = "std_cond") # Computes the studentised cond. ones.
# Joins the residuals to the data:
xxx <- cbind(ntits3, raw_cond, pearson_mar, std_cond)

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttMAy_lmm1b, n = 1000, plot = FALSE)
par(.pardefault)
plot(simu.resid) # Ok.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(ttMAy_lmm1b) # Ok.
performance::check_collinearity(ttMAy_lmm1b) # Ok-ish but some VIF > 4-5 and moderate correlation for
# "manag_intensity"!
stats::vcov(ttMAy_lmm1b) # Ok-ish.



### *** 4.1.2.2. Distribution and homoscedasticity ----
## Assessing the normality of the residuals:
stats::shapiro.test(xxx$raw_cond) # Ok. But plotting would be better:
redres::plot_resqq(ttMAy_lmm1b) # As expected, the plot shows a substantial departure from Normality at the
# extreme ends of the quantiles, that is at the border of the parameters space. Overall, as almost all
# points stay within the 95% CI, we can say it is ok-ish.
# Plotting them against each predictor:
xxx %>%
  tidyr::gather(key = "type", value = "residual",
                c(logged_woody_area, log_F_metric_d2b1, clutch_size, urban_intensity, light_pollution,
                  noise_iq, cumdd_30)) %>%
  ggplot2::ggplot(ggplot2::aes(x = residual)) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::facet_grid(. ~ type, scales = "free") +
  ggplot2::theme_bw() # Residuals vs predictor plots look rather ok, even though some strange patterns can
# be seen. I would not be too worried about them but they would require some further thinking.

## Assessing the normality in the random effect:
redres::plot_ranef(ttMAy_lmm1b) # Same thing here.

## Assessing homogeneity of variance and influential observations:
plot(ttMAy_lmm1b, type=c("p","smooth"), col.line = 2, id = 0.05, idLabels = ~.obs,
     ylab = "Pearson's residuals", xlab = "Fitted values") # It's ok but there are a few possible outliers:
ntits3[c(245,213,112,194,102,32,155),] # High residuals ~= heavy juveniles. RAS.
ntits3[c(26,42,198,152,256,236,173,192,170),] # Low residuals ~= light juveniles. RAS.

# Residuals vs leverage:
plot(ttMAy_lmm1b, stats::rstudent(.) ~ stats::hatvalues(.))
cd <- stats::cooks.distance(ttMAy_lmm1b)
plot(cd, ylab = "Cook's distance")
ntits3[which(cd>0.4),] # Ok, all observations are < 0.5, so no overly influential points.
ntits3[which(cd>0.1),] # Even with very conservative values, it's ok!

## Residuals vs predictors:
redres::plot_redres(ttMAy_lmm1b, xvar = "cumdd_30") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Residual vs predictor") # Globally ok, but strange pattern for CS?
# plot(ntits3$log_F_metric_d2b1, stats::residuals(ttMAy_lmm1b)) # Same plot (I should create a
# custom function).
redres::plot_redres(ttMAy_lmm1b, type = "raw_mar", xvar = "manag_intensity") # Ok.

## Distribution of the predicted values:
par(.pardefault)
predictions <- stats::predict(object = ttMAy_lmm1b, type = "response") # Extract the predicted values.
par(mfrow= c(1,2))
hist(predictions, main = "Predicted mass", xlab = "Nestling mass (g)")
plot(ecdf(predictions), main = "Predicted CDF", xlab = "Nestling mass (g)")
fitdistrplus::plotdist(data = ntits3$mass, histo = TRUE, demp = TRUE) # Rather ok.



### *** 4.1.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
ntits3 %>% dplyr::select(log_patch_area, logged_woody_area, logged_woodyvol,
                         log_F_metric_d2b1, log_F_metric_d2b1_d1, log_F_metric_d2b1_d1b1, log_F_metric_d2b1,
                         urban_intensity, logged_herby_area, logged_built_area, logged_traffic,
                         light_pollution, noise_m, noise_iq,
                         cumdd_30, min_t_before, min_t_between,
                         hatching_rate, brood_size, clutch_size) -> mydata
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
performance::r2_nakagawa(ttMAy_lmm1b) # [Additive model]: Marg_R2_lmm = 0.79; Cond_R2_lmm = 79.
performance::r2_nakagawa(ttMAy_lmm2) # [Interact. model]: Marg_R2_lmm = 0.79; Cond_R2_lmm = NA.

## Likelihood-ration tests (LRT) of GOF:
# Importance of the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT")
res.LRT_re <- DHARMa::simulateLRT(m0 = ttMAy_lm1, m1 = ttMAy_lmm1b, n = 1000, seed = 18)
tictoc::toc() # Took ~24s to run.
# The LRT is highly significant, suggesting that M1 better describes the data than M0, supporting the
# importance of the random effect!

# Importance of the fixed effects (only using the LM):
ttMAy_lm0 <- stats::lm(mass ~ 1, data = ntits3)
res.LRT_null <- stats::anova(object = ttMAy_lm0, ttMAy_lm1, test = "LRT")
# The test is highly significant, confirming that the model is useful to explain the data.





### ** 4.1.3. Inference and predictions ----
# __________________________________________

### *** 4.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
ttMAy_lmm0b <- stats::update(ttMAy_lmm1b, .~. -log_F_metric_d2b1)

res.LRT_addeff <- pbkrtest::PBmodcomp(ttMAy_lmm1b,
                                      ttMAy_lmm0b, nsim = 1000, seed = 7) # Took ~38s to run!
readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
                                                             "res.ttMAy_LRT_addeff.csv"))
# The LRT is significant, indicating that our connectivity metric does improve the description of the data.


## For the interaction effect:
res.LRT_inteff <- pbkrtest::PBmodcomp(ttMAy_lmm2b,
                                      ttMAy_lmm1b, nsim = 1000, seed = 444) # Took ~37s to run!
readr::write_csv2(x = res.LRT_inteff$test, file = here::here("output", "tables",
                                                             "res.ttMAy_LRT_inteff.csv"))
# The LRT is NOT significant, indicating that our hypothesis of an interaction effect is not supported
# by the data.



### *** 4.1.3.2. Bootstrapped confidence intervals for estimated parameters ----
tictoc::tic("Bootstrap CI for additive LMM parameters")
res.ttMAy_addeff_CI_boot <- confint(ttMAy_lmm1b, method="boot")
tt <- as.data.frame(res.ttMAy_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttMAy_bootCI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~8s to run!



### *** 4.1.3.3. Conclusion ----
# For the initial model:
summary(ttMAy_lmm1b) # AIC = 1157.1 and Marg_R2_lmm = 0.79; Cond_R2_lmm = 79.
summary(ttMAy_lmm2b) # AIC = 1157.5 and Marg_R2_lmm = 0.79; Cond_R2_lmm = NA.
# Diagnostics ran for 'ttMAy_lmm1b' and 'ttMAy_lmm2b' (initial models) indicated that the models do fit the
# data relatively well and that there are no major cause for concern:
# - All sorts of residuals are globally fine. Even though some patterns exist and may require improvements.
# - Assumptions all seem validated, although there is a mild multicollinearity with some VIF > 4-5 and
#   moderate correlation for "manag_intensity".
# - No true outliers but some with rather strange values (wide range for PM).
## Significant variables: F-metric (++), speciesCC (---), clutch_size (---), year2020 and 2022 (+).
## Almost significant variables: light_pollution (-) and noise_iq (-).
## Hypothesis 1 is validated (PB-based LRT; p = 0.043) but hypothesis 2 is rejected (p = 0.64)!

# I did not run exploratory improved models yet, but improvements are likely possible.





########################## ************************************************* ###############################
par(.pardefault)
############################################ MAJJJJJJJ§§§§§§§§ todolist ####################################
# - Tester avec ou sans outliers (noise_m, connectivity???); si ça change beaucoup les résultats, rapporter
#   les deux??? Tester aussi patch_perim + traffic + choisir RE définitifs??? (e.g. id_patch + site???)
#   + tester id_nestbox (3 REs????)???
#   + model laying date??????????????
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
