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





# ----------------- #
##### PM MODELS #####
# ----------------- #

##### * 0. Data transformations ------------------------------------------------
# ---------------------------------------------------------------------------- #


# Rescaling variables:
pm %>% dplyr::mutate(woodyveg_vw = woodyveg_vw/1000, # Converting m3 into dm3.
                     noise_m = noise_m/10, # Converting dB into B.
                     cumdd_30 = cumdd_30/100) %>% # Converting degree-days into hundred of degree-days.
  dplyr::mutate(logged_Fmetric = log10(pmF_d531_beta0),
                logged_woodyveg = log10(woodyveg_vw)) %>%
  dplyr::mutate(manag_low = ifelse(manag_intensity == "0", "1", "0"),
                manag_mid = ifelse(manag_intensity == "1", "1", "0"),
                manag_high = ifelse(manag_intensity == "2", "1", "0")) %>%
  dplyr::mutate(dplyr::across(where(is.matrix), as.numeric),
                dplyr::across(where(is.character), as.factor)) -> pm2
## QUID de factors????? manag_intensity + year (et quid des levels de year?)!!!
# schielzeth dit dummy coding + centrage -----> mais ça dépend des objectifs!!!
# Sans dummy: manag0 = gestion nat; year0 = 2019!
# Mais poser question CV car toujours pas vraiment régler cette histoire de scaling/centrage!!!!!!
# I could also try median-centering, that would be more relevant????





##### * 1. Clutch size: Poisson GLMM -------------------------------------------
# ---------------------------------------------------------------------------- #

### ** 1.1. Model fit ----
# ________________________

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
# Even then, estimates and p-values are extremely surprising as only one effect is significant, which
# I do not believe... It is thus time to investigate things further, notably to assess Poisson
# regression assumptions.

# But first, following Ben Bolker's advices, I'll try the approach proposed by Chung et al. (2013)
# that sets a weak prior on the variance to get an approximate Bayesian maximum a posteriori estimate
# that avoids singularity:
pmCy_bglmm0 <- blme::bglmer(clutch_size ~ logged_woodyveg + logged_Fmetric + urban_intensity +
                              manag_low + manag_high + light_pollution + noise_m +
                              cumdd_30 + father_cond + mother_cond + year + (1|id_nestbox),
                            data = pm2, family = poisson)
summary(pmCy_bglmm0)
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
# summary(pmCy_bglmm1) # It now converges and throws no warnings but estimates are still weird!
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



### ** 1.2. Diagnostics and assumption checks ----
# ________________________________________________

### *** 1.2.1. Poisson distribution ----
# Theoretical count distribution:
theo_count <- rpois(n = nrow(pm), lambda = mean(pm$clutch_size))
tc_df <-data.frame(theo_count)

ggplot2::ggplot(pm, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that clutch_size may not be following a Poisson distribution: it seems slightly
# underdispersed and relatively symmetrical.
# hist(rnorm(n = nrow(pm), mean = mean(pm$clutch_size), sd = stats::sd(pm$clutch_size))) # Perhaps Normal?


# Assessing over or under-dispersion:
aods3::gof(pmCy_bglmm1)
# The sum of squared Pearson residuals is less than the residual degrees of freedom, it's a known
# sign of underdispersion! We can confirm it by dividing the residual deviance by the number of degrees
# of freedom: 65.7/245=0.27 (<< 1), so underdispersion!
AER::dispersiontest(object = pmCy_glm0, alternative = c("less"))
# The data are indeed significantly under-dispersed (explaining the lack of fit?).


### *** 1.2.2. Model goodness-of-fit and performances ----
# Residual analyses:
plot(pmCy_bglmm1)
plot(pmCy_bglmm1, id_nestbox~stats::resid(.)) # Justification for the nestbox random effect (RE).
plot(pmCy_bglmm1, site~stats::resid(.)) # That's interesting because it shows that there's quite a lot
# of among and within sites variability in the residuals. It may mean a "site" RE is required.
# Note also that "chateau_de_larrey" is an 'outlier' site and 3 other sites have extreme values
# ("seminaire", "chateau_de_pouilly" and "arquebuse").
# However, according to glmer(), this variation is still consistent with Poisson variation among
# otherwise identical sites. Still following Bolker, we will do posterior predictive simulations to test
# if the model is behaving like the data.


# Posterior predictive simulations:
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

# Let's try with the among-nestbox variance:
sims2 <- simulate(pmCy_bglmm1,nsim=1000)
vfun <- function(x) {
  m_new <- update(pmCy_bglmm1, data = transform(pm2, clutch_size = x))
  pm2$.resid <- stats::residuals(m_new, "pearson")
  nestboxmeans <- plyr::ddply(pm2, "id_nestbox", plyr::summarise, mresid = mean(.resid))
  stats::var(nestboxmeans$mresid)
}
vdist <- sapply(sims2, vfun) # VERY LONG§§§

pm2$.glmresid <- stats::residuals(pmCy_bglmm1, "pearson")
obs_boxmeans <- plyr::ddply(pm2, "id_nestbox", plyr::summarise, mresid = mean(.glmresid))
obs_boxvar <- stats::var(obs_boxmeans$mresid)
par(las=1,bty="l")
hist(vdist, breaks = 30, col = "gray", freq = FALSE, main = "",
     xlab="Among-nestbox variance in residuals")
par(xpd = NA) ## prevent point getting cut off at top of plot
points(obs_boxvar, §§§§§, col="red", pch=16, cex=2) # §§§§§ = variance estimée de id_nestbox par le modèle


# Likelihood-ration tests (LRT) of GOF:
# See hartig for LRT for RE!
# + LRT avec modèle null?????
# + FINIR checks/diags by https://datavoreconsulting.com/post/count-data-glms-choosing-poisson-negative-binomial-zero-inflated-poisson/
# and https://bookdown.org/egarpor/PM-UC3M/glm-diagnostics.html#glm-diagnostics-1



### ** 1.3. Inference and predictions ----
# ________________________________________

### *** 1.2.1. Hypothesis testing ----
# + WALD for fixed effects
# See bolker + delladata for other stuff in inference+predictions
# See bolker + delladata for other stuff in inference+predictions
# See bolker + delladata for other stuff in inference+predictions


par(.pardefault)

######################################### TO DO LIST ####################################################
# - Finish diagnostics and assumption checks: ii) truc ZIpoisson pscl
# - Tester Sdt comme NOVA avec MEDIAN et 1.5*IQR????? (C'est d'ailleurs la step1 de Bolker pour la convergence)!
# - LRT boot (Hartig?) to assess random effects + null model (~1)?????
# - Simplify!
# - Try binomial.
# - Try LMM.
# - Move on to another Y.
# - Try the Conway-Maxwell Poisson (Shmueli et al, 2005*) avec {COMPoissonReg} (Sellers & Lotze, 2015*).
# - Compare all methods of estimation and inference (cf. inference bolker example) for the **same model**
#   to see if any is making a difference????

# - Idem mais avec CC en plus, year en effet fixe????? (mais sans paternal cond!) --> mais QUID de
#   possibles interactions entre species et autres X??????
#_______________________________________________________________________________________________________#
