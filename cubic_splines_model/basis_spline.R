library(brms)
library(ggplot2)
library(tidyr)
library(dplyr)
library(bayesplot)

# ------------------------------------------------------------------------------
# Setup: Create Folders If Not Exist
# ------------------------------------------------------------------------------
dir.create("Results/cs_model", recursive = TRUE, showWarnings = FALSE)
dir.create("Results/cs_model/R_data_files", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------
source("data_upload.R")
source("feature_selections.R")
source("spatial_prep.R")

# # Prepare data for the model
# data <- list(
#   y = data$ViolentCrimesPerPop,
#   PctKids2Par = data$PctKids2Par,
#   PctImmigRec10 = data$PctImmigRec10,
#   PctPopUnderPov = data$PctPopUnderPov,
#   medFamInc = data$medFamInc,
#   racePctWhite = data$racePctWhite,
#   lat = data$Latitude_scaled,
#   lon = data$Longitude_scaled,
#   N = nrow(data)
# )

# ------------------------------------------------------------------------------
# Formula & Priors for Gaussian Process Model
# ------------------------------------------------------------------------------


knots_list <- list(
  Longitude_scaled = quantile(data$Longitude_scaled, probs = c(0.25, 0.5, 0.75)), 
  Latitude_scaled  = quantile(data$Latitude_scaled,  probs = c(0.25, 0.5, 0.75))
)

formula.base <- bf(ViolentCrimesPerPop ~ s(Longitude_scaled, bs = "bs", k = 10) +
                     s(Latitude_scaled, bs = "bs", k = 10) 
)


formula.cs <- update(formula.base, paste("~ . +", paste(ten_features, collapse = " + ")) )

priors.cs <- c(
  prior(normal(0, 4), class = "b"),             # Regular slope priors for covariates
  prior(normal(0, 1), class = "Intercept"),     # Intercept prior
  prior(exponential(3), class = "sds"),         # Smoothness of splines
  prior(gamma(2, 0.1), class = "phi")           # Precision parameter for Beta regression
)

# ------------------------------------------------------------------------------
# Prior Predictive Check
# ------------------------------------------------------------------------------
fit_prior_cs <- brm(
  formula = formula.cs,
  data = data,
  prior = priors.cs,
  family = "beta",
  sample_prior = "only",  
  chains = 4, 
  cores = 4,
  iter = 4000
)


set.seed(42)
ppc_plot_cs <- pp_check(fit_prior_cs, ndraws = 30) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = ppc_plot_cs, filename = "Results/cs_model/cs_ppc.png")
saveRDS(ppc_plot_cs, file = "Results/cs_model/R_data_files/cs_ppc.rds")

# ------------------------------------------------------------------------------
# Model Fitting
# ------------------------------------------------------------------------------
fit_cs <- brm(
  formula = formula.cs,
  data = data,
  prior = priors.cs,
  family = "beta",
  chains = 4, 
  cores = 4,
  iter = 4000,
  seed = 42
  )

pp_check(fit_cs)
