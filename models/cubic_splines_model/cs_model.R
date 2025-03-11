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

spatial.features <- c(five_features, "Longitude_scaled", 
                      "Latitude_scaled", "ViolentCrimesPerPop")

data <- data %>% select(all_of(c(spatial.features)))
names(data)
dim(data)

# ------------------------------------------------------------------------------
# Formula & Priors for Gaussian Process Model
# ------------------------------------------------------------------------------
knots_list <- list(
  lon_knots = quantile(data$Longitude_scaled, probs = c(0.25, 0.5, 0.75)), 
  lat_knots  = quantile(data$Latitude_scaled,  probs = c(0.25, 0.5, 0.75))
)


formula.base <- as.formula(ViolentCrimesPerPop ~ s(Latitude_scaled, bs = "cs") + 
                             s(Longitude_scaled, bs = "cs"))

formula.cs <- update(formula.base, paste(". ~ . +", paste(five_features, collapse = " + ")))

# for 10 features
priors.cs <- c(
  prior(normal(-1.0, 0.1), class = "Intercept"),  
  prior(normal(0, 2), class = "b"),                
  prior(exponential(8), class = "sds"),         # Smoothness of splines
  prior(gamma(2, 0.35), class = "phi")           # Precision parameter for Beta regression
  )

# for 5 features
priors.cs <- c(
  prior(normal(0, 1), class = "b"),             # Regular slope priors for covariates
  prior(normal(-1, 0.1), class = "Intercept"),     # Intercept prior
  prior(exponential(8), class = "sds"),         # Smoothness of splines
  prior(gamma(2, 0.4), class = "phi")           # Precision parameter for Beta regression
)

# ------------------------------------------------------------------------------
# Prior Predictive Check
# ------------------------------------------------------------------------------
fit_prior_cs <- brm(
  formula = formula.cs,
  data = data,
  prior = priors.cs,
  family = "beta", 
  knots = knots_list,
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
  knots = knots_list,
  chains = 4, 
  cores = 4,
  iter = 4000,
  seed = 42
)

# Save Model
saveRDS(fit_cs, file = "Results/cs_model/cs_model.rds")

# ------------------------------------------------------------------------------
# Results Summary
# ------------------------------------------------------------------------------
sink("Results/cs_model/cs_summary.txt")  
print(summary(fit_cs))      
sink()                         

# ------------------------------------------------------------------------------
# Trace Plots
# ------------------------------------------------------------------------------
trace_plots_cs <- plot(
  fit_cs,
  ask = FALSE
)

for (i in seq_along(trace_plots_cs)) {
  ggsave(
    filename = paste0("Results/cs_model/cs_trace_plot_", i, ".png"),
    plot = trace_plots_cs[[i]]
  )
  
  saveRDS(
    object = trace_plots_cs[[i]], 
    file = paste0("Results/cs_model/R_data_files/cs_trace_plot_", i, ".rds")
  )
}

# ------------------------------------------------------------------------------
# Posterior Density Plot
# ------------------------------------------------------------------------------
set.seed(42)
cs_pd <- pp_check(fit_cs) +
  theme_bw(base_size = 22) 
ggsave(plot = cs_pd, filename = "Results/cs_model/cs_pd.png")
saveRDS(cs_pd, file = "Results/cs_model/R_data_files/cs_pd.rds")

# ------------------------------------------------------------------------------
# ELPD Calculation
# ------------------------------------------------------------------------------
sink("Results/cs_model/cs_elpd.txt")
cat("In-Sample ELPD:\n")
print(sum(colMeans(log_lik(fit_cs))))
cat("\nOut-Of-Sample ELPD:\n")
print(loo(fit_cs))
sink()   

# ------------------------------------------------------------------------------
# Residual Plot
# ------------------------------------------------------------------------------
y_pred_cs <- posterior_predict(fit_cs)
y_pred_mean_cs <- colMeans(y_pred_cs)
y_obs_cs <- fit_cs$data$y
residuals_cs <- y_obs_cs - y_pred_mean_cs
data_plot_cs <- data.frame(y_obs = y_obs_cs, residuals = residuals_cs)

cs_residual_plot <- ggplot(data_plot_cs, aes(x = y_obs, y = residuals)) +
  geom_point(alpha = 0.5, col = "blue") +
  geom_hline(yintercept = 0, color = "red", lwd = 1) +
  labs(
    x = "Normalized Violent Crimes per Capita",
    y = "Residuals"
  ) +
  theme_bw(base_size = 22)  

ggsave(plot = cs_residual_plot, filename = "Results/cs_model/cs_residual.png")
saveRDS(cs_residual_plot, file = "Results/cs_model/R_data_files/cs_residual.rds")

