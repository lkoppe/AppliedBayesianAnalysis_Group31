library(brms)
library(ggplot2)
library(tidyr)
library(dplyr)
library(bayesplot)

# ------------------------------------------------------------------------------
# Setup: Create Folders If Not Exist
# ------------------------------------------------------------------------------
dir.create("Results/gp_model", recursive = TRUE, showWarnings = FALSE)
dir.create("Results/gp_model/R_data_files", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------
source("data_upload.R")
source("feature_selections.R")
source("spatial_prep.R")

# Prepare data for the model
spatial.features <- c(five_features, "Longitude_scaled", 
                      "Latitude_scaled", "ViolentCrimesPerPop")

data <- data %>% select(all_of(c(spatial.features)))
names(data)
dim(data)

# ------------------------------------------------------------------------------
# Formula & Priors for Gaussian Process Model
# ------------------------------------------------------------------------------
formula.base2 <- as.formula(ViolentCrimesPerPop ~ gp(Latitude_scaled, Longitude_scaled))

formula.gp <- update(formula.base2, paste(". ~ . +", paste(five_features, collapse = " + ")))

priors.gp <- c(
  prior(normal(-1.0, 0.1), class = "Intercept"),  
  prior(normal(0, 2), class = "b"),              
  prior(gamma(2, 0.4), class = "phi"),           
  prior(exponential(8), class = "sdgp"),         
  prior(exponential(2), class = "lscale")
)


# ------------------------------------------------------------------------------
# Prior Predictive Check
# ------------------------------------------------------------------------------
fit_prior_gp <- brm(
  formula = formula.gp,
  data = data, 
  prior = priors.gp,
  family = "beta",
  sample_prior = "only",  
  chains = 4, 
  cores = 4,
  iter = 4000
)

ppc_plot_gp <- pp_check(fit_prior_gp, ndraws = 200) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = ppc_plot_gp, filename = "Results/gp_model/gp_ppc.png")
saveRDS(ppc_plot_gp, file = "Results/gp_model/R_data_files/gp_ppc.rds")

# ------------------------------------------------------------------------------
# Model Fitting
# ------------------------------------------------------------------------------
fit_gp <- brm(
  formula = formula.gp,
  data = data,
  prior = priors.gp,
  family = "beta",
  chains = 4, 
  cores = 4,
  iter = 4000,
  seed = 42
)

# Save Model
saveRDS(fit_gp, file = "Results/gp_model/gp_model.rds")

# ------------------------------------------------------------------------------
# Results Summary
# ------------------------------------------------------------------------------
sink("Results/gp_model/gp_summary.txt")  
print(summary(fit_gp))      
sink()                         

# ------------------------------------------------------------------------------
# Trace Plots
# ------------------------------------------------------------------------------
trace_plots_gp <- plot(
  fit_gp,
  pars = c("b_PctKids2Par", "b_PctImmigRec10", "b_PctPopUnderPov", "b_medFamInc", "b_racePctWhite", "Intercept"),
  ask = FALSE
)

for (i in seq_along(trace_plots_gp)) {
  ggsave(
    filename = paste0("Results/gp_model/gp_trace_plot_", i, ".png"),
    plot = trace_plots_gp[[i]]
  )
  
  saveRDS(
    object = trace_plots_gp[[i]], 
    file = paste0("Results/gp_model/R_data_files/gp_trace_plot_", i, ".rds")
  )
}

# ------------------------------------------------------------------------------
# Posterior Density Plot
# ------------------------------------------------------------------------------
gp_pd <- pp_check(fit_gp, ndraws = 100) +
  theme_bw(base_size = 22) 
ggsave(plot = gp_pd, filename = "Results/gp_model/gp_pd.png")
saveRDS(gp_pd, file = "Results/gp_model/R_data_files/gp_pd.rds")

# ------------------------------------------------------------------------------
# ELPD Calculation
# ------------------------------------------------------------------------------
sink("Results/gp_model/gp_elpd.txt")
cat("In-Sample ELPD:\n")
(elpd.in.gp <- sum(colMeans(log_lik(fit_gp, cores = 5)))) #177.1327
#  x  == (4 chains x 4000 iterations) x  

cat("\nOut-Of-Sample ELPD:\n")
(elpd.out.gp <- loo(fit_gp)) #146.7
sink()  

# ------------------------------------------------------------------------------
# Residual Plot
# ------------------------------------------------------------------------------
y_pred_gp <- posterior_predict(fit_gp)
y_pred_mean_gp <- colMeans(y_pred_gp)
y_obs_gp <- fit_gp$data$y
residuals_gp <- y_obs_gp - y_pred_mean_gp
data_plot_gp <- data.frame(y_obs = y_obs_gp, residuals = residuals_gp)

gp_residual_plot <- ggplot(data_plot_gp, aes(x = y_obs, y = residuals)) +
  geom_point(alpha = 0.5, col = "blue") +
  geom_hline(yintercept = 0, color = "red", lwd = 1) +
  labs(
    x = "Normalized Violent Crimes per Capita",
    y = "Residuals"
  ) +
  theme_bw(base_size = 22)  

ggsave(plot = gp_residual_plot, filename = "Results/gp_model/gp_residual.png")
saveRDS(gp_residual_plot, file = "Results/gp_model/R_data_files/gp_residual.rds")
