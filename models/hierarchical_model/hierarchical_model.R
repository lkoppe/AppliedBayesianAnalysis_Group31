# ============== Hierarchical model with 10 features ===========================
library(brms)
library(ggplot2)
library(bayesplot)

# ------------------------------------------------------------------------------
# Setup: Create Folders If Not Exist
# ------------------------------------------------------------------------------
dir.create("Results/hierarchical_model", 
           recursive = TRUE, showWarnings = FALSE)
dir.create("Results/hierarchical_model/R_data_files",
           recursive = TRUE, showWarnings = FALSE)

# # ------------------------------------------------------------------------------
# # Load Data
# # ------------------------------------------------------------------------------
source("data_upload.R")
source("feature_selections.R")

# ------------------------------------------------------------------------------
# Formula & Priors for Hierarchical Model
# ------------------------------------------------------------------------------
prior.hier <- 
  prior(normal(-1.4, 0.1), class = "Intercept") +  
  prior(normal(0, 4), class = "b") +  
  prior(exponential(3), class = "sd", group = "statenew") +  
  prior(gamma(2, 0.1), class = "phi")

formula.hier <- as.formula(
  paste("ViolentCrimesPerPop ~", 
        paste(five_features, collapse = " + "),
        " + (1 +", 
        paste(five_features, collapse = " + "), 
        "| statenew )")
  )

# ------------------------------------------------------------------------------
# Prior Predictive Check
# ------------------------------------------------------------------------------
fit_prior_hier <- brm(
  formula = formula.hier,
  data = data.x, 
  prior = prior.hier,
  family = "beta",
  sample_prior = "only",  
  chains = 4, 
  iter = 4000
)

set.seed(42)
ppc_plot_hier <- pp_check(fit_prior_hier, ndraws = 30) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = ppc_plot_hier, filename = "Results/hierarchical_model/hierarchical_ppc.png")
saveRDS(ppc_plot_hier, file = "Results/hierarchical_model/R_data_files/hierarchical_ppc.rds")

# ------------------------------------------------------------------------------
# Model Fitting
# ------------------------------------------------------------------------------
brm.hier <- brm(
  formula = formula.hier, 
  data = data.x, 
  prior = prior.hier,
  family = "beta", 
  chains = 4,
  seed = 42,
  save_pars = save_pars(all = TRUE),
  cores = 4,
  iter = 4000
)

# Save Model
saveRDS(brm.hier, file = "Results/hierarchical_model/hierarchical_model.rds")

# ------------------------------------------------------------------------------
# Results Summary
# ------------------------------------------------------------------------------
sink("Results/hierarchical_model/hierarchical_summary.txt")  
print(summary(brm.hier))      
sink()                         

# ------------------------------------------------------------------------------
# Trace Plots
# ------------------------------------------------------------------------------
trace_plots_hier <- plot(
  brm.hier,
  #pars = five_features,
  ask = FALSE
)

for (i in seq_along(trace_plots_hier)) {
  ggsave(
    filename = paste0("Results/hierarchical_model/hierarchical_trace_plot_", i, ".png"),
    plot = trace_plots_hier[[i]]
  )
  
  saveRDS(
    object = trace_plots_hier[[i]], 
    file = paste0("Results/hierarchical_model/R_data_files/hierarchical_trace_plot_", i, ".rds")
  )
}

# ------------------------------------------------------------------------------
# Posterior Density Plot
# ------------------------------------------------------------------------------
set.seed(42)
hierarchical_pd <- pp_check(brm.hier)
ggsave(plot = hierarchical_pd, filename = "Results/hierarchical_model/hierarchical_pd.png")
saveRDS(hierarchical_pd, file = "Results/hierarchical_model/R_data_files/hierarchical_pd.rds")

# ------------------------------------------------------------------------------
# ELPD Calculation
# ------------------------------------------------------------------------------
sink("Results/hierarchical_model/hierarchical_elpd.txt")
cat("In-Sample ELPD:\n")
(elpd.in.hier <- sum(colMeans(log_lik(brm.hier, cores = 5)))) #1735.366
# 8000 x 1994 == (4 chains x 2000 iterations) x 1994

cat("\nOut-Of-Sample ELPD:\n")
(elpd.out.hier <- loo(brm.hier)) #1645.4
sink()

# ------------------------------------------------------------------------------
# Residual Plot
# ------------------------------------------------------------------------------
y_pred_brm_hier <- posterior_predict(brm.hier)
y_pred_mean_hier <- colMeans(y_pred_brm_hier)
y_obs_hier <- brm.hier$data$ViolentCrimesPerPop
residuals_brm_hier <- y_obs_hier - y_pred_mean_hier
data_plot_hier <- data.frame(y_obs = y_obs_hier, residuals = residuals_brm_hier)

hierarchical_residual_plot <- ggplot(data_plot_hier, aes(x = y_obs, y = residuals)) +
  geom_point(alpha = 0.5, col = "purple") +
  geom_hline(yintercept = 0, color = "red", lwd = 1) +
  labs(
    x = "Normalized Violent Crimes per Capita",
    y = "Residuals",
  ) +
  theme_bw(base_size = 22)  

ggsave(plot = hierarchical_residual_plot, filename = "Results/hierarchical_model/hierarchical_residual.png")
saveRDS(hierarchical_residual_plot, file = "Results/hierarchical_model/R_data_files/hierarchical_residual.rds")
