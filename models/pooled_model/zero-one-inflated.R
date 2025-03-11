# ============== Pooled model with 5 features zoi model ========================
# ------------------------------------------------------------------------------
# Load Data and Libraries
# ------------------------------------------------------------------------------
source("data_upload_with_zeros.R")
source("feature_selections.R")

library(brms)
library(ggplot2)
library(bayesplot)

# ------------------------------------------------------------------------------
# Setup: Create Folders If Not Exist
# ------------------------------------------------------------------------------
dir.create("Results/pooled_model/zoi_model", 
           recursive = TRUE, showWarnings = FALSE)
dir.create("Results/pooled_model/zoi_model/R_data_files_zoi", 
           recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Formula & Priors for Zero-One Inflated Beta Model
# ------------------------------------------------------------------------------
prior.pool.zoi5 <- prior(normal(-1.4, 0.1), class = "Intercept") +
  prior(normal(0, 4), class = "b") +
  prior(normal(0, 4), class = "b", dpar = "phi") +
  prior(normal(0, 4), class = "b", dpar = "zoi") +
  prior(normal(0, 4), class = "b", dpar = "coi")

formula.pool.zoi5 <- bf(
  paste("ViolentCrimesPerPop ~", paste(five_features, collapse = " + ")),  
  paste("phi ~", paste(five_features, collapse = " + ")), 
  paste("zoi ~", paste(five_features, collapse = " + ")),
  paste("coi ~", paste(five_features, collapse = " + "))
)


# ------------------------------------------------------------------------------
# Prior Predictive Check
# ------------------------------------------------------------------------------
fit_prior01 <- brm(
  formula = formula.pool.zoi5,
  data = data, 
  prior = prior.pool.zoi5,
  family = zero_one_inflated_beta(),
  sample_prior = "only",  
  chains = 4,
  seed = 42,
  cores = 4,
  iter = 12000
)

set.seed(42)
ppc_plot01 <- pp_check(fit_prior01, ndraws = 30) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = ppc_plot01, filename = "Results/pooled_model/zoi_model/zoi_ppc.png")
saveRDS(ppc_plot01, file = "Results/pooled_model/zoi_model/R_data_files_zoi/zoi_ppc.rds")

# ------------------------------------------------------------------------------
# Model Fitting: Pool 5
# ------------------------------------------------------------------------------
brms.pool.zoi5 <- brm(
  formula = formula.pool.zoi5,
  data = data,
  family = zero_one_inflated_beta(),
  prior = prior.pool.zoi5,
  chains = 4, 
  iter = 8000,
  seed = 42,
  control = list(max_treedepth = 12),
  cores = 4
)

# Save Model
saveRDS(brms.pool.zoi5, file = "Results/pooled_model/zoi_model/zoi_model_5.rds")

# ------------------------------------------------------------------------------
# Results Summary
# ------------------------------------------------------------------------------
sink("Results/pooled_model/zoi_model/zoi_summary_5.txt")
print(summary(brms.pool.zoi5))
sink()

# ------------------------------------------------------------------------------
# Trace Plots
# ------------------------------------------------------------------------------
trace_plots.zoi5 <- plot(brms.pool.zoi5, ask = FALSE)

for (i in seq_along(trace_plots.zoi5)) {
  ggsave(filename = paste0("Results/pooled_model/zoi_model/zoi_trace_plot_5_", i, ".png"),
         plot = trace_plots.zoi5[[i]])
  saveRDS(object = trace_plots.zoi5[[i]],
          file = paste0("Results/pooled_model/zoi_model/R_data_files_zoi/zoi_trace_plot_5_", i, ".rds"))
}

# ------------------------------------------------------------------------------
# Posterior Density Plot
# ------------------------------------------------------------------------------
set.seed(42)
pooled.zoi_pd5 <- pp_check(brms.pool.zoi5, ndraws = 30) +
  theme_bw(base_size = 22)

ggsave(plot = pooled.zoi_pd5, filename = "Results/pooled_model/zoi_model/zoi_pd_5.png")
saveRDS(pooled.zoi_pd5, file = "Results/pooled_model/zoi_model/R_data_files_zoi/zoi_pd_5.rds")

# ------------------------------------------------------------------------------
# ELPD Calculation
# ------------------------------------------------------------------------------
sink("Results/pooled_model/zoi_model/zoi_elpd5.txt")
cat("In-Sample ELPD:\n")
print(sum(colMeans(log_lik(brms.pool.zoi5))))
cat("\nOut-Of-Sample ELPD:\n")
print(loo(brms.pool.zoi5))
sink()

# ------------------------------------------------------------------------------
# Residual Plot
# ------------------------------------------------------------------------------
y_pred.zoi_5 <- posterior_predict(brms.pool.zoi5)
# y_pred_18 <- posterior_predict(brms_pool_18)

residuals.zoi_5 <- brms.pool.zoi5$data$ViolentCrimesPerPop - colMeans(y_pred.zoi_5)
# residuals_18 <- brms_pool_18$data$ViolentCrimesPerPop - colMeans(y_pred_18)

data_plot.zoi_5 <- data.frame(y_obs = brms.pool.zoi5$data$ViolentCrimesPerPop, residuals = residuals.zoi_5)
# data_plot_18 <- data.frame(y_obs = brms_pool_18$data$ViolentCrimesPerPop, residuals = residuals_18)

zoi_residual_plot_5 <- ggplot(data_plot.zoi_5, aes(x = y_obs, y = residuals)) +
  geom_point(alpha = 0.5, col = "blue") + 
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Observed", 
       y = "Residuals") + 
  ggtitle("Pooled 0-1 Inflated Beta Model with 5 features") +
  theme_bw(base_size = 22)

# zoi_residual_plot_18 <- ggplot(data_plot_18, aes(x = y_obs, y = residuals)) +
#   geom_point(alpha = 0.5, col = "purple") + geom_hline(yintercept = 0, color = "red") +
#   labs(x = "Observed", y = "Residuals") + theme_bw(base_size = 22)

ggsave(plot = zoi_residual_plot_5, filename = "Results/pooled_model/zoi_model/zoi_residual_5.png")
# ggsave(plot = zoi_residual_plot_18, filename = "Results/pooled_model/zoi_model/zoi_residual_18.png")