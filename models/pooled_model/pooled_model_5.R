
# ============== Pooled model with 5 features ==================================
library(brms)
library(ggplot2)
library(bayesplot)

# ------------------------------------------------------------------------------
# Setup: Create Folders If Not Exist
# ------------------------------------------------------------------------------
dir.create("Results/pooled_model/pooled_5", 
           recursive = TRUE, showWarnings = FALSE)
dir.create("Results/pooled_model/pooled_5/R_data_files_5", 
           recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------
source("data_upload.R")
source("feature_selections.R")

# ------------------------------------------------------------------------------
# Formula & Priors for Pooled Model
# ------------------------------------------------------------------------------

formula.pool5 <- as.formula(paste("ViolentCrimesPerPop ~", 
                                  paste(five_features, collapse = " + ")))

prior.pool5 <- 
  prior(normal(-1.4, 0.1), class = "Intercept") +
  prior(normal(0, 4), class = "b") + 
  prior(gamma(2, 0.1), class = "phi")

# ------------------------------------------------------------------------------
# Prior Predictive Check
# ------------------------------------------------------------------------------
fit_prior5 <- brm(
  formula = formula.pool5,
  data = data, 
  prior = prior.pool5,
  family = "beta",
  sample_prior = "only",  
  chains = 4,
  seed = 42,
  cores = 4,
  iter = 4000
)

set.seed(42)
ppc_plot5 <- pp_check(fit_prior5, ndraws = 30) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = ppc_plot5, filename = "Results/pooled_model/pooled_5/pooled_ppc5.png")
saveRDS(ppc_plot5, file = "Results/pooled_model/pooled_5/R_data_files_5/pooled_ppc5.rds")

# ------------------------------------------------------------------------------
# Model Fitting
# ------------------------------------------------------------------------------
brm.pool5 <- brm(
  formula = formula.pool5, 
  data = data,
  prior = prior.pool5, 
  family = "beta",
  chains = 4,
  seed = 42,
  cores = 4,
  iter = 4000
)

# Save Model
saveRDS(brm.pool5, file = "Results/pooled_model/pooled_5/pooled_model5.rds") 

# ------------------------------------------------------------------------------
# Results Summary
# ------------------------------------------------------------------------------
sink("Results/pooled_model/pooled_5/pooled_summary5.txt")  
print(summary(brm.pool5))      
sink()                         

# ------------------------------------------------------------------------------
# Trace Plots
# ------------------------------------------------------------------------------
trace_plots5 <- plot(
  brm.pool5,
  #pars = c("Intercept", "PctKids2Par", "PctIlleg", "PctFam2Par", "racePctWhite", "PctYoungKids2Par"),
  ask = FALSE
)

for (i in seq_along(trace_plots5)) {
  ggsave(
    filename = paste0("Results/pooled_model/pooled_5/pooled_trace_plot5_", i, ".png"),
    plot = trace_plots5[[i]]
  )
  
  saveRDS(
    object = trace_plots5[[i]], 
    file = paste0("Results/pooled_model/pooled_5/R_data_files_5/pooled_trace_plot5_", i, ".rds")
  )
}

# ------------------------------------------------------------------------------
# Posterior Density Plot
# ------------------------------------------------------------------------------
set.seed(42)


# ? ? ? ? ?  ?? ? ?  Problem with title fitting on the plot
pooled_pd5 <- pp_check(brm.pool5, ndraws = 30) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = pooled_pd5, filename = "Results/pooled_model/pooled_5/pooled_pd5.png")
saveRDS(pooled_pd5, file = "Results/pooled_model/pooled_5/R_data_files_5/pooled_pd5.rds")

# ------------------------------------------------------------------------------
# ELPD Calculation
# ------------------------------------------------------------------------------
sink("Results/pooled_model/pooled_5/pooled_elpd5.txt")
cat("In-Sample ELPD:\n")
(elpd.in.pool5 <- sum(colMeans(log_lik(brm.pool5, cores = 5)))) #1473.79
# 8000 x 1994 == (4 chains x 2000 iterations) x 1994

cat("\nOut-Of-Sample ELPD:\n")
(elpd.out.pool5 <- loo(brm.pool5)) #1461.6
sink()   

# ------------------------------------------------------------------------------
# Residual Plot
# ------------------------------------------------------------------------------
y_pred_brm5 <- posterior_predict(brm.pool5)
y_pred_mean5 <- colMeans(y_pred_brm5)
y_obs <- brm.pool5$data$ViolentCrimesPerPop
residuals_brm5 <- y_obs - y_pred_mean5
data_plot5 <- data.frame(y_obs = y_obs, residuals = residuals_brm5)

pooled_residual_plot_5 <- ggplot(data_plot5, aes(x = y_obs, y = residuals)) +
  geom_point(alpha = 0.5, col = "purple") +
  geom_hline(yintercept = 0, color = "red", lwd = 1) +
  labs(
    x = "Normalized Violent Crimes per Capita",
    y = "Residuals"
  ) +
  ggtitle("Pooled Model with 5 features") +
  theme_bw(base_size = 22)  

ggsave(plot = pooled_residual_plot_5, filename = "Results/pooled_model/pooled_5/pooled_residual5.png")
saveRDS(pooled_residual_plot_5, file = "Results/pooled_model/pooled_5/R_data_files_5/pooled_residual5.rds")
