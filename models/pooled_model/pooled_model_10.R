
# ============== Pooled model with 10 features ==================================

# ------------------------------------------------------------------------------
# Setup: Create Folders If Not Exist
# ------------------------------------------------------------------------------
dir.create("Results/pooled_model/pooled_10", 
           recursive = TRUE, showWarnings = FALSE)
dir.create("Results/pooled_model/pooled_10/R_data_files_10", 
           recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Formula & Priors for Pooled Model
# ------------------------------------------------------------------------------
prior.pool10 <- 
  prior(normal(-1.4, 0.1), class = "Intercept") +
  prior(normal(0, 4), class = "b") + 
  prior(gamma(2, 0.1), class = "phi")

formula.pool10 <- as.formula(paste("ViolentCrimesPerPop ~", 
                                  paste(ten_features, collapse = " + ")))

# ------------------------------------------------------------------------------
# Prior Predictive Check
# ------------------------------------------------------------------------------
fit_prior10 <- brm(
  formula = formula.pool10,
  data = data, 
  prior = prior.pool10,
  family = "beta",
  sample_prior = "only",  
  chains = 4,
  seed = 42,
  cores = 4,
  iter = 4000
)

set.seed(42)
ppc_plot10 <- pp_check(fit_prior10, ndraws = 30) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = ppc_plot10, filename = "Results/pooled_model/pooled_10/pooled_ppc10.png")
saveRDS(ppc_plot10, file = "Results/pooled_model/pooled_10/R_data_files_10/pooled_ppc10.rds")

# ------------------------------------------------------------------------------
# Model Fitting
# ------------------------------------------------------------------------------
brm.pool10 <- brm(
  formula = formula.pool10, 
  data = data,
  prior = prior.pool10, 
  family = "beta",
  chains = 4,
  seed = 42,
  cores = 4,
  iter = 4000
)

# Save Model
saveRDS(brm.pool10, file = "Results/pooled_model/pooled_10/pooled_model10.rds") 

# ------------------------------------------------------------------------------
# Results Summary
# ------------------------------------------------------------------------------
sink("Results/pooled_model/pooled_10/pooled_summary10.txt")  
print(summary(brm.pool10))      
sink()                         

# ------------------------------------------------------------------------------
# Trace Plots
# ------------------------------------------------------------------------------
trace_plots10 <- plot(
  brm.pool10,
  #pars = ten_features,
  ask = FALSE
)

for (i in seq_along(trace_plots10)) {
  ggsave(
    filename = paste0("Results/pooled_model/pooled_10/pooled_trace_plot10_", i, ".png"),
    plot = trace_plots10[[i]]
  )
  
  saveRDS(
    object = trace_plots10[[i]], 
    file = paste0("Results/pooled_model/pooled_10/R_data_files_10/pooled_trace_plot10_", i, ".rds")
  )
}

# ------------------------------------------------------------------------------
# Posterior Density Plot
# ------------------------------------------------------------------------------
set.seed(42)


# ? ? ? ? ?  ?? ? ?  Problem with title fitting on the plot
pooled_pd10 <- pp_check(brm.pool10, ndraws = 30) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = pooled_pd10, filename = "Results/pooled_model/pooled_10/pooled_pd10.png")
saveRDS(pooled_pd10, file = "Results/pooled_model/pooled_10/R_data_files_10/pooled_pd10.rds")

# ------------------------------------------------------------------------------
# ELPD Calculation
# ------------------------------------------------------------------------------
sink("Results/pooled_model/pooled_10/pooled_elpd10.txt")
cat("In-Sample ELPD:\n")
print(sum(colMeans(log_lik(brm.pool10))))
cat("\nOut-Of-Sample ELPD:\n")
print(loo(brm.pool10))
sink()   

# ------------------------------------------------------------------------------
# Residual Plot
# ------------------------------------------------------------------------------
y_pred_brm10 <- posterior_predict(brm.pool10)
y_pred_mean10 <- colMeans(y_pred_brm10)
y_obs <- brm.pool10$data$ViolentCrimesPerPop
residuals_brm10 <- y_obs - y_pred_mean10
data_plot10 <- data.frame(y_obs = y_obs, residuals = residuals_brm10)

pooled_residual_plot_10 <- ggplot(data_plot10, aes(x = y_obs, y = residuals)) +
  geom_point(alpha = 0.5, col = "purple") +
  geom_hline(yintercept = 0, color = "red", lwd = 1) +
  labs(
    x = "Normalized Violent Crimes per Capita",
    y = "Residuals"
  ) +
  ggtitle("Pooled Model with 10 features") +
  theme_bw(base_size = 22)  

ggsave(plot = pooled_residual_plot_10, filename = "Results/pooled_model/pooled_10/pooled_residual10.png")
saveRDS(pooled_residual_plot_10, file = "Results/pooled_model/pooled_10/R_data_files_10/pooled_residual10.rds")
