library(brms)
library(ggplot2)
library(tidyr)
library(dplyr)
library(bayesplot)
# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------
source("data_upload.R")
source("feature_selections.R")
source("spatial_prep.R")

knots_latitude <- quantile(data$Latitude_scaled, probs = c(0.25, 0.5, 0.75))
knots_longitude <- quantile(data$Longitude_scaled, probs = c(0.25, 0.5, 0.75))

## Spline Bases : simple bases and knot bases
# 1) Longitude
Longitude_scaled_simple_0 <- rep(1, nrow(data))
Longitude_scaled_simple_1 <- data$Longitude_scaled
Longitude_scaled_simple_2 <- (data$Longitude_scaled)^2
Longitude_scaled_simple_3 <- (data$Longitude_scaled)^3

Longitude_scaled_knot_1 <- ((data$Longitude_scaled - knots_longitude[1])^3) * 
  ifelse(data$Longitude_scaled > knots_longitude[1], 1, 0)
Longitude_scaled_knot_2 <- ((data$Longitude_scaled - knots_longitude[2])^3) * 
  ifelse(data$Longitude_scaled > knots_longitude[2], 1, 0)
Longitude_scaled_knot_3 <- ((data$Longitude_scaled - knots_longitude[3])^3) * 
  ifelse(data$Longitude_scaled > knots_longitude[3], 1, 0)

# 2) Latitude
Latitude_scaled_simple_0 <- rep(1, nrow(data))
Latitude_scaled_simple_1 <- data$Latitude_scaled
Latitude_scaled_simple_2 <- (data$Latitude_scaled)^2
Latitude_scaled_simple_3 <- (data$Latitude_scaled)^3

Latitude_scaled_knot_1 <- ((data$Latitude_scaled - knots_latitude[1])^3) * 
  ifelse(data$Latitude_scaled > knots_latitude[1], 1, 0)
Latitude_scaled_knot_2 <- ((data$Latitude_scaled - knots_latitude[2])^3) * 
  ifelse(data$Latitude_scaled > knots_latitude[2], 1, 0)
Latitude_scaled_knot_3 <- ((data$Latitude_scaled - knots_latitude[3])^3) * 
  ifelse(data$Latitude_scaled > knots_latitude[3], 1, 0)


data.bases <- data.frame(Longitude_scaled_simple_0, Longitude_scaled_simple_1, 
                    Longitude_scaled_simple_2, Longitude_scaled_simple_3,
                    Longitude_scaled_knot_1, Longitude_scaled_knot_2, 
                    Longitude_scaled_knot_3,
                    Latitude_scaled_simple_0, Latitude_scaled_simple_1, 
                    Latitude_scaled_simple_2, Latitude_scaled_simple_3,
                    Latitude_scaled_knot_1, Latitude_scaled_knot_2, 
                    Latitude_scaled_knot_3,
                    ViolentCrimesPerPop = data$ViolentCrimesPerPop)


# ------------------------------------------------------------------------------
# Setup: Create Folders If Not Exist
# ------------------------------------------------------------------------------
dir.create("Results/bases_model", 
           recursive = TRUE, showWarnings = FALSE)
dir.create("Results/bases_model/R_data_files", 
           recursive = TRUE, showWarnings = FALSE)


formula.bases <- as.formula(ViolentCrimesPerPop ~ Longitude_scaled_simple_0 + 
    Longitude_scaled_simple_1 + Longitude_scaled_simple_2 + 
    Longitude_scaled_simple_3 +
    Longitude_scaled_knot_1 + Longitude_scaled_knot_2 + 
    Longitude_scaled_knot_3 +
    Latitude_scaled_simple_0 + Latitude_scaled_simple_1 + 
    Latitude_scaled_simple_2 + Latitude_scaled_simple_3 +
    Latitude_scaled_knot_1 + Latitude_scaled_knot_2 + 
    Latitude_scaled_knot_3
  )


# ------------------------------------------------------------------------------
# Prior Predictive Check
# ------------------------------------------------------------------------------

284136/29063000

priors.bases <- c(
  prior(normal(0, 4), class = "Intercept"),  
  prior(normal(0, 4), class = "b"),              
  prior(gamma(2, 0.1), class = "phi")
)


fit_prior_bases <- brm(
  formula = formula.bases,
  data = data.bases, 
  prior = priors.bases,
  family = "beta",
  sample_prior = "only",  
  chains = 4, 
  cores = 4,
  iter = 4000
)

set.seed(42)
ppc_plot_bases <- pp_check(fit_prior_bases, ndraws = 100) +
  theme_bw(base_size = 22) +
  labs(x = "Normalized Violent Crimes per Capita")

ggsave(plot = ppc_plot_bases, filename = "Results/bases_model/bases_ppc.png")
saveRDS(ppc_plot_bases, file = "Results/bases_model/R_data_files/bases_ppc.rds")




# ------------------------------------------------------------------------------
# Model Fitting
# ------------------------------------------------------------------------------
fit_bases <- brm(
  formula = formula.bases,
  data = data.bases,
  prior = priors.bases,
  family = "beta",
  chains = 4, 
  cores = 4,
  iter = 4000,
  seed = 42
)


pp_check(fit_bases)

# Save Model
saveRDS(fit_bases, file = "Results/bases_model/bases_model.rds")

# ------------------------------------------------------------------------------
# Results Summary
# ------------------------------------------------------------------------------
sink("Results/bases_model/bases_summary.txt")  
print(summary(fit_bases))      
sink()                         


# ------------------------------------------------------------------------------
# Trace Plots
# ------------------------------------------------------------------------------
trace_plots_bases <- plot(
  fit_bases,
  # pars = c("b_PctKids2Par", "b_PctImmigRec10", "b_PctPopUnderPov", 
  # "b_medFamInc", "b_racePctWhite", "Intercept"),
  ask = FALSE
)

for (i in seq_along(trace_plots_bases)) {
  ggsave(
    filename = paste0("Results/bases_model/bases_trace_plot_", i, ".png"),
    plot = trace_plots_bases[[i]]
  )
  
  saveRDS(
    object = trace_plots_bases[[i]], 
    file = paste0("Results/bases_model/R_data_files/bases_trace_plot_", i, ".rds")
  )
}

# ------------------------------------------------------------------------------
# Posterior Density Plot
# ------------------------------------------------------------------------------
bases_pd <- pp_check(fit_bases) +
  theme_bw(base_size = 22) 
ggsave(plot = bases_pd, filename = "Results/bases_model/bases_pd.png")
saveRDS(bases_pd, file = "Results/bases_model/R_data_files/bases_pd.rds")
