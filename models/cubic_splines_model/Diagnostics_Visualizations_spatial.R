library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

############################## linear #########################################
# Extract posterior predictive samples
posterior_samples_lin3 <- rstan::extract(posterior_mod_spatial_lin3)
posterior_ypred_lin3 <- na.omit(as.matrix(posterior_samples_lin3$y_pred))


# Visualization
# ==============================================================================

# Compute summary statistics for predictions
mean_ypred <- apply(posterior_ypred_lin3, 2, mean)        
lower_ypred <- apply(posterior_ypred, 2, quantile, probs = 0.025)  
upper_ypred <- apply(posterior_ypred, 2, quantile, probs = 0.975) 

# Prepare data for plotting
plot_data <- data.frame(
  Observed = posterior_data$y,
  Predicted = mean_ypred,
  Residual = posterior_data$y - mean_ypred,
  PctKids2Par = posterior_data$x1,
  PctImmigRec10 = posterior_data$x2,
  PctPopUnderPov = posterior_data$x3,
  medFamInc = posterior_data$x4,
  racePctWhite = posterior_data$x5
) %>%
  pivot_longer(
    cols = PctKids2Par:racePctWhite,
    names_to = "Predictor",
    values_to = "Predictor_Value"
  )

# Compare observed vs predicted
bayesplot::ppc_dens_overlay(
  y = data$ViolentCrimesPerPop,
  yrep = posterior_ypred[sample(1:nrow(posterior_ypred), 20),]
)

# Scatter plot of Observed vs Predicted
ggplot(plot_data, aes(x = Observed, y = Residual)) +
  geom_point(alpha = 0.5, col = "purple") +
  geom_hline(yintercept = 0, color = "red", lwd = 1) +
  labs(
    x = "Violent Crimes per 100k Population",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 22)

# Facet plot of Predicted vs Predictors with Observed values
ggplot(plot_data, aes(x = Predictor_Value)) +
  geom_point(aes(y = Observed, color = "Observed"), alpha = 0.7, size = 2) +
  geom_point(aes(y = Predicted, color = "Predicted"), alpha = 0.3, size = 2) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), alpha = 0.1, width = 0) +
  facet_wrap(~ Predictor) +
  labs(
    title = "Observed vs Predicted with Uncertainty",
    x = "Predictor Value",
    y = "Response (y)",
    color = "Legend"
  ) +
  theme_minimal()

#######################################################
# Visualization of the effects
# separately first
# Generate a grid of longitude and latitude
lon_grid <- seq(min(data$Longitude_scaled), max(data$Longitude_scaled), length.out = 100)
lat_grid <- seq(min(data$Latitude_scaled), max(data$Latitude_scaled), length.out = 100)
grid <- expand.grid(lon = lon_grid, lat = lat_grid)

# Access spline coefficients
# Extract the posterior samples for beta_lon
posterior_samples_lin4 <- rstan::extract(posterior_mod_spatial_lin4)
beta_lon_samples <- posterior_samples_lin4$beta_lon  # Extract beta_lon
beta_lat_samples <- posterior_samples_lin4$beta_lat # Extract beta_lat

# Mean of each coefficient
beta_lon_mean <- colMeans(beta_lon_samples)
beta_lat_mean <- colMeans(beta_lat_samples)

# Compute spline effects
# Create a data frame for the spline effect
lat_effect_data <- data.frame(
  Latitude = lat_grid,
  Effect = beta_lat_mean[1] * lat_grid +
    beta_lat_mean[2] * pmax(0, lat_grid - 0.1573) +
    beta_lat_mean[3] * pmax(0, lat_grid - 0.1911) +
    beta_lat_mean[4] * pmax(0, lat_grid - 0.6022)
)

lon_effect_data <- data.frame(
  Longitude = lon_grid,
  Effect = beta_lon_mean[1] * lon_grid +
    beta_lon_mean[2] * pmax(0, lon_grid - 0.2628) +
    beta_lon_mean[3] * pmax(0, lon_grid - 0.6677) + 
    beta_lon_mean[4] * pmax(0, lon_grid - 0.7206)
)

# Create a data frame for the observed data
observed_data <- data.frame(
  Latitude = posterior_data$lat,  
  Longitude = posterior_data$lon,
  Observed = posterior_data$y
)

# Plot the latitude effect with data points
ggplot() +
  geom_line(data = lat_effect_data, aes(x = Latitude, y = Effect), color = "blue", size = 1) +
  geom_point(data = observed_data, aes(x = Latitude, y = Observed), color = "red", alpha = 0.6) +
  labs(
    x = "Latitude",
    y = "Effect / Observed"
  ) +
  theme_minimal(base_size = 22)

ggplot() +
  geom_line(data = lon_effect_data, aes(x = Longitude, y = Effect), color = "blue", size = 1) +
  geom_point(data = observed_data, aes(x = Longitude, y = Observed), color = "red", alpha = 0.6) +
  labs(
    x = "Longitude",
    y = "Effect / Observed"
  ) +
  theme_minimal(base_size = 22)


# Combine effects
grid$Effect <- with(grid, 
                    beta_lat_mean[1] * lat_grid +
                      beta_lat_mean[2] * pmax(0, lat_grid - 0.3) +
                      beta_lat_mean[3] * pmax(0, lat_grid - 0.6) +
                      beta_lon_mean[1] * lon_grid +
                      beta_lon_mean[2] * pmax(0, lon_grid - 0.3) +
                      beta_lon_mean[3] * pmax(0, lon_grid - 0.6)
)

effect_matrix <- matrix(grid$Effect, nrow = length(lat_grid), ncol = length(lon_grid))
# Plot with plotly
plot_ly() %>%
  add_surface(
    x = lon_grid,  # Longitude grid
    y = lat_grid,  # Latitude grid
    z = effect_matrix,
    colorscale = "Viridis",
    showscale = TRUE,
    name = "Combined Effect"
  ) %>%
  layout(
    title = "Modeled Combined Effect of Longitude and Latitude",
    scene = list(
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(title = "Effect"),
      aspectmode = "cube"
    )
  )

# Visualization of the fitted data
posterior_fitted <- posterior_samples_lin3$mu  # Extract posterior samples for mu
mean_fitted <- apply(posterior_fitted, 2, mean)  # Take the mean across iterations

# Create a data frame for observed lat/lon and fitted effects
fitted_data <- data.frame(
  Latitude = posterior_data$lat,
  Longitude = posterior_data$lon,
  Effect = mean_fitted  # Fitted response from the model
)

# Plot with plotly
plot_ly() %>%
  # Add the surface for the fitted effect
  add_mesh(
    x = fitted_data$Longitude,
    y = fitted_data$Latitude,
    z = fitted_data$Effect,
    intensity = fitted_data$Effect,
    colorscale = "Viridis", showscale = TRUE,
    name = "Fitted Surface"
  ) %>%
  # Add scatter points for observed data
  add_markers(
    x = posterior_data$lon,
    y = posterior_data$lat,
    z = posterior_data$y,
    marker = list(size = 4, color = "red", opacity = 0.7),
    name = "Observed Data"
  ) %>%
  # Layout for better visualization
  layout(
    title = "Fitted Surface with Observed Data Points",
    scene = list(
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(title = "Response"),
      aspectmode = "cube"
    )
  )

####################################### cubic ################################
posterior_samples_cubic3 <- rstan::extract(posterior_mod_spatial_cubic3)
beta_lon_samples <- posterior_samples_cubic3$beta_lon  # Extract beta_lon
beta_lat_samples <- posterior_samples_cubic3$beta_lat # Extract beta_lat

# Mean of each coefficient
beta_lon_mean <- colMeans(beta_lon_samples)
beta_lat_mean <- colMeans(beta_lat_samples)

grid$Effect <- with(grid, 
                    beta_lat_mean[1] * lat_grid +
                      beta_lat_mean[2] * pmax(0, lat_grid - 0.3) +
                      beta_lat_mean[3] * pmax(0, lat_grid - 0.6) +
                      beta_lon_mean[1] * lon_grid +
                      beta_lon_mean[2] * pmax(0, lon_grid - 0.3) +
                      beta_lon_mean[3] * pmax(0, lon_grid - 0.6)
)

# Create a data frame for the observed data
observed_data <- data.frame(
  Latitude = posterior_data$lat,  
  Longitude = posterior_data$lon,
  Observed = posterior_data$y
)

posterior_fitted <- posterior_samples_cubic3$mu  # Extract posterior samples for mu
mean_fitted <- apply(posterior_fitted, 2, mean)  # Take the mean across iterations

# Create a data frame for observed lat/lon and fitted effects
fitted_data <- data.frame(
  Latitude = posterior_data$lat,
  Longitude = posterior_data$lon,
  Effect = mean_fitted  # Fitted response from the model
)

# Plot with plotly
plot_ly() %>%
  # Add the surface for the fitted effect
  add_mesh(
    x = fitted_data$Longitude,
    y = fitted_data$Latitude,
    z = fitted_data$Effect,
    intensity = fitted_data$Effect,
    colorscale = "Viridis", showscale = TRUE,
    name = "Fitted Surface"
  ) %>%
  # Add scatter points for observed data
  add_markers(
    x = posterior_data$lon,
    y = posterior_data$lat,
    z = posterior_data$y,
    marker = list(size = 4, color = "red", opacity = 0.7),
    name = "Observed Data"
  ) %>%
  # Layout for better visualization
  layout(
    title = "Fitted Surface with Observed Data Points",
    scene = list(
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(title = "Response"),
      aspectmode = "cube"
    )
  )
