# Compare models

library(loo)

lin <- as.matrix(posterior_mod_spatial_lin3, pars = "y_pred")
loo(lin)
# elpd 106.5
lin5 <- as.matrix(posterior_mod_spatial_lin5, pars = "y_pred")
loo(lin5)
# elpd 105.5
lin10 <- as.matrix(posterior_mod_spatial_lin10, pars = "y_pred")
loo(lin10)
# elpd 105.2
# more knots seem to be worse

loo(fit)
# elpd 114.8

# using quartiles
lin4 <- as.matrix(posterior_mod_spatial_lin4, pars = "y_pred")
loo(lin4)
# 106.4

cubic <- as.matrix(posterior_mod_spatial_cubic3, pars = "y_pred")
loo(cubic)
# elpd 106.5 (almost exactly the same)

# compare this to a model without latitude and longitude effects
non <- as.matrix(posterior_mod_nonspatial, pars = "y_pred")
loo(non)
# elpd 106.3 (the entire spline thing is pretty much useless)

pool <- as.matrix(posterior_mod_pool, pars = "y_pred")
loo(pool)
# elpd 575.0

hier <- as.matrix(posterior_mod_hier, pars = "ypred")
loo(hier)
# elpd 543.9



library(bridgesampling)
hier_pool_b <- bridge_sampler(posterior_mod_pool)
hier_stan_b <- bridge_sampler(posterior_mod_hier)
post_prob(hier_pool_b, hier_stan_b)

stan_non_b <- bridge_sampler(posterior_mod_nonspatial)
stan_lin3_b <- bridge_sampler(posterior_mod_spatial_lin3)
round(post_prob(stan_non_b, stan_lin3_b), digits = 5)
