
# Us-state
pooled_model5 <- readRDS("C:/Users/rahul/OneDrive/Desktop/Notes/WS-24.25/ABDA/Project_ABDA/Bayesian-Analysis/Results/pooled_model/pooled_5/pooled_model5.rds")
pooled_model10 <- readRDS("C:/Users/rahul/OneDrive/Desktop/Notes/WS-24.25/ABDA/Project_ABDA/Bayesian-Analysis/Results/pooled_model/pooled_10/pooled_model10.rds")
zoi_model_5 <- readRDS("C:/Users/rahul/OneDrive/Desktop/Notes/WS-24.25/ABDA/Project_ABDA/Bayesian-Analysis/Results/pooled_model/zoi_model/zoi_model_5.rds")
hierarchical_model <- readRDS("C:/Users/rahul/OneDrive/Desktop/Notes/WS-24.25/ABDA/Project_ABDA/Bayesian-Analysis/Results/hierarchical_model/hierarchical_model.rds")

# California
cs_model <- readRDS("C:/Users/rahul/OneDrive/Desktop/Notes/WS-24.25/ABDA/Project_ABDA/Bayesian-Analysis/Results/cs_model/cs_model.rds")
gp_model <- readRDS("C:/Users/rahul/OneDrive/Desktop/Notes/WS-24.25/ABDA/Project_ABDA/Bayesian-Analysis/Results/gp_model/gp_model.rds")


library(loo)

dir.create("Results/model_comparison", 
           recursive = TRUE, showWarnings = FALSE)

# LOO
loo.pool5 <- loo(pooled_model5)
saveRDS(loo.pool5, file = "Results/model_comparison/loo.pool5.rds")

loo.pool10 <- loo(pooled_model10)
saveRDS(loo.pool10, file = "Results/model_comparison/loo.pool10.rds")

loo.poolzoi5 <- loo(zoi_model_5)
saveRDS(loo.poolzoi5, file = "Results/model_comparison/loo.poolzoi5.rds")

loo.hier5 <- loo(hierarchical_model)
saveRDS(loo.hier5, file = "Results/model_comparison/loo.hier5.rds")


loo.cs <- loo(cs_model)
saveRDS(loo.cs, file = "Results/model_comparison/loo.cs.rds")

loo.gp <- loo(gp_model)
saveRDS(loo.gp, file = "Results/model_comparison/loo.gp.rds")

# Compare models
sink("Results/model_comparison/loo.comp.us.state.txt")
(loo.comp.us.state <- loo_compare(loo.pool5, loo.pool10, loo.poolzoi5, loo.hier5))
sink()

sink("Results/model_comparison/loo.comp.cali.txt")
(loo.comp.cali <- loo_compare(loo.cs, loo.gp))
sink()




library(bridgesampling)

log.m.pool5 <- bridge_sampler(pooled_model5)
log.m.pool10 <- bridge_sampler(pooled_model10)
log.m.poolzoi5 <- bridge_sampler(zoi_model_5)
log.m.hier5 <- bridge_sampler(hierarchical_model)


post_prob(log.m.pool5, log.m.pool10, log.m.poolzoi5, log.m.hier5)


log.m.cs <- bridge_sampler(cs_model)
log.m.gp <- bridge_sampler(gp_model)

library(rstan)

extract(cs_model)
stancode(cs_model)


cs_model$save_pars
