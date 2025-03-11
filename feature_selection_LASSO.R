
covariates <- names(data)[-which(names(data) == "ViolentCrimesPerPop")]
formula <- as.formula(paste("ViolentCrimesPerPop ~", paste(covariates, collapse = " + ")))


### logit(response) then GLM with identity link or Gaussian --------------
logit_y <- log(data$ViolentCrimesPerPop/(1 - data$ViolentCrimesPerPop))

plot(density(logit_y)) ## pretty normal

set.seed(1234)
lasso.model <- cv.glmnet(as.matrix(data[ , -which(names(data) == "ViolentCrimesPerPop")]), 
                         logit_y, alpha = 1, family = "gaussian")

# lasso on full data set with best lambda
lasso.model <- glmnet(as.matrix(data[ , -which(names(data) == "ViolentCrimesPerPop")]), 
                      logit_y, alpha = 1, lambda = lasso.model$lambda.min)


lasso.coef <-  coef(lasso.model) #19

coef.df <- as.data.frame(as.matrix(lasso.coef))
coef.df$feature_name <- rownames(coef.df)

colnames(coef.df) <- c("coef_val", "coeff_name")

coef.df.non.zero <- coef.df[coef.df$coef_val != 0, ]

reasonable.variable <- rownames(coef.df.non.zero) # <<<<<<< IMP

reasonable.variable <- reasonable.variable[reasonable.variable != "(Intercept)"]


## Transforming back to orginal scales ===================

# Predict using the fitted Lasso model
predicted.log.y <- predict(lasso.model, 
                           newx = as.matrix(data[ , -which(names(data) == "ViolentCrimesPerPop")]))

# Transform predictions back to the original scale (inverse logit)
predicted.y <- exp(predicted.log.y) / (1 + exp(predicted.log.y))


plot(density(predicted.y))
lines(density(data$ViolentCrimesPerPop), col = "red")
reasonable.variable


rm(list = setdiff(ls(), c("reasonable.variable", "data.x", "data")) )

# adding state new variable again in out data set
data$statenew <- data.x$statenew

