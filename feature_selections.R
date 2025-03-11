library(ggcorrplot)
library(caret)
library(randomForest)
library(gam)


cor_matrix <- cor(data)
ggcorrplot(cor_matrix)


# If two variables exceed the correlation cutoff (e.g., 0.75), the function 
# removes the one with the highest mean absolute correlation with 
# all other variables.
remove.covariates.indices <- findCorrelation(cor_matrix, cutoff = 0.7)


# For regression, a linear line is fit between each pair of dependent variable 
# and independent variables, and the absolute value of the t-statistic for 
# the slope of the independent variable is used.


## feature importance ----------------------------------------------------------
#use roc_curve area as score
roc_imp <- filterVarImp(x = data[, !names(data) %in% c("ViolentCrimesPerPop")], 
                        y = data$ViolentCrimesPerPop)

#sort the score in decreasing order
roc_imp <- data.frame(cbind(variable = rownames(roc_imp), score = roc_imp[,1]))

roc_imp$score <- as.double(roc_imp$score)

# First 5 very important features (according to correlation)
five_features <-  roc_imp[order(roc_imp$score, decreasing = TRUE), ][1:5,]$variable

# First 10 very important features (according to correlation)
ten_features <-  roc_imp[order(roc_imp$score, decreasing = TRUE), ][1:10,]$variable

