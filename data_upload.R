
rm(list = ls())

library(glmnet)
library(Matrix)
library(dplyr)

## 1) Extracting the attributes' name ==================================== 
attribute_names <- readLines("data/communities.names") # char string 522

attribute_names <- gsub(pattern = "@attribute ", replacement = "", 
                        # grepl: returns logical values for matches
                        # extract lines starting with @attribute only 
                        attribute_names[grepl("@attribute", attribute_names)])

## split the string wrt the blank space and use 1st element
attribute_names <- sapply(strsplit(attribute_names, " "), FUN = `[`, 1)

data <- read.csv("data/communities.data", 
                 header = FALSE, 
                 na.strings = "?", # ? as a NA
                 col.names = attribute_names # assign column names 
) #  1994 x 128



# y : beta response RV -------------------------

#> For beta regression, our target variable needs to be in the open 
#> interval (0,1), but we have actual zeros and ones.
#> So, replace values less than 1e-6, with 1e-6 as lower bound
#> and, values greater than 1 - 1e-6  with 1 - 1e-6 as upper bound
data$ViolentCrimesPerPop <- pmin(pmax(data$ViolentCrimesPerPop, 1e-6), 1 - 1e-6)

# Setting new state ids from 1 to 46 states ----------------
# but keeping the original state names as it in the form of name of vector

#> Stan doesn't like how there are only 46 unique state numbers but 
#> the maximum number is 56. We need to reassign continuous numbers.

unique_states <- sort(unique(data$state)) # 46
new_state_ids <- setNames(seq_along(unique_states), unique_states) 

# adding new column of new state 
# access the vector through the "names"
# the return value is then a new state values
data$statenew <- new_state_ids[as.character(data$state)]



##%%%%%%%%%%%%%%%%%%%%%%%%%%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## removing NA coulmns
data <- data[ , colSums(is.na(data)) == 0]


## keeping the original data set as a copy
# but will be replaced with the name data later
data.x <- data

## keeping only Numeric covariates
data <- data[ ,-which(names(data) %in% c("state", "communityname", "fold", "statenew"))]





