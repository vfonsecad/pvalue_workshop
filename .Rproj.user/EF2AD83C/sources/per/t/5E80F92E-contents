
# -----------------------------------------------------------------------

# -------------   looking for a model that fits better the data   -------
# even if a "perfect" model cannot be found, we can probably do much better
# than what we got before
# we will try to find a better model via decision trees (cart)
# the idea is to then turn the insights of the decision tree result
# into linear components to fit another generalized linear model
# -----------------------------------------------------------------------


rm(list = ls())

# --- Libraries ---

library(data.table)
library(caret)
library(ggplot2)
library(rpart.plot)
library(randomForest)
library(ipred)




# --- Read data sets ---

source("case01/case01_01_read_data.R")

# --- Select one current dataset for work


# -------------------------- model training  -----------------------------

# - formula

my_formula0 <- as.formula(paste0(current_response_var, 
                                "~  antennae + body"))
# - model training: See documentation if tuning parameters are to be changed.
# - these following settings were chosen based on external assessment that I did not
# - bring in this code

my_cart0 <- caret::train(my_formula0, my_data, method = "rpart",model=TRUE,
                         trControl = trainControl(
                           method = "cv", number = 10, verboseIter = TRUE),
                         tuneGrid=expand.grid(cp=0.01)
)

# - Plot the tree
rpart.plot(my_cart0$finalModel)

# - CP table
my_cart0$finalModel$cptable

# - fitted values on the same dataset
my_cart0_pred <-predict(my_cart0,my_data)

# - confusion matrix with fitted values
confusionMatrix(my_cart0_pred,my_data[[current_response_var]], positive = "M")



# --- transformations after first check 


antennae_new <- as.factor((my_data[["antennae"]]<0.82)*1 +
(my_data[["antennae"]]>=0.82 & my_data[["antennae"]]<1.1 )*2 +
(my_data[["antennae"]]>=1.1 & my_data[["antennae"]]<1.3 )*3 +
(my_data[["antennae"]]>=1.3 & my_data[["antennae"]]<1.4 )*4 +
(my_data[["antennae"]]>=1.4)*5)

my_data[["antennae_new"]] <- antennae_new


# -------------------------- re training of linear model ---------------

# --- glm

my_formula2 <- as.formula(paste0(current_response_var, 
                                 "~  antennae_new + body "))
my_model2 <- glm(my_formula2, data = my_data, family = binomial())
my_model2_summary <- summary(my_model2)
my_model2_summary$coefficients # - p-values in the last column

my_fitted2_numerical <- 1/(1+exp(-predict(my_model2)))
plot(my_fitted2_numerical, my_data[[current_response_var]])
threshold <- 0.39 # - try thresholds to see which one separates effectively
my_fitted2 <- as.factor(ifelse(my_fitted2_numerical>threshold, "M", "U"))
my_fitted2 <- relevel(my_fitted2, "U")
table(my_fitted2, my_data[[current_response_var]])    
my_data0 <- copy(my_data)
my_data0[["fitted"]] <- my_fitted2


# - confusion matrix with fitted values
confusionMatrix(my_fitted2,my_data[[current_response_var]], positive = "M")

