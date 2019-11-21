
# -----------------------------------------------------------------------

# -------------       SIGNIFICANCE TESTS      ----------------------

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

source("01_read_data.R")

# --- Select one current dataset for work


# -------------------------- MODEL TRAINING -----------------------------

my_formula0 <- as.formula(paste0(current_response_var, 
                                "~  antennae + treatment"))#
# - CV

my_cart0 <- caret::train(my_formula0, my_data, method = "rpart",model=TRUE,
                         trControl = trainControl(
                           method = "cv", number = 10, verboseIter = TRUE),
                         tuneGrid=expand.grid(cp=0.01)
)

# - Plot the tree
rpart.plot(my_cart0$finalModel)

# - CP table
my_cart0$finalModel$cptable

# - Prediction on test set
my_cart0_pred <-predict(my_cart0,my_data)

# - Performance on test set
confusionMatrix(my_cart0_pred,my_data[[current_response_var]], positive = "M")



# --- transformations after first check 


antennae_new <- as.factor((my_data[["antennae"]]<0.82)*1 +
(my_data[["antennae"]]>=0.82 & my_data[["antennae"]]<1.1 )*2 +
(my_data[["antennae"]]>=1.1 & my_data[["antennae"]]<1.3 )*3 +
(my_data[["antennae"]]>=1.3 & my_data[["antennae"]]<1.4 )*4 +
(my_data[["antennae"]]>=1.4)*5)

my_data[["antennae_new"]] <- antennae_new


# -------------------------- MODEL TRAINING 2 -----------------------------

my_formula1 <- as.formula(paste0(current_response_var, 
                                 "~  antennae_new + treatment"))#
# - CV

my_cart1 <- caret::train(my_formula1, my_data, method = "rpart",model=TRUE,
                         trControl = trainControl(
                           method = "cv", number = 10, verboseIter = TRUE),
                         tuneGrid=expand.grid(cp=0.01)
)

# - Plot the tree
rpart.plot(my_cart1$finalModel)

# - CP table
my_cart1$finalModel$cptable

# - Prediction on test set
my_cart1_pred <-predict(my_cart1,my_data)

# - Performance on test set
confusionMatrix(my_cart1_pred,my_data[[current_response_var]], positive = "M")

# ----------- glm

my_formula2 <- as.formula(paste0(current_response_var, 
                                 "~  antennae_new + treatment "))#
my_model2 <- glm(my_formula2, data = my_data, family = binomial())
my_model2_summary <- summary(my_model2)
my_model2_summary$coefficients

my_fitted2_numerical <- 1/(1+exp(-predict(my_model2)))
plot(my_fitted2_numerical, my_data[[current_response_var]])
threshold <- 0.5
my_fitted2 <- as.factor(ifelse(my_fitted2_numerical>threshold, "M", "U"))
my_fitted2 <- relevel(my_fitted2, "U")
table(my_fitted2, my_data[[current_response_var]])    
my_data0 <- copy(my_data)
my_data0[["fitted"]] <- my_fitted0



x_new <- as.factor((my_data[["antennae_new"]]==2 & my_data[["treatment"]] =="low") +
                     (my_data[["antennae_new"]]==5 & my_data[["treatment"]] =="low"))

my_data[["x_new"]] <- x_new


my_formula3 <- as.formula(paste0(current_response_var, 
                                 "~ -1+ x_new "))#
my_model3 <- glm(my_formula3, data = my_data, family = binomial())
my_model3_summary <- summary(my_model3)
my_model3_summary$coefficients

my_fitted3_numerical <- 1/(1+exp(-predict(my_model3)))
plot(my_fitted3_numerical, my_data[[current_response_var]])
threshold <- 0.3
my_fitted3 <- as.factor(ifelse(my_fitted3_numerical>threshold, "M", "U"))
my_fitted3 <- relevel(my_fitted3, "U")
table(my_fitted3, my_data[[current_response_var]])    

# - Performance
confusionMatrix(my_fitted3,my_data[[current_response_var]], positive = "M")

