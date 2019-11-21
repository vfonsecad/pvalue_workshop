
# -----------------------------------------------------------------------

# ------------- significance tests based on a generalized linear model  -

# -----------------------------------------------------------------------


rm(list = ls())

# --- Libraries ---

library(data.table)
library(caret)
library(ggplot2)



# --- Read data sets ---

source("case01/case01_01_read_data.R")


# -------------------------- model training -----------------------------

# --- formula of the model

my_formula <- as.formula(paste0(current_response_var, 
"~ body_norm + antennae_norm"))


# --- linear model with binomial distribution for the response variable

my_model0 <- glm(my_formula, data = my_data, family = binomial())
my_model0_summary <- summary(my_model0)
my_model0_summary$coefficients # p-values are in the last column

# --- check fit of the model

my_fitted0_numerical <- 1/(1+exp(-predict(my_model0)))
plot(my_fitted0_numerical, my_data[[current_response_var]])
threshold <- 0.38 # --- set a different threshold to divide the predicted probabilities
my_fitted0 <- as.factor(ifelse((1/(1+exp(-predict(my_model0))))>threshold, "M", "U"))
my_fitted0 <- relevel(my_fitted0, "U")
table(my_fitted0, my_data[[current_response_var]])    
my_data0 <- copy(my_data)
my_data0[["fitted"]] <- my_fitted0


# - confusion matrix with fitted values
confusionMatrix(my_fitted0,my_data[[current_response_var]], positive = "M")


# --- density of predicted categories

ggplot(my_data0, aes(x = body_norm, group = fitted, fill = fitted))+
  geom_density(alpha = 0.4)

# --- density of real categories

ggplot(my_data, aes(x = body_norm, group = Mating, fill = Mating))+
  geom_density(alpha = 0.4)

