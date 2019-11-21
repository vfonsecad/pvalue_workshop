
# -----------------------------------------------------------------------

# -------------       SIGNIFICANCE TESTS      ----------------------

# -----------------------------------------------------------------------


rm(list = ls())

# --- Libraries ---

library(data.table)
library(caret)
library(ggplot2)



# --- Read data sets ---

source("01_read_data.R")

# --- Select one current dataset for work


# -------------------------- MODEL TRAINING -----------------------------

# --- train Model by Tag in Caret


my_formula <- as.formula(paste0(current_response_var, 
"~ body_norm + antennae_norm"))# + treatment*antennae + treatment*body"))# + body*antennae"))


# --- Model 0

my_model0 <- glm(my_formula, data = my_data, family = binomial())
my_model0_summary <- summary(my_model0)
my_model0_summary$coefficients



my_fitted0_numerical <- 1/(1+exp(-predict(my_model0)))
plot(my_fitted0_numerical, my_data[[current_response_var]])
threshold <- 0.38
my_fitted0 <- as.factor(ifelse((1/(1+exp(-predict(my_model0))))>threshold, "M_fitted", "U_fitted"))
my_fitted0 <- relevel(my_fitted0, "U_fitted")
table(my_fitted0, my_data[[current_response_var]])    
my_data0 <- copy(my_data)
my_data0[["fitted"]] <- my_fitted0

ggplot(my_data0, aes(x = body_norm, group = fitted, fill = fitted))+
  geom_density(alpha = 0.4)
ggplot(my_data, aes(x = body_norm, group = Mating, fill = Mating))+
  geom_density(alpha = 0.4)

