
# -----------------------------------------------------------------------

# -------------       SIGNIFICANCE TESTS     ----------------------

# -----------------------------------------------------------------------


rm(list = ls())

# --- Libraries ---

library(data.table)
library(caret)
library(ggplot2)
library(MASS)

# --- Read data sets ---

source("case03/01_read_data.R")


# --- model formulas

my_formula01 <- as.formula("diff ~ age + dosage + drug + happy_sad_group")
my_formula02 <- as.formula("diff ~ age + dosage + drug + happy_sad_group+
                           dosage*drug")
my_formula03 <- as.formula("diff ~ dosage + drug + dosage:drug")

# --- linear regression, anova, aov...

my_mod <- lm(my_formula03, data = my_data)

summary(my_mod)
anova(my_mod)
aov(my_mod)


# --- reproducibility of p-values


pvalues<- NULL

for(iteration in seq(1, 100)){
  
  sample_iteration <- sample(1:N, N, replace = TRUE)
  my_data_iteration <- my_data[sample_iteration]
  my_mod <- lm(my_formula03, data = my_data_iteration)
  my_mod_summary <- summary(my_mod)
  pvalues <- rbind(pvalues, my_mod_summary$coefficients[,4])
  
}


summary(pvalues)


# --- model fitness


my_mod <- lm(my_formula03, data = my_data)
my_data_yvar <- my_data[[current_response_var]]
my_mod_fitted <- predict(my_mod, my_data)
my_data_mod <- copy(my_data)
my_data_mod[["fitted"]] <- my_mod_fitted

ggplot(my_data_mod, aes(x = diff, y = fitted, colour = drug)) +
  geom_point()

hist(my_mod$residuals)
my_mod_stepaic <- stepAIC(my_mod)
summary(my_mod_stepaic)


