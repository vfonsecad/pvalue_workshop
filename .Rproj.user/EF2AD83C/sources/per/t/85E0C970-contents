
# -----------------------------------------------------------------------

# -------------    reproducibility of p-value     ----------------------

# -----------------------------------------------------------------------



rm(list = ls())

# --- Libraries ---

library(data.table)
library(caret)
library(ggplot2)
library(rpart.plot)
library(randomForest)
library(ipred)




# --- read data sets ---

source("case01/case01_01_read_data.R")



# --- transformations after first check 


antennae_new <- as.factor((my_data[["antennae"]]<0.82)*1 +
                            (my_data[["antennae"]]>=0.82 & my_data[["antennae"]]<1.1 )*2 +
                            (my_data[["antennae"]]>=1.1 & my_data[["antennae"]]<1.3 )*3 +
                            (my_data[["antennae"]]>=1.3 & my_data[["antennae"]]<1.4 )*4 +
                            (my_data[["antennae"]]>=1.4)*5)

my_data[["antennae_new"]] <- antennae_new

# --- here we group categories 1 and 3 because based on the previous 
# - exercise in case01_03_model02_cart.R categories 1 and 3 were not "significantly" different to one another
# - and they were "significantly" different from 2, 4 and 5

x_new <- as.factor((my_data[["antennae_new"]]==1 ) +
                     (my_data[["antennae_new"]]==3 ))

my_data[["x_new"]] <- x_new

# --- model

my_formula3 <- as.formula(paste0(current_response_var, 
                                 "~  x_new "))#
my_model3 <- glm(my_formula3, data = my_data, family = binomial())
my_model3_summary <- summary(my_model3)
my_model3_summary
my_model3_summary$coefficients[2,4]



# --- loop model: do we get always a "significant" p-value?

pvalues <- NULL

for(iteration in seq(1,100)) {
  sample_iteration <- sample(1:nrow(my_data), nrow(my_data), replace = TRUE)
  my_data_iteration <- my_data[sample_iteration]
  
  my_formula3 <- as.formula(paste0(current_response_var, 
                                   "~  x_new "))#
  my_model3 <- glm(my_formula3, data = my_data_iteration, family = binomial())
  my_model3_summary <- summary(my_model3)
  pvalues <- c(pvalues, my_model3_summary$coefficients[2,4])
}

# - interestingly, through all the iterations, such a pvalue is always
# - at least less than 0.1

ggplot(data.table(pvalues), aes(x = pvalues)) +
  geom_density(fill = "red", alpha = 0.4)
