
# -----------------------------------------------------------------------

# -------------      CART    ----------------------

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

source("case03/01_read_data.R")


# --- model formulas

my_formula01 <- as.formula("diff ~ age + dosage + drug + happy_sad_group")
my_formula02 <- as.formula("diff ~ age + dosage + drug + happy_sad_group+
                           dosage*drug")
my_formula03 <- as.formula("diff ~ dosage + drug + dosage:drug")





# -------------------------- MODEL TRAINING -----------------------------

my_formula0 <- my_formula03

# --- cart

my_cart0 <- caret::train(my_formula0, my_data, method = "rpart",model=TRUE,
                         trControl = trainControl(
                           method = "cv", number = 10, verboseIter = TRUE),
                         tuneGrid=expand.grid(cp=c(0.01))
)

# - Plot the tree
rpart.plot(my_cart0$finalModel)

# - CP table
my_cart0$finalModel$cptable

# - prediction plot

my_mod_fitted <- predict(my_cart0, my_data)
my_data_mod <- copy(my_data)
my_data_mod[["fitted"]] <- my_mod_fitted

ggplot(my_data_mod, aes(x = diff, y = fitted, colour = drug)) +
  geom_point()


