

# ----------------------------------------------------------

# -------------       DATA SETS       ----------------------

# ----------------------------------------------------------


rm(list = ls())

# --- libraries

library(data.table)
library(caret)
library(ggplot2)

# --- read datasets with fread function


my_data <- fread("case01/data/isopods.csv", sep = ";", dec = ",") # Yvar: Mating (binary)
current_response_var <- "Mating"

# --- preliminar data treatment

sapply(my_data, class)
sapply(my_data, function(x) sum(is.na(x)))
my_data <- na.omit(copy(my_data))

my_data[[current_response_var]] <- as.factor(my_data[[current_response_var]])
my_data[[current_response_var]] <- relevel(my_data[[current_response_var]], "U")

my_data[["treatment"]] <- as.character(my_data[["treatment"]])
my_data[["replica"]] <- as.character(my_data[["replica"]])

my_data[["body_norm"]] <- (my_data[["body"]] - mean(my_data[["body"]]))/sd(my_data[["body"]])
my_data[["antennae_norm"]] <- (my_data[["antennae"]] - mean(my_data[["antennae"]]))/sd(my_data[["antennae"]])






# --- descriptive statistics


ggplot(my_data, aes(x = antennae, group = Mating, fill = Mating))+
  geom_density(alpha = 0.4)

ggplot(my_data, aes(x = body, group = Mating, fill = Mating))+
  geom_density(alpha = 0.4)

table(my_data[[current_response_var]], my_data[["replica"]])
table(my_data[[current_response_var]], my_data[["treatment"]])




 

