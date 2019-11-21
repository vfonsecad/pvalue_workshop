

# ----------------------------------------------------------

# -------------       DATA SETS       ----------------------

# ----------------------------------------------------------


# rm(list = ls())

# --- libraries

library(data.table)
library(caret)
library(ggplot2)

# --- read datasets with fread function


my_data <- fread("case03/data/Islander_data.csv", sep = ",", dec = ".") # Yvar
colnames_original <- colnames(my_data)
colnames(my_data) <- tolower(colnames_original)
current_response_var <- "diff"
N <- nrow(my_data)

# --- preliminar data treatment
