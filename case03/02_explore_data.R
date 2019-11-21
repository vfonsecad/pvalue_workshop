
# -----------------------------------------------------------------------

# -------------       EXPLORE DATA      ----------------------

# -----------------------------------------------------------------------


rm(list = ls())

# --- Libraries ---

library(data.table)
library(caret)
library(ggplot2)

# --- Read data sets ---

source("case03/01_read_data.R")


# --- functions

data_summary <- function(my_data){
  
  N <- nrow(my_data)
  output <- list()
  my_data_classes <- sapply(my_data, class)
  
  for(my_col in names(my_data_classes)){
    
    if(my_data_classes[my_col] == "character"){
      output[[my_col]] <- round(100*(table(my_data[[my_col]])/N),2)
    } else{
        output[[my_col]] <- summary(my_data[[my_col]])
      }
  }
  
  return(output)
  
}



# --- exploratory analysis

data_summary(my_data)
all_vars <- colnames(my_data)
all_vars


# - plot dosage
ggplot(my_data, aes(x = diff, group = as.factor(dosage), 
                    fill = as.factor(dosage)))+
  geom_density(alpha=0.6,linetype = 0)+
  ggtitle("dosage")

ggplot(my_data, aes(y = diff, x = as.factor(dosage), 
                    fill = as.factor(dosage)))+
  geom_boxplot(alpha=0.6,linetype = 1)+
  ggtitle("dosage")


# - plot happy_sad_group
ggplot(my_data, aes(x = diff, group = as.factor(happy_sad_group), 
                    fill = as.factor(happy_sad_group)))+
  geom_density(alpha=0.6,linetype = 0)+
  ggtitle("happy_sad_group")

ggplot(my_data, aes(y = diff, x = as.factor(happy_sad_group), 
                    fill = as.factor(happy_sad_group)))+
  geom_boxplot(alpha=0.6,linetype = 1)+
  ggtitle("happy_sad_group")


# - plot drug
ggplot(my_data, aes(x = diff, group = as.factor(drug), 
                    fill = as.factor(drug)))+
  geom_density(alpha=0.6,linetype = 0)+
  ggtitle("drug")

ggplot(my_data, aes(y = diff, x = as.factor(drug), 
                    fill = as.factor(drug)))+
  geom_boxplot(alpha=0.6,linetype = 1)+
  ggtitle("drug")

# - plot age

ggplot(my_data, aes(x = age, y = diff, colour = as.factor(drug)))+
  geom_point()


ggplot(my_data, aes(x = age, y = diff, colour = dosage))+
  geom_point()+
  facet_wrap("drug")

  