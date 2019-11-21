

rm(list = ls())

# --- Libraries ---

library(MASS)



ng <- 100
g <- c(rep("group1", ng), rep("group2", ng))
y_matrix <- svd(mvrnorm(ng, c(1,0.5), Sigma = diag(2)))$u
y <- c(y_matrix[,1], y_matrix[,2]) 
md <- data.table(y,g)
ggplot(md, aes(x = y, group = g, fill = g)) + geom_density(alpha=0.5)


pvalues <- NULL

for(iteration in seq(1,100)) {
  
  sample_iteration <- sample(1:nrow(md), nrow(md), replace = TRUE)
  md_iteration <- md[sample_iteration]
  
  mod0 <- lm( y ~ g, data = md_iteration)
  mod0_summary <- summary(mod0)
  pvalues <- c(pvalues, mod0_summary$coefficients[2,4])
}

round(pvalues,4)
plot(pvalues)


# --- change simulation each time



ng <- 20

pvalues <- NULL

for(iteration in seq(1,100)) {
  
  
  g <- c(rep("group1", ng), rep("group2", ng))
  y_matrix <- svd(mvrnorm(ng, c(1,0.5), Sigma = diag(2)))$u
  y <- c(y_matrix[,1], y_matrix[,2]) 
  md <- data.table(y,g)
  
  sample_iteration <- sample(1:nrow(md), nrow(md), replace = TRUE)
  md_iteration <- md[sample_iteration]
  
  mod0 <- lm( y ~ g, data = md_iteration)
  mod0_summary <- summary(mod0)
  pvalues <- c(pvalues, mod0_summary$coefficients[2,4])
}

round(pvalues,4)
plot(pvalues)
