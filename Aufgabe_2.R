#setwd load data and package
rm(list=ls())
setwd("C:/Users/felix/Documents/GitHub/MicroEcon_Assignment_02")
library("stats")
library("tidyverse")
library("haven")
library("xtable")
df = as.data.frame(read_dta(file = "titanic.dta"))


library(dplyr)
library(sjmisc)

#create dummy for class factor
df %<>% 
  to_dummy(class, suffix = "label") %>% 
  bind_cols(df)

colnames(df) = c("class1", "class2", "class3", "classcrew", "class", "ageadult", "sexmale", "survived")
#set variable types:
#########################################################################################################
### a. Estimate probit
Probit_Model_Estimate = glm(survived ~ class1 + class2 + class3 + ageadult  + sexmale, family = binomial(link = "probit"), data = df)


#########################################################################################################
### b. Marginal Effect

Beta_Sex = Probit_Model_Estimate[["coefficients"]]["sexmale"]
ME = function(x){pnorm(x + Beta_Sex) - pnorm(x) }
curve(ME(x), from=-5, to=5, , xlab="z", ylab="Marginal-Effect of Male", xaxp  = c(-4, 4, 16))

## ii. Compute the maximum marginal effect for out of the set of discrete values

covariate_matrix_all_possible_values = matrix(c(1,1,0,0,1, 1,0,1,0,1, 1, 0,0,1,1, 1, 0,0,0,1, 1, 1,0,0,0,1,  0,1,0,0, 1, 0,0,1,0,1, 0,0,0,0), byrow = TRUE, nrow = 8, ncol = 5)

#compute xiB for each value:
xiB_all_possible_values = Probit_Model_Estimate[["coefficients"]][1:5] %*% t(covariate_matrix_all_possible_values)

#compute Marginal effect for each value:
ME_all_possible_values = ME(xiB_all_possible_values)

cbind(covariate_matrix_all_possible_values, t(xiB_all_possible_values), t(ME_all_possible_values))

xtable(cbind(covariate_matrix_all_possible_values, t(xiB_all_possible_values), t(ME_all_possible_values)))
#####
#OLD

#create covariate matrix for computing xiB (with column for 1 constant)
covariates_of_interest = df[, c("class1", "class2", "class3", "ageadult", "sexmale")]
#add col of 1 's
covariates_of_interest = cbind(rep(1, nrow(covariates_of_interest)), covariates_of_interest)
#transform to matrix
covariates_of_interest = as.matrix(covariates_of_interest)

#compute the vecto
xiB_vector =  t(Probit_Model_Estimate[["coefficients"]] %*% t(covariates_of_interest ))

pnorm(xiB_vector) * Probit_Model_Estimate[["coefficients"]]["sexmale"]