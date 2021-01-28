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

colnames(df) = c("class1", "class2", "class3", "classcrew", "class", "ageadult", "man", "survived")
#set variable types:
#########################################################################################################
### a. Estimate probit
Probit_Model_Estimate = glm(survived ~ class1 + class2 + class3 + ageadult  + man, family = binomial(link = "probit"), data = df)
summary(Probit_Model_Estimate )
xtable(summary(Probit_Model_Estimate ))
#########################################################################################################
### b. Marginal Effect (with formula from Lecture)

Beta_Man = Probit_Model_Estimate[["coefficients"]]["man"]
ME = function(x){dnorm(x) * Beta_Man }
curve(ME(x), from=-5, to=5, , xlab="xiB", ylab="Marginal-Effect of Man", xaxp  = c(-4, 4, 16))


#########################################################################################################
### b.iii ' Effect of gender (not marginal)

ME = function(x){pnorm(x + Beta_Man) - pnorm(x) }
curve(ME(x), from=-5, to=5, , xlab="z", ylab="Marginal-Effect of Man", xaxp  = c(-4, 4, 16))
#Compute the maximum marginal effect for out of the set of discrete values
covariate_matrix_all_possible_values = matrix(c(1,1,0,0,1, 1,0,1,0,1, 1, 0,0,1,1, 1, 0,0,0,1, 1, 1,0,0,0,1,  0,1,0,0, 1, 0,0,1,0,1, 0,0,0,0), byrow = TRUE, nrow = 8, ncol = 5)

#########################################################################################################
### d.test Hypothesis

#perform regression with interaction terms.:
Probit_Model_Estimate_Interact = glm(survived ~ class1 + class2 + class3 + ageadult  + man + class1*man + class2*man + class3*man, family = binomial(link = "probit"), data = df)
summary(Probit_Model_Estimate_Interact )
xtable(summary(Probit_Model_Estimate_Interact ))

#get log likelihoods from restricted and unrestriced models:
Log_Likelihood_Restricted = logLik(Probit_Model_Estimate)
Log_Likelihood_Unrestricted = logLik(Probit_Model_Estimate_Interact)

#compute test-statistics
Test_stat = -2*(Log_Likelihood_Restricted -  Log_Likelihood_Unrestricted)

#compute p_value:
p_value = 1- pchisq(Test_stat[1], df = 3)







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