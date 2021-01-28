#setwd load data and package
rm(list=ls())
#setwd("C:/Users/felix/Documents/GitHub/MicroEcon_Assignment_02")
library("stats")
library("tidyverse")
library("haven")
library("xtable")

library(dplyr)
library(sjmisc)

#Preproc data:
df = as.data.frame(read_dta(file = "titanic.dta"))
#create dummy for class factor
df %<>% 
  to_dummy(class, suffix = "label") %>% 
  bind_cols(df)
colnames(df) = c("class1", "class2", "class3", "classcrew", "class", "ageadult", "man", "survived")

#########################################################################################################
### a. Estimate probit
Probit_Model_Estimate = glm(survived ~ class1 + class2 + class3 + ageadult  + man, family = binomial(link = "probit"), data = df)
summary(Probit_Model_Estimate )

#output latex table
xtable(summary(Probit_Model_Estimate ))

#########################################################################################################
### b. Marginal Effect (with formula from Lecture)

Beta_Man = Probit_Model_Estimate[["coefficients"]]["man"]
ME = function(x){dnorm(x) * Beta_Man }
curve(ME(x), from=-5, to=5, , xlab="xiB", ylab="Marginal-Effect of Man", xaxp  = c(-4, 4, 16))


### iii ' Effect of gender (not marginal)
ME = function(x){pnorm(x + Beta_Man) - pnorm(x) }
curve(ME(x), from=-5, to=5, , xlab="z", ylab="Marginal-Effect of Man", xaxp  = c(-4, 4, 16))


#########################################################################################################
### c --> Already estimated in a.

#########################################################################################################
### d.test Hypothesis

#perform regression with interaction terms.:
Probit_Model_Estimate_Interact = glm(survived ~ class1 + class2 + class3 + ageadult  + man + class1*man + class2*man + class3*man, family = binomial(link = "probit"), data = df)
summary(Probit_Model_Estimate_Interact )
#output latex table
xtable(summary(Probit_Model_Estimate_Interact ))

#get log likelihoods from restricted and unrestriced models:

# log likelihood from model in b (restricted)
Log_Likelihood_Restricted = logLik(Probit_Model_Estimate)
# log likelihood from model in d (unrestricted)
Log_Likelihood_Unrestricted = logLik(Probit_Model_Estimate_Interact)

#compute test-statistics
Test_stat = -2*(Log_Likelihood_Restricted -  Log_Likelihood_Unrestricted)

#compute p_value (under H0 the test stat is asymptotically Chi squared distributed with degrees of freedom = number of restrictions)
p_value = 1- pchisq(Test_stat[1], df = 3)
