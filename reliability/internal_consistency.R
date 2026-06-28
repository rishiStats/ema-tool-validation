library(multilevelTools)
library(tidyverse)
library(psych)
library(multilevel)
library(lavaan)
library(polycor)
library(lme4)
library(performance)
library(semTools)

data = read_csv("tropical-summer-ema/data/daily_data.csv")
data$Q3r = 6 - data$Q3
data$Q4r = 6 - data$Q4
data$Q5r= 6 - data$Q5
data$Q7r = 6 - data$Q7

#cronbach's alpha "Q3r", "Q4r", "Q5r", "Q7r") #,
items = c( "Q8", "Q9", "Q10", "Q11")
alpha_results = alpha(data[items])

#multi-level omega
omega_results = omegaSEM(
  items = items,
  id = "Study ID", 
  data = data,
  savemodel = TRUE
)
print(omega_results$Results)

#split half reliability 
split_results <- splitHalf(data[items], raw = TRUE, brute = TRUE)
print(split_results)

#ordinal alpha
poly_cor <- polychoric(data[items])
ordinal_alpha <- alpha(poly_cor$rho)
print(ordinal_alpha$total)

#ravkov rho 
raykov_model <- 'f =~ Q3r + Q4r + Q5r + Q7r + Q8 + Q9 + Q10 + Q11'
raykov_fit <- cfa(raykov_model, data = data, ordered = TRUE)
raykov_rho <- compRelSEM(raykov_fit)
print(raykov_rho)

#revelles beta
omega_results_full <- omega(data[items], nfactors = 2)
omega_results_full

