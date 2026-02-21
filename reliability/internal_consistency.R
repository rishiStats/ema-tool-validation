library(multilevelTools)
library(tidyverse)
library(psych)

data = read_csv("tropical-summer-ema/data/daily_data.csv")
data$Q3r = 6 - data$Q3
data$Q4r = 6 - data$Q4
data$Q5r= 6 - data$Q5
data$Q6r = 6 - data$Q6
data$Q7r = 6 - data$Q7

#cronbach's alpha
items = c("Q3r", "Q4r", "Q5r", "Q6r", "Q7r", "Q8", "Q9", "Q10", "Q11")
alpha_results = psych::alpha(data[items])

#multi-level cronbach's alpha
data = as.data.frame(data)
ml_results = multilevel.reliability(data, 
                                     items=items, 
                                     grp="Study ID", 
                                     Time="day",
                                     lmer=TRUE)
print(ml_results)

#multi-level omega
omega_results = omegaSEM(
  items = items,
  id = "Study ID", 
  data = data,
  savemodel = TRUE
)
print(omega_results$Results)

#split half reliability 
split_results <- splitHalf(data[items])
print(split_results)



