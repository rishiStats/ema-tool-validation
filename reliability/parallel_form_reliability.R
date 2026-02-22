library(tidyverse)
library(lme4)
library(lmerTest)

data = read_csv("tropical-summer-ema/data/daily_data.csv")
data$Q3r = 6 - data$Q3
data$Q4r = 6 - data$Q4
data$Q5r = 6 - data$Q5
data$Q6r = 6 - data$Q6
data$Q7r = 6 - data$Q7

items = c("Q3r", "Q4r", "Q5r", "Q6r", "Q7r", "Q8", "Q9", "Q10", "Q11")

model <- lmer(Total ~ form + (1 | `Study ID`), data = data)
summary(model)