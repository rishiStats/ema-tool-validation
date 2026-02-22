library(tidyverse)
library(lavaan)
library(semTools)

# Read data
baseline = read_csv("tropical-summer-ema/data/baseline.csv")
daily_data = read_csv("tropical-summer-ema/data/daily_data.csv")

# Merge datasets
data = left_join(daily_data, baseline, by = "Study ID")

# Reverse code items
data$Q3r = 6 - data$Q3
data$Q4r = 6 - data$Q4
data$Q5r = 6 - data$Q5
data$Q6r = 6 - data$Q6
data$Q7r = 6 - data$Q7

# measurement model
config_model <- '
  MH =~ Q3r + Q4r + Q5r + Q6r + Q7r + Q8 + Q9 + Q10 + Q11
'

#invariance accross days
config_day <- cfa(config_model, data = data, group = "day", std.lv = TRUE)
metric_day <- cfa(config_model, data = data, group = "day", 
                  group.equal = "loadings", std.lv = TRUE)
scalar_day <- cfa(config_model, data = data, group = "day",
                  group.equal = c("loadings", "intercepts"), std.lv = TRUE)
anova(config_day, metric_day, scalar_day, test = "Chisq")

#invariance across forms 
config_form <- cfa(config_model, data = data, group = "form", std.lv = TRUE)
metric_form <- cfa(config_model, data = data, group = "form",
                   group.equal = "loadings", std.lv = TRUE)
scalar_form <- cfa(config_model, data = data, group = "form",
                   group.equal = c("loadings", "intercepts"), std.lv = TRUE)
anova(config_form, metric_form, scalar_form, test = "Chisq")

#invariance across location
config_location <- cfa(config_model, data = data, group = "location", std.lv = TRUE)
metric_location <- cfa(config_model, data = data, group = "location",
                       group.equal = "loadings", std.lv = TRUE)
scalar_location <- cfa(config_model, data = data, group = "location",
                       group.equal = c("loadings", "intercepts"), std.lv = TRUE)
anova(config_location, metric_location, scalar_location, test = "Chisq")

#invariance across ses
config_ses <- cfa(config_model, data = data, group = "kuppu", std.lv = TRUE)
metric_ses <- cfa(config_model, data = data, group = "kuppu",
                       group.equal = "loadings", std.lv = TRUE)
scalar_ses <- cfa(config_model, data = data, group = "kuppu",
                       group.equal = c("loadings", "intercepts"), std.lv = TRUE)
anova(config_ses, metric_ses, scalar_ses, test = "Chisq")

#invariance across sex
config_sex <- cfa(config_model, data = data, group = "sex", std.lv = TRUE)
metric_sex <- cfa(config_model, data = data, group = "sex",
                  group.equal = "loadings", std.lv = TRUE)
scalar_sex <- cfa(config_model, data = data, group = "sex",
                  group.equal = c("loadings", "intercepts"), std.lv = TRUE)
anova(config_sex, metric_sex, scalar_sex, test = "Chisq")
