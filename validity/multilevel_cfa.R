library(lavaan)
data = read_csv("tropical-summer-ema/data/daily_data.csv")
data$Q3r = 6 - data$Q3
data$Q4r = 6 - data$Q4
data$Q5r = 6 - data$Q5
data$Q6r = 6 - data$Q6
data$Q7r = 6 - data$Q7

items = c("Q3r", "Q4r", "Q5r", "Q6r", "Q7r", "Q8", "Q9", "Q10", "Q11")

model_fixed <- '
  level: 1
    # Within-person factor structure
    f1_w =~ Q3r + Q4r + Q5r + Q6r + Q7r
    f2_w =~ Q8 + Q9 + Q10 + Q11

  level: 2
    # Between-person factor structure
    f1_b =~ Q3r + Q4r + Q5r + Q6r + Q7r
    f2_b =~ Q8 + Q9 + Q10 + Q11
    
    # negative variance of Q10 at level 2 to zero
    Q10 ~~ 0*Q10
'
fit_fixed <- sem(model_fixed, 
                         data = data, 
                         cluster = "Study ID")
summary(fit_fixed, standardized = TRUE, fit.measures = TRUE)