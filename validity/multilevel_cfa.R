library(lavaan)
library(readr)
data = read_csv("tropical-summer-ema/data/daily_data.csv")
data$Q3r = 6 - data$Q3
data$Q4r = 6 - data$Q4
data$Q5r = 6 - data$Q5
data$Q6r = 6 - data$Q6
data$Q7r = 6 - data$Q7

items <- c("Q3r", "Q4r", "Q5r", "Q6r", "Q7r", "Q8", "Q9", "Q10", "Q11")

# ── Model 1: Full 9-item two-factor model (initial/unconstrained) ──────────────
model_full <- '
  level: 1
    f1_w =~ Q3r + Q4r + Q5r + Q6r + Q7r
    f2_w =~ Q8 + Q9 + Q10 + Q11

  level: 2
    f1_b =~ Q3r + Q4r + Q5r + Q6r + Q7r
    f2_b =~ Q8 + Q9 + Q10 + Q11
'

fit_full <- sem(model_full,
                data    = data,
                cluster = "Study ID")

summary(fit_full, standardized = TRUE, fit.measures = TRUE)



#model 2 

model_fixed <- '
  level: 1
    f1_w =~ Q3r + Q4r + Q5r + Q6r + Q7r
    f2_w =~ Q8 + Q9 + Q10 + Q11

  level: 2
    f1_b =~ Q3r + Q4r + Q5r + Q6r + Q7r
    f2_b =~ Q8 + Q9 + Q10 + Q11

    Q10 ~~ 0*Q10
'

fit_fixed <- sem(model_fixed,
                 data    = data,
                 cluster = "Study ID")

summary(fit_fixed, standardized = TRUE, fit.measures = TRUE)

modindices(fit_fixed, sort. = TRUE, maximum.number = 15)

#model 3
model_revised_fixed <- '
  level: 1
    f1_w =~ Q3r + Q4r + Q5r +Q7r
    f2_w =~ Q8 + Q9 + Q10 + Q11
    Q3r ~~ Q4r

  level: 2
    f1_b =~ Q3r + Q4r + Q5r +Q7r
    f2_b =~ Q8 + Q9 + Q10 + Q11
    Q3r ~~ Q4r
    Q10 ~~ 0*Q10
'

fit_revised_fixed <- sem(model_revised_fixed,
                         data    = data,
                         cluster = "Study ID")

summary(fit_revised_fixed, standardized = TRUE, fit.measures = TRUE)



#model 4
model_revised_fixed <- '
  level: 1
    f1_w =~ Q3r + Q4r + Q5r +Q7r
    f2_w =~ Q8 + Q9 + Q10 + Q11
    Q3r ~~ Q4r

  level: 2
    f1_b =~ Q3r + Q4r + Q5r +Q7r
    f2_b =~ Q8 + Q9 + Q10 + Q11
    Q3r ~~ Q4r
    Q10 ~~ 0*Q10
    Q5r ~~ 0*Q5r
'

fit_revised_fixed <- sem(model_revised_fixed,
                         data    = data,
                         cluster = "Study ID")

summary(fit_revised_fixed, standardized = TRUE, fit.measures = TRUE)


library(lavaanPlot)

labels <- list(Q3r = "Item 3", Q4r = "Item 4", Q5r = "Item 5", Q7r = "Item 7",
               Q8 = "Item 8", Q9 = "Item 9", Q10 = "Item 10", Q11 = "Item 11",
               f1_w = "Factor 1 (Within)", f2_w = "Factor 2 (Within)",
               f1_b = "Factor 1 (Between)", f2_b = "Factor 2 (Between)")

lavaanPlot(model = fit_revised_fixed,
           labels = labels,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"),
           coefs = TRUE, stand = TRUE,
           graph_options = list(layout = "dot", rankdir = "LR"))