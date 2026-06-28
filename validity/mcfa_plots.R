# =========================
# 1. LIBRARIES
# =========================
library(lavaan)
library(readr)
library(lavaanPlot)

# =========================
# 2. STUDY 1
# =========================
data1 <- read_csv("tropical-summer-ema/data/daily_data.csv")

# Reverse coding (1–5 scale)
data1$Q3r <- 6 - data1$Q3
data1$Q4r <- 6 - data1$Q4
data1$Q5r <- 6 - data1$Q5
data1$Q6r <- 6 - data1$Q6
data1$Q7r <- 6 - data1$Q7

model1 <- '
  level: 1
    f1_w =~ Q3r + Q4r + Q5r + Q7r
    f2_w =~ Q8 + Q9 + Q10 + Q11
    Q3r ~~ Q4r

  level: 2
    f1_b =~ Q3r + Q4r + Q5r + Q7r
    f2_b =~ Q8 + Q9 + Q10 + Q11
    Q3r ~~ Q4r
    Q10 ~~ 0*Q10
    Q5r ~~ 0*Q5r
'

fit1 <- sem(model1,
            data = data1,
            cluster = "Study ID")

labels1 <- list(
  Q3r = "Item 3", Q4r = "Item 4", Q5r = "Item 5", Q7r = "Item 7",
  Q8 = "Item 8", Q9 = "Item 9", Q10 = "Item 10", Q11 = "Item 11",
  f1_w = "Factor 1 (Within)", f2_w = "Factor 2 (Within)",
  f1_b = "Factor 1 (Between)", f2_b = "Factor 2 (Between)"
)

p1 <- lavaanPlot(
  model = fit1,
  labels = labels1,
  node_options = list(shape = "box", fontname = "Helvetica"),
  edge_options = list(color = "grey"),
  coefs = TRUE,
  stand = TRUE,
  graph_options = list(layout = "dot", rankdir = "LR")
)

# show Study 1 plot
p1


# =========================
# 3. STUDY 2
# =========================
data2 <- read_csv("Desktop/daily_data_cleaned.csv")

# Reverse coding (check your scale!)
data2$q3r <- 4 - data2$q3
data2$q4r <- 4 - data2$q4
data2$q5r <- 4 - data2$q5
data2$q6r <- 4 - data2$q6
data2$q7r <- 4 - data2$q7

model2 <- '
  level: 1
    f1_w =~ q3r + q4r + q5r + q7r
    f2_w =~ q8 + q9 + q10 + q11
    q3r ~~ q4r

  level: 2
    f1_b =~ q3r + q4r + q5r + q7r
    f2_b =~ q8 + q9 + q10 + q11
    q3r ~~ q4r
'

fit2 <- sem(model2,
            data = data2,
            cluster = "ID")

labels2 <- list(
  q3r = "Item 3", q4r = "Item 4", q5r = "Item 5", q7r = "Item 7",
  q8 = "Item 8", q9 = "Item 9", q10 = "Item 10", q11 = "Item 11",
  f1_w = "Factor 1 (Within)", f2_w = "Factor 2 (Within)",
  f1_b = "Factor 1 (Between)", f2_b = "Factor 2 (Between)"
)

p2 <- lavaanPlot(
  model = fit2,
  labels = labels2,
  node_options = list(shape = "box", fontname = "Helvetica"),
  edge_options = list(color = "grey"),
  coefs = TRUE,
  stand = TRUE,
  graph_options = list(layout = "dot", rankdir = "LR")
)

# show Study 2 plot
p2