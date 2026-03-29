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

model = lmer(Total ~ form + (1 | `Study ID`), data = data)
summary(model)


variables = c("Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "PP", "WB", "Total")
results = lapply(variables, function(var) {
  temp = data %>%
    group_by(`Study ID`, form) %>%
    summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = form, values_from = value) %>%
    rename(form_a = `1`, form_b = `2`)
  data.frame(
    Item = var,
    Form_A_Mean = round(mean(temp$form_a, na.rm = TRUE), 3),
    Form_B_Mean = round(mean(temp$form_b, na.rm = TRUE), 3),
    Correlation = round(cor(temp$form_a, temp$form_b, use = "complete.obs"), 3)
  )
})
output_df = do.call(rbind, results)
print(output_df)
