library(tidyverse)
library(lme4)
library(lmerTest)

# Recode and compute scales
data <- data %>%
  mutate(
    Q3r = 6 - Q3,
    Q4r = 6 - Q4,
    Q5r = 6 - Q5,
    Q6r = 6 - Q6,
    Q7r = 6 - Q7,
    PP    = rowSums(across(c(Q3r, Q4r, Q5r, Q7r)), na.rm = TRUE),  # Fix 1
    WB    = rowSums(across(c(Q8, Q9, Q10, Q11)),    na.rm = TRUE),  # Fix 1
    Total = PP + WB
  )

# Items for which you care about form crossover
items <- c("Q3r", "Q4r", "Q5r", "Q7r", "Q8", "Q9", "Q10", "Q11")  # Fix 2

# Example MLM on Total (just for reference)
model_Total <- lmer(Total ~ form + (1 | `Study ID`), data = data)  # Fix 2
summary(model_Total)

# Variables to tabulate (including composites)
variables <- c("Q3r", "Q4r", "Q5r", "Q7r", "Q8", "Q9", "Q10", "Q11", "PP", "WB", "Total")  # Fix 2

# Run MLM per item and collect descriptives + form effect size
results <- lapply(variables, function(var) {  # Fix 2
  # Fit MLM: variable ~ form + (1 | Study ID)
  form_model <- lmer(
    formula(paste(var, "~ form + (1 | `Study ID`)")),
    data = data,
    control = lmerControl(optimizer = "bobyqa",  # Fix 3
                          optCtrl = list(maxfun = 2e5))
  )
  
  temp <- data %>%
    group_by(`Study ID`, form) %>%
    summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = form, values_from = value) %>%
    rename(form_a = `1`, form_b = `2`)
  
  # Extract form effect size (fixed-effect coefficient)
  form_coef <- fixef(form_model)["form"]  # Fix 4
  
  # Compute SE and p-value for form effect  # Fix 5
  form_summary <- coef(summary(form_model))
  form_se      <- form_summary["form", "Std. Error"]
  form_p       <- form_summary["form", "Pr(>|t|)"]
  
  data.frame(
    Item             = var,
    Form_A_Mean      = round(mean(temp$form_a, na.rm = TRUE), 3),
    Form_B_Mean      = round(mean(temp$form_b, na.rm = TRUE), 3),
    Correlation      = round(cor(temp$form_a, temp$form_b, use = "complete.obs"), 3),
    Form_Effect_Size = round(form_coef, 3),
    Form_SE          = round(form_se, 3),  # Fix 5
    Form_p           = round(form_p, 3)    # Fix 5
  )
})

# Combine into final table
output_df <- do.call(rbind, results)  # Fix 2
print(output_df)