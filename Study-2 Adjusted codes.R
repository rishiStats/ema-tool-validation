library(readr)


data <- read_csv("Desktop/daily_data_cleaned.csv")

colnames(data)


library(tidyverse)
library(psych)

colnames(data)

data_within = data %>% 
  mutate(across(q3:q7, ~ 4 - .x, .names = "{.col}")) %>% 
  group_by(`ID`) %>% 
  mutate(across(q3:q11, ~ .x - mean(.x), .names = "pcm_{.col}")) %>% 
  ungroup()

#item difficulty index
within_summary = data_within %>% 
  summarise(across(q3:q11, 
                   ~ (mean(.x) - min(.x)) / (max(.x) - min(.x)), 
                   .names = "diff_{.col}"))


#item discrimination index
scale_range = 5 
data_ranked = data_within %>%
  mutate(total = rowSums(across(q3:q11)))

n27 = round(0.27 * nrow(data_ranked))

high_group = slice_max(data_ranked, total, n = n27, with_ties = FALSE)
low_group  = slice_min(data_ranked, total, n = n27, with_ties = FALSE)

discrimination = data_ranked %>%
  summarise(across(q3:q11, ~ (mean(high_group[[cur_column()]]) - 
                                mean(low_group[[cur_column()]])) / scale_range))


#inter - item correlation 
corr.test(dplyr::select(data_within, q3:q11))
dev.off()
corPlot(data_within %>% dplyr::select(q3:q11) %>% setNames(paste("Item", 3:11)), 
        numbers = TRUE, upper = FALSE, diag = FALSE, main = "Inter-Item Correlations")

#item-total correlation
alpha(data_within %>% dplyr::select(q3:q11))$item.stats$r.drop

#descriptor efficiency analysis 

enc_result = data_within %>%
  summarise(across(q3:q11, ~ {
    p <- prop.table(table(factor(.x, levels = 0:4)))  # fix levels explicitly
    enc <- exp(-sum(p * log(p + 1e-10)))              # epsilon avoids log(0)
    enc
  }, .names = "ENC_{.col}"))

enc_result/5




#-----------------------------------


library(lavaan)
library(readr)
data <- read_csv("Desktop/daily_data_cleaned.csv")
data$q3r = 4 - data$q3
data$q4r = 4 - data$q4
data$q5r = 4 - data$q5
data$q6r = 4 - data$q6
data$q7r =4 - data$q7

items <- c("q3r", "q4r", "q5r", "q6r", "q7r", "q8", "q9", "q10", "q11")

# ── Model 1: Full 9-item two-factor model (initial/unconstrained) ──────────────
model_full <- '
  level: 1
    f1_w =~ q3r + q4r + q5r + q6r + q7r
    f2_w =~ q8 + q9 + q10 + q11

  level: 2
    f1_b =~ q3r + q4r + q5r + q6r + q7r
    f2_b =~ q8 + q9 + q10 + q11
'

fit_full <- sem(model_full,
                data    = data,
                cluster = "ID")

summary(fit_full, standardized = TRUE, fit.measures = TRUE)

modindices(fit_full, sort. = TRUE, maximum.number = 15)

#model 2
model_revised_fixed <- '
  level: 1
    f1_w =~ q3r + q4r + q5r +q7r
    f2_w =~ q8 + q9 + q10 + q11
    q3r ~~ q4r

  level: 2
    f1_b =~ q3r + q4r + q5r +q7r
    f2_b =~ q8 + q9 + q10 + q11
    q3r ~~ q4r
'

fit_revised_fixed <- sem(model_revised_fixed,
                         data    = data,
                         cluster = "ID")

summary(fit_revised_fixed, standardized = TRUE, fit.measures = TRUE)


library(lavaanPlot)

labels <- list(q3r = "Item 3", q4r = "Item 4", q5r = "Item 5", q7r = "Item 7",
               q8 = "Item 8", q9 = "Item 9", q10 = "Item 10", q11 = "Item 11",
               f1_w = "Factor 1 (Within)", f2_w = "Factor 2 (Within)",
               f1_b = "Factor 1 (Between)", f2_b = "Factor 2 (Between)")

lavaanPlot(model = fit_revised_fixed,
           labels = labels,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"),
           coefs = TRUE, stand = TRUE,
           graph_options = list(layout = "dot", rankdir = "LR"))



#-----------------------------------------------------------



library(multilevelTools)
library(tidyverse)
library(psych)
library(multilevel)
library(lavaan)
library(polycor)
library(lme4)
library(performance)
library(semTools)

data <- read_csv("Desktop/daily_data_cleaned.csv")
data$q3r = 4 - data$q3
data$q4r = 4 - data$q4
data$q5r= 4 - data$q5
data$q7r = 4 - data$q7

#cronbach's alpha "q3r", "q4r", "q5r", "q7r") #,
items = c( "q3r", "q4r", "q5r", "q7r", "q8", "q9", "q10", "q11")
alpha_results = alpha(data[items])
alpha_results
#multi-level omega
omega_results = omegaSEM(
  items = items,
  id = "ID", 
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
raykov_model <- 'f =~ q3r + q4r + q5r + q7r + q8 + q9 + q10 + q11'
raykov_fit <- cfa(raykov_model, data = data, ordered = TRUE)
raykov_rho <- compRelSEM(raykov_fit)
print(raykov_rho)

#revelles beta
omega_results_full <- omega(data[items], nfactors = 2)
omega_results_full

#----------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
data <- read_csv("Desktop/daily_data_cleaned.csv")
# Recode and compute scales
data <- data %>%
  mutate(
    q3r = 4 - q3,
    q4r = 4 - q4,
    q5r = 4 - q5,
    q6r = 4 - q6,
    q7r = 4 - q7,
    PP    = rowSums(across(c(q3r, q4r, q5r, q7r)), na.rm = TRUE),  # Fix 1
    WB    = rowSums(across(c(q8, q9, q10, q11)),    na.rm = TRUE),  # Fix 1
    Total = PP + WB
  )

# Items for which you care about form crossover
items <- c("q3r", "q4r", "q5r", "q7r", "q8", "q9", "q10", "q11")  # Fix 2

# Example MLM on Total (just for reference)
model_Total <- lmer(Total ~ form + (1 | `ID`), data = data)  # Fix 2
summary(model_Total)

# Variables to tabulate (including composites)
variables <- c("q3r", "q4r", "q5r", "q7r", "q8", "q9", "q10", "q11", "PP", "WB", "Total")  # Fix 2

# Run MLM per item and collect descriptives + form effect size
results <- lapply(variables, function(var) {
  
  form_model <- lmer(
    formula(paste(var, "~ form + (1 | `ID`)")),
    data = data,
    control = lmerControl(optimizer = "bobyqa",
                          optCtrl = list(maxfun = 2e5))
  )
  
  temp <- data %>%
    group_by(`ID`, form) %>%
    summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = form, values_from = value) %>%
    rename(form_a = a, form_b = b)   # ✅ FIXED
  
  form_summary <- coef(summary(form_model))
  
  data.frame(
    Item             = var,
    Form_A_Mean      = round(mean(temp$form_a, na.rm = TRUE), 3),
    Form_B_Mean      = round(mean(temp$form_b, na.rm = TRUE), 3),
    Correlation      = round(cor(temp$form_a, temp$form_b, use = "complete.obs"), 3),
    Form_Effect_Size = round(form_summary["formb", "Estimate"], 3),  # ✅ FIXED
    Form_SE          = round(form_summary["formb", "Std. Error"], 3),
    Form_p           = round(form_summary["formb", "Pr(>|t|)"], 3)
  )
})
# Combine into final table
output_df <- do.call(rbind, results)  # Fix 2
print(output_df)



#-------------------------------------------



library(tidyverse)
library(psych)

baseline =  read_csv("tropical-monsoon-gema/data_wrangling/baseline/baseline_final.csv")
daily_data = read_csv("Desktop/daily_data_cleaned.csv")

daily_ave = daily_data %>%
  group_by(`ID`) %>%
  summarize(q3= mean(q3), 
            q4= mean(q4),
            q5= mean(q5),
            q7 =mean(q7),
            q8= mean(q8),
            q9= mean(q9),
            q10=mean(q10),
            q11 = mean(q11))

data = left_join(daily_ave, baseline, by = "ID")

# MTMM matrix
ema_vars <- c("q3", "q4", "q5", "q7", "q8", "q9", "q10", "q11")
baseline_vars <- c("stress" , "anxiety",  "depression", "loneliness", 
                   "social", "problem_cope" ,  "swls" )

all_vars <- c(ema_vars, baseline_vars)

mtmm <- corr.test(data[all_vars], use = "pairwise")
print(mtmm$stars, short = FALSE)














library(tidyverse)
library(psych)

# ── 1. EMA composites ─────────────────────────────────────────────────────────
daily_ave <- daily_data %>%
  group_by(`ID`) %>%
  summarize(
    ema_psychopath = sum(c(q3, q4, q5, q7), na.rm = TRUE),  
    # high = low distress = good
    ema_wellbeing  = sum(c(q8, q9, q10, q11), na.rm = TRUE),                
    # high = good
    .groups = "drop"
  ) %>%
  mutate(
    ema_overall_mh = 16 - ema_psychopath + ema_wellbeing) 
    # both already high=good, no extra flip needed


# ── 2. All psychological baseline vars ───────────────────────────────────────
psych_vars <- c("depression",  "anxiety" , "stress" , "problem_cope" , "emotion_cope", 
                "avoidant_cope" , "swls" , "social" , "loneliness" , "sleep_dur" , "sleep_dist_fin", 
                "sleep_lat_fin" , "day_dysf", "sleep_qual", "sleep_meds" , "sleep_total", "extravert", 
                "agreeable",  "conscience", "neurotic", "openness", "resilience")

baseline_sel <- baseline %>%
  dplyr::select(`ID`, all_of(psych_vars))

data <- left_join(daily_ave, baseline_sel, by = "ID")

# ── 3. Full correlation matrix ────────────────────────────────────────────────
all_vars <- c("ema_overall_mh", "ema_psychopath", "ema_wellbeing", psych_vars)

full_corr <- corr.test(data[all_vars], use = "pairwise")

cat("── Full correlation matrix with significance ──\n")
print(full_corr$stars, short = FALSE)

# ── 4. Structured summary table ──────────────────────────────────────────────
r <- full_corr$r
p <- full_corr$p

summary_df <- data.frame(
  Baseline_scale = psych_vars,
  
  r_overall      = round(r["ema_overall_mh",  psych_vars], 3),
  p_overall      = round(p["ema_overall_mh",  psych_vars], 3),
  
  r_psychopath   = round(r["ema_psychopath",  psych_vars], 3),
  p_psychopath   = round(p["ema_psychopath",  psych_vars], 3),
  
  r_wellbeing    = round(r["ema_wellbeing",   psych_vars], 3),
  p_wellbeing    = round(p["ema_wellbeing",   psych_vars], 3)
) %>%
  mutate(
    sig_overall    = case_when(p_overall    < .001 ~ "***",
                               p_overall    < .01  ~ "**",
                               p_overall    < .05  ~ "*",
                               TRUE                ~ ""),
    sig_psychopath = case_when(p_psychopath < .001 ~ "***",
                               p_psychopath < .01  ~ "**",
                               p_psychopath < .05  ~ "*",
                               TRUE                ~ ""),
    sig_wellbeing  = case_when(p_wellbeing  < .001 ~ "***",
                               p_wellbeing  < .01  ~ "**",
                               p_wellbeing  < .05  ~ "*",
                               TRUE                ~ ""),
    # Paste r and sig together
    overall_label    = paste0(r_overall,    sig_overall),
    psychopath_label = paste0(r_psychopath, sig_psychopath),
    wellbeing_label  = paste0(r_wellbeing,  sig_wellbeing)
  ) %>%
  dplyr::select(Baseline_scale, overall_label, psychopath_label, wellbeing_label)

cat("\n── Summary Table ──\n")
print(summary_df, row.names = FALSE)
