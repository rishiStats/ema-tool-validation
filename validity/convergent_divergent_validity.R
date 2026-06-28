
library(tidyverse)
library(psych)

baseline = read_csv("tropical-summer-ema/data/baseline.csv")
daily_data = read_csv("tropical-summer-ema/data/daily_data.csv")

daily_ave = daily_data %>%
  group_by(`Study ID`) %>%
  summarize(q3= mean(Q3), 
            q4= mean(Q4),
            q5= mean(Q5),
            q7 =mean(Q7),
            q8= mean(Q8),
            q9= mean(Q9),
            q10=mean(Q10),
            q11 = mean(Q11))

baseline = baseline %>%
  dplyr::select(c(1, 42:64))
data = left_join(daily_ave, baseline, by = "Study ID")

# MTMM matrix
ema_vars <- c("q3", "q4", "q5", "q7", "q8", "q9", "q10", "q11")
baseline_vars <- c("Stress", "Anxiety", "Depression", "loneliness",
                   "SWLS", "Problem Focused Coping", "Emotion Focused Coping", "Avoidant Coping")

all_vars <- c(ema_vars, baseline_vars)

mtmm <- corr.test(data[all_vars], use = "pairwise")
print(mtmm$stars, short = FALSE)




library(tidyverse)
library(psych)

# ── 1. EMA composites ─────────────────────────────────────────────────────────
daily_ave <- daily_data %>%
  group_by(`Study ID`) %>%
  summarize(
    ema_psychopath = mean(c(6-Q3, 6-Q4, 6-Q5, 6-Q6, 6-Q7), na.rm = TRUE),  
    # high = low distress = good
    ema_wellbeing  = mean(c(Q8, Q9, Q10, Q11), na.rm = TRUE),                
    # high = good
    .groups = "drop"
  ) %>%
  mutate(
    ema_overall_mh = (ema_psychopath + ema_wellbeing) / 2  
    # both already high=good, no extra flip needed
  )
# ── 2. All psychological baseline vars ───────────────────────────────────────
psych_vars <- c("extraversion", "agreeableness", "conscientiousness",
                "neuroticism", "openess to experience",
                "SWLS", "Problem Focused Coping", "Emotion Focused Coping",
                "Avoidant Coping", "loneliness",
                "Stress", "Anxiety", "Depression",
                "Sleep_qual", "Sleep_latency", "Sleep_duration",
                "Sleep_disturbance", "Daytime_dyfucntion", "Sleep_meds")

baseline_sel <- baseline %>%
  dplyr::select(`Study ID`, all_of(psych_vars))

data <- left_join(daily_ave, baseline_sel, by = "Study ID")

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