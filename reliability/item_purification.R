library(tidyverse)
library(psych)
data = read_csv("tropical-summer-ema/data/daily_data.csv")

colnames(data)

data_within = data %>% 
  mutate(across(Q3:Q7, ~ 6- .x, .names = "{.col}")) %>% 
  group_by(`Study ID`) %>% 
  mutate(across(Q3:Q11, ~ .x - mean(.x), .names = "pcm_{.col}")) %>% 
  ungroup()

#item difficulty index
within_summary = data_within %>% 
  summarise(across(Q3:Q11, 
                   ~ (mean(.x) - min(.x)) / (max(.x) - min(.x)), 
                   .names = "diff_{.col}"))


#item discrimination index
scale_range = 6  
data_ranked = data_within %>%
  mutate(total = rowSums(across(Q3:Q11)))

n27 = round(0.27 * nrow(data_ranked))

high_group = slice_max(data_ranked, total, n = n27, with_ties = FALSE)
low_group  = slice_min(data_ranked, total, n = n27, with_ties = FALSE)

discrimination = data_ranked %>%
  summarise(across(Q3:Q11, ~ (mean(high_group[[cur_column()]]) - 
                                mean(low_group[[cur_column()]])) / scale_range))


#inter - item correlation 
corr.test(data_within %>% select(Q3:Q11))
dev.off()
corPlot(data_within %>% select(Q3:Q11) %>% setNames(paste("Item", 3:11)), 
        numbers = TRUE, upper = FALSE, diag = FALSE, main = "Inter-Item Correlations")

#item-total correlation
alpha(data_within %>% select(Q3:Q11))$item.stats$r.drop

#descriptor efficiency analysis 

enc_result = data_within %>%
  summarise(across(Q3:Q11, ~ {
    p <- prop.table(table(factor(.x, levels = 0:6)))  # fix levels explicitly
    enc <- exp(-sum(p * log(p + 1e-10)))              # epsilon avoids log(0)
    enc
  }, .names = "ENC_{.col}"))

enc_result/7