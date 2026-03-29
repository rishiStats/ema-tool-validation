library(tidyverse)

#round 1 data
data_1 = read_csv("ema-tool-validation/data/content_validity_data.csv")
data_1 %>%
  count(Domain, sort = TRUE)

data_1 %>%
  summarise(across(c(Clinical, Academic, Research), ~ sum(. == 1)))

data_1 %>%
  summarize(min = min(Experience), max= max(Experience), 
            median = median(Experience), iqr = IQR(Experience))

data_1 = data_1 %>%
  mutate(across(`Item 1`:`Item 19`, ~ case_when(
    . %in% c(1, 2) ~ 0,
    . %in% c(3, 4) ~ 1,))) %>%
      select(-c(Domain, Academic, Clinical, Research, Experience))

data_1 = data_1 %>%
  t() %>%
  as.data.frame() %>%
  mutate(relevant = rowSums(.[, 1:13]), i_cvi = relevant/13, 
         ua = if_else(relevant == 13, 1, 0), i_cvr =(relevant- (13/2))/(13/2), 
         kappa = i_cvi - ((1 - i_cvi) * ((13/2 + 1)/(13 + 1))))

data_1 %>%
  summarize(s_cvi_ave = mean(i_cvi),s_cvi_ua = mean(ua),  
            s_cvr = mean(i_cvr), s_kappa = mean(kappa))


#round 2 

data_2 = read_csv("ema-tool-validation/data/content_validity_data - Round2.csv")
data_2 = data_2 %>%
  select(-c(Domain, Academic, Clinical, Research, Experience))

data_2 = data_2 %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(relevant = rowSums(.[, 1:13]), 
         i_cvi = relevant/13, 
         ua = if_else(relevant == 13, 1, 0), 
         i_cvr =(relevant- (13/2))/(13/2),
         kappa = i_cvi - ((1 - i_cvi) * ((13/2 + 1)/(13 + 1)))) 
data_2 %>% 
summarize(s_cvi_ave = mean(i_cvi), s_cvi_ua = mean(ua), 
            s_cvr = mean(i_cvr), s_kappa = mean(kappa))
