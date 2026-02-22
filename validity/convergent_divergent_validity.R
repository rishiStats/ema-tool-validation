
library(tidyverse)

baseline = read_csv("tropical-summer-ema/data/baseline.csv")
daily_data = read_csv("tropical-summer-ema/data/daily_data.csv")

daily_ave = daily_data %>%
  group_by(`Study ID`) %>%
  summarize(q3= mean(Q3), 
            q4= mean(Q4),
            q5= mean(Q5),
            q6= mean(Q6),
            q7 =mean(Q7),
            q8= mean(Q8),
            q9= mean(Q9),
            q10=mean(Q10),
            q11 = mean(Q11))

baseline = baseline %>%
  select(c(1, 42:64))
data = left_join(daily_ave, baseline, by = "Study ID")

cols<- c("q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11") 
rows <- c("extraversion","agreeableness","conscientiousness", "neuroticism","openess to experience",
          "SWLS","Problem Focused Coping", "Emotion Focused Coping" ,"Avoidant Coping","loneliness",
          "Stress","Anxiety", "Depression" , 
          "Sleep_qual","Sleep_latency", "Sleep_duration" ,"Sleep_disturbance", "Daytime_dyfucntion", "Sleep_meds")        

cor_cross <- corr.test(data[rows], data[cols], use = "pairwise")
print(cor_cross$stars, short=FALSE)      