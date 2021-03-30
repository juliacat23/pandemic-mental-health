source("Data_Clean.R")

# daily score boxplot
daily.score.boxplot <- daily.long %>% 
  group_by(symptom) %>% 
  select(symptom, value) %>% 
  filter(symptom != c("daily_score")) %>% 
  ggplot(aes(x = symptom, y = value)) + 
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() + 
  geom_jitter(width = 0.2) +
  scale_x_discrete(labels = c("Anxiety",'Depression','Focus', "Loneliness")) + 
  labs(x = "", y = "High Frequency Mood Assessment Scores") + 
  theme_apa()

daily.score.boxplot
ggsave("daily_score_plot.png", width = 6, height = 3.5, units = "in")