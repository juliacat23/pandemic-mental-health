source("Data_Clean.R")
library(ggpubr)
library(cowplot)
library(papaja)
## Daily Mood Assessments --------------
social.time.2 <- gather(social.screen, category, screen_time, social_overall:tiktok, factor_key=TRUE)
daily.long <- gather(daily, symptom, value, daily.anxiety:daily_score, factor_key = TRUE)

daily.social.long <- social.time.2 %>% 
  inner_join(daily.long, by = "date")  %>% 
  select(-date) %>% 
  filter(category == "social_overall") %>% 
  filter(symptom != "daily_score")

daily.social.long$symptom <- factor(daily.social.long$symptom, levels = c("daily.anxiety", "daily.depression", "daily.focus", "daily.lonley"),
                                    labels = c("Anxiety", "Depression", "Focus", "Loneliness"))


c3 <- daily.social.long %>% 
  ggplot(aes(x = screen_time, y = value)) +
  geom_point() + 
  geom_smooth(method='lm', color="darkgray", alpha=0.3, size=0.5, fill="lightgray", se=TRUE) +
  facet_wrap(~symptom) + 
  labs(
    x = "Usage (in Minutes)",
    y = "Score") + 
  theme_apa() + 
  stat_cor(method = "pearson", size = 2.5) + 
  theme(
    legend.position = "none")

c4 <- social.time.2 %>% 
  inner_join(daily.long, by = "date") %>% 
  select(-date) %>% 
  filter(category == "social_overall") %>% 
  filter(symptom == "daily_score") %>% 
  ggplot(aes(x = screen_time, y = value)) + 
  geom_point(color="black") +
  geom_smooth(method="lm", color="darkgray", alpha=0.3, size=0.5, fill="lightgray", se=TRUE) +
  labs(
    x = "Usage (in Minutes)",
    y = "Score") +
  theme_apa() + 
  stat_cor(method = "pearson") + 
  theme(
    legend.position = "none")

plot_grid(c4, c3, labels = "AUTO")
ggsave("scatter_plot1.png", width = 7, height = 4.5, units = "in") 