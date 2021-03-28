library(papaja)
library(Hmisc)
library(psych)
source("Data_Clean.R")

#line graph overall 
head(social.screen)
social.time <- gather(social.screen, category, screen_time, social_overall:tiktok, factor_key=TRUE)
social.daily.plot <- social.time %>% 
  group_by(date, category) %>% 
  summarise(
    screen_time = sum(screen_time)) %>% 
  filter(category %in% c("social_overall")) %>% 
  ggplot(aes(x = date, y = screen_time)) +
  geom_line() +
  labs(x = "Date", y = "Daily Use (in minutes)") + 
  theme_apa()
social.daily.plot 
ggsave("daily_use_line.png", width = 6, height = 4.5, units = "in") 

#line graph by category
social.category <- gather(social.screen, category, screen_time, twitter:tiktok, factor_key=TRUE)
social.category <- social.category %>% 
  group_by(date, category) %>% 
  summarise(
    screen_time = sum(screen_time)) %>% 
  mutate(category2=category)

social.category.plot <- social.category %>%
  ggplot(aes(x=date, y=screen_time)) +
  geom_line(data=social.category %>% dplyr::select(-category),aes(group=category2), color="grey", size=0.4, alpha=0.5) +
  geom_line(aes(color=category), color="red", size=0.5) +
  labs(
    x = "Date",
    y = "Usage (in Minutes)") + 
  theme_apa() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        strip.text.x = element_text(size = 9)) + 
  facet_wrap(~ category, ncol=2, labeller=labeller(category = capitalize))
social.category.plot

ggsave("social_category_line.png", width = 6, height = 4.5, units = "in") 

# daily boxplot
daily.average <- social.time %>% 
  group_by(category) %>% 
  select(category, screen_time) %>% 
  filter(category != c("social_overall")) %>% 
  ggplot(aes(x = reorder(category, -screen_time), y = screen_time)) + 
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() + 
  geom_jitter(width = 0.2) +
  scale_x_discrete(labels = c('Twitter','Snapchat','TikTok', "Instagram", "Linkedin", "Facebook")) + 
  labs(x = "", y = "Daily Social Media Use (in minutes)") + 
  theme_apa()

daily.average
ggsave("daily_boxplot.png", width = 6, height = 4.5, units = "in") 

#daily use descriptive statistics table 
social.daily.summary <- describe(social_daily) 
print(social.daily.summary)

social.daily.summary <- as.data.frame(social.daily.summary)
social.daily.summary <- rowid_to_column(social.daily.summary)
social.daily.summary[1,1] <- "Total SM"
social.daily.summary[2,1] <- "Twitter"
social.daily.summary[3,1] <- "Instagram"
social.daily.summary[4,1] <- "Snapchat"
social.daily.summary[5,1] <- "Facebook"
social.daily.summary[6,1] <- "Linkedin"
social.daily.summary[7,1] <- "Tiktok"
social.daily.summary[8,1] <- "Anxiety"
social.daily.summary[9,1] <- "Depression"
social.daily.summary[10,1] <- "Focus"
social.daily.summary[11,1] <- "Lonliness"
social.daily.summary[12,1] <- "Daily Score"

social.daily.table <- social.daily.summary %>%
  filter(rowid %in% c("Total SM", "Twitter", "Instagram", "Snapchat", "Facebook", "Linkedin", "Tiktok")) %>% 
  select(rowid, mean, sd, min, max, range, se)
print(social.daily.table)




