## Data Viz Script
library(tidyverse)
library(gghighlight)
library(gridExtra)
library(gtable)
library(gt)
library(egg)
library(cowplot)
library(gapminder)
library(patchwork)
library(Hmisc)
library(hrbrthemes)
library(jtools)
library(GGally)
library(xtable)
library(ggcorrplot)
library(psych)
library(ggpubr)
library(psychTools)



## Source data from cleaning script 
source("Data_Clean.R")

## Social Media Usage  ---------------

#line graph overall 
head(social.screen)
social.time <- gather(social.screen, category, screen_time, social_overall:tiktok, factor_key=TRUE)
social.time <- social.time %>% 
  group_by(date, category) %>% 
  summarise(
    screen_time = sum(screen_time))

avg.screen <- social.time %>% 
  filter(category %in% c("social_overall")) %>% 
  select(screen_time)

daily.average <- social.time %>% 
  group_by(category) %>% 
  select(category, screen_time) %>% 
  filter(category != c("social_overall")) %>% 
  summarise(
    total = sum(screen_time),
    n = n(), 
    average = total/n) %>% 
  ggplot(aes(x = reorder(category, -average), y = average)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = c('Twitter','Snapchat','TikTok', "Instagram", "Linkedin", "Facebook")) + 
  labs(x = "", y = "Average Daily Use (in minutes)") + 
  theme_apa()
  



social.time %>%
  filter(category == "social_overall") %>% 
  ggplot(aes(x=date, y=screen_time)) + 
  geom_line() +
  scale_size_manual(values=c(1.5,0.2)) +
  labs(x = "Date", 
       y = "Usage (in Minutes)") + 
  theme_apa() + 
  theme(
    legend.position = "none")
ggsave("overall_use.png")

#line graph by category
social.category <- gather(social.screen, category, screen_time, twitter:tiktok, factor_key=TRUE)
social.category <- social.category %>% 
  group_by(date, category) %>% 
  summarise(
    screen_time = sum(screen_time)) %>% 
  mutate(category2=category)

c2 <- social.category %>%
  ggplot(aes(x=date, y=screen_time)) +
  geom_line(data=social.category %>% dplyr::select(-category),aes(group=category2), color="grey", size=0.1, alpha=0.5) +
  geom_line(aes(color=category), color="red", size=0.2) +
  labs(
    x = "Date",
    y = "Usage (in Minutes)") + 
  theme_apa() +
  theme(axis.text.x = element_text(size = 1),
        axis.text.y = element_text(size = 1),
        axis.title.x = element_text(size = 2),
        axis.title.y = element_text(size = 2),
        strip.text.x = element_text(size = 2)) + 
  facet_wrap(~ category, ncol=2, labeller=labeller(category = capitalize))
c2

ggsave("plot23.png", width = .9, height = 1.3, units = "in")
plot_grid(c1, c2, labels = "AUTO")

ggsave("plot1.png", width = 8, height = 4.5, units = "in") 

## Daily Mood Assessments --------------
social.time.2 <- gather(social.screen, category, screen_time, social_overall:tiktok, factor_key=TRUE)
daily.long <- gather(daily, symptom, value, daily.anxiety:daily_score, factor_key = TRUE)

daily.social.long <- social.time.2 %>% 
  inner_join(daily.long, by = "date")  %>% 
  select(-date) %>% 
  filter(category == "social_overall") %>% 
  filter(symptom != "daily_score")

daily.social.long$symptom <- factor(daily.social.long$symptom, levels = c("daily.anxiety", "daily.depression", "daily.focus", "daily.lonley"),
                                 labels = c("Anxiety", "Depression", "Focus", "Lonliness"))


c3 <- daily.social.long %>% 
ggplot(aes(x = screen_time, y = value)) +
  geom_point() + 
  geom_smooth(method='lm', color="darkgray", alpha=0.3, size=0.5, fill="lightgray", se=TRUE) +
  facet_wrap(~symptom) + 
  labs(
    x = "Usage (in Minutes)",
    y = "Score") + 
  theme_apa() + 
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
  theme(
    legend.position = "none")

plot_grid(c4, c3, labels = "AUTO")
ggsave("plot2.png", width = 8, height = 4.5, units = "in") 

social_daily.symptoms <- social_daily %>% 
  select(-twitter, -instagram, -snapchat, -facebook, -linkedin, -tiktok)

social_daily.screen <- social_daily %>% 
  select(-daily.anxiety, -daily.depression, -daily.focus, -daily.lonley)

dev.off()
png("gen_cor_plot.png", width = 512, height = 400)
cor.plot(social_daily, main = "", upper = FALSE, diag = FALSE, zlim = c(-0.26, .95), scale = FALSE, stars = TRUE, labels = c("Total SM", "Twiter", "Instagram", "Snapchat", "Facebook", "Linkedin", "Tik Tok", "Anxiety", "Depression", "Focus", "Lonley", "Daily Score"))
dev.off()
png("cor_screen_plot.png", width = 512, height = 400)
pairs.panels(social_daily.screen, stars = TRUE, labels = c("Total SM", "Twitter", "Instagram", "Snapchat", "Facebook", "Linkedin", "Tik-Tok", "Daily Score"))
dev.off()
png("cor_symptoms_plot.png", width = 512, height = 400)
pairs.panels(social_daily.symptoms, stars = TRUE, labels = c("Total SM", "Anxiety", "Depression", "Focus", "Loneliness", "Daily Score"))
dev.off()

## Before and After ----------------------------
composite.long <- gather(composite, test, score, stress:total_score, factor_key=TRUE)
dev.off()
ggplot(composite.long, aes(x = test, y = score, fill = date)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values=c("gray", "black"), name = "", labels = c("Before", "After")) + 
  scale_x_discrete(labels = c('PSS','GAD-7','CES-R', "Composite")) + 
  geom_text(aes(label=score), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3) + 
  labs(x = "Test", y = "Score") + 
  theme_apa() +
  theme(
    axis.text.x = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_blank())
ggsave("comparison.png", width = 4, height = 4, units = "in")

## ----------------------

summary(daily)

summary(daily.long.average)

shapiro.test(social_daily.symptoms$social_overall)
shapiro.test(social_daily.symptoms$daily.anxiety)
shapiro.test(social_daily.symptoms$daily.depression)
shapiro.test(social_daily.symptoms$daily.lonley)
shapiro.test(social_daily.symptoms$daily.focus)
shapiro.test(social_daily.symptoms$daily_score) 
cor.test(social_daily.screen$daily_score, social_daily.screen$tiktok)
fisher.test(composite)

composite.compare <- read_excel("composite.xlsx")
library(rstatix)
x <- social_daily.symptoms$daily_score

ggqqplot(social_daily.symptoms$daily.anxiety)

# load required packages
library(rstatix)
library(ggpubr)
library(tidyverse)
library(cowplot)
source("Data_Clean.R") #import cleaned data

# organize data 
social_daily.symptoms <- social_daily %>% 
  select(-twitter, -instagram, -snapchat, -facebook, -linkedin, -tiktok)
social_daily.screen <- social_daily %>% 
  select(-daily.anxiety, -daily.depression, -daily.focus, -daily.lonley)

# Shapiro-Wilk’s normality test
shapiro.test(social_daily.symptoms$daily.anxiety)
shapiro.test(social_daily.symptoms$daily.depression)
shapiro.test(social_daily.symptoms$daily.lonley)
shapiro.test(social_daily.symptoms$daily.focus)
shapiro.test(social_daily.symptoms$daily_score) # null-hypothesis: distribution is normal 

# Kolmogorov Smirnov's test
anxiety.x <- social_daily.symptoms$daily.anxiety
depression.x <- social_daily.symptoms$daily.depression
focus.x <- social_daily.symptoms$daily.focus
lonely.x <- social_daily.symptoms$daily.lonley
score.x <- social_daily.symptoms$daily_score
ks.test(anxiety.x, "pnorm", mean=mean(anxiety.x), sd=sd(anxiety.x))
ks.test(depression.x, "pnorm", mean=mean(depression.x), sd=sd(depression.x))
ks.test(focus.x, "pnorm", mean=mean(focus.x), sd=sd(focus.x))
ks.test(lonely.x, "pnorm", mean=mean(lonely.x), sd=sd(lonely.x))
ks.test(score.x, "pnorm", mean=mean(score.x), sd=sd(score.x))

##qq plots 
norm.anxiety <- ggqqplot(anxiety.x)
norm.depression <- ggqqplot(depression.x)
norm.focus <- ggqqplot(focus.x)
norm.lonely <- ggqqplot(lonely.x)
norm.score <- ggqqplot(score.x)
cowplot::plot_grid(norm.anxiety, norm.depression, norm.focus, norm.lonely, norm.score, labels = "AUTO", nrow = 3)
ggsave("qqplot.png")

#distribution plots 
dist.anxiety <- ggdensity(anxiety.x)
dist.depression <- ggdensity(depression.x)
dist.focus <- ggdensity(focus.x)
dist.lonely <- ggdensity(lonely.x)
dist.score <- ggdensity(score.x)
cowplot::plot_grid(dist.anxiety, dist.depression, dist.focus, dist.lonely, dist.score, labels = "AUTO", nrow = 3)

