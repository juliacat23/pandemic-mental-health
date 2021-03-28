source("corstars.R")
source("formatted_cors.R")
source("Data_Clean.R")
library(ggcorrplot)

# organize data 
social_daily.symptoms <- social_daily %>% 
  select(-twitter, -instagram, -snapchat, -facebook, -linkedin, -tiktok)

social_daily.screen <- social_daily %>% 
  select(-daily.anxiety, -daily.depression, -daily.focus, -daily.lonley)

## screen time
# calculate correlation matrix
cor.screen <- corstars(social_daily.screen, result="none")
cor.screen <- rowid_to_column(cor.screen)
cor.screen[1, 1] <- "Total SM Use"
cor.screen[2, 1] <- "Twitter"
cor.screen[3,1]  <- "Instagram" 
cor.screen[4,1]  <- "Snapchat"
cor.screen[5,1]  <- "Facebook" 
cor.screen[6,1] <- "Linkedin"
cor.screen[7,1] <- "Tiktok"
cor.screen[8,1] <- "Daily Score"
cor.screen <- cor.screen[-1,]
print(cor.screen)

# plot correlation matrix for social media use
formatted_cors(social_daily.screen) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text() +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto"))

ggsave("cor_screen_plot.png", width = 7, height = 4.5, units = "in") 

## daily score
# calculate correlation matrix 
cor.screen.2 <- corstars(social_daily.symptoms, result="none")
print(cor.screen.2)

# plot correlation matrix for daily score 
formatted_cors(social_daily.symptoms) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text() +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto"))

ggsave("cor_mood_plot.png", width = 7, height = 4.5, units = "in") 

