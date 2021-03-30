source("Data_Clean.R") 

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
