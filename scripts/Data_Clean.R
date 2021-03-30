library(tidyverse)
library(readxl)
library(lubridate)

#import data 
stress.data <- read_excel("data/PSS.xlsx")
anxiety.data <-read_excel("data/GAD-7.xlsx")
depression.data <- read_excel("data/CES-R.xlsx")
daily.data <- read_excel("data/daily.xlsx")
social.screen <- read_excel("data/screen-time.xlsx")

#data cleaning

## Perceived Stress Scale
colnames(stress.data)
names(stress.data)[28] <- "score"

stress <- stress.data %>% 
  summarise(
    date = lubridate::as_date(StartDate),
    score = score) 
stress$score <- as.numeric(as.character(stress$score))

## Anxiety (GAD-7)
colnames(anxiety.data)
names(anxiety.data)[25] <- "score"

anxiety <- anxiety.data %>% 
  summarise(
    date = lubridate::as_date(StartDate),
    score = score)
anxiety$score <- as.numeric(as.character(anxiety$score))

## Depression (CESD-R)
colnames(depression.data) 
names(depression.data)[38] <- "score"

depression <- depression.data %>% 
  summarise(
    date = lubridate::as_date(StartDate),
    score = score)
depression$score <- as.numeric(as.character(depression$score))

# compile composite score 

composite <- data.frame("date" = stress$date, # keep date
                        "stress" = stress$score, #PSS score
                        "anxiety" = anxiety$score, #GAD-7 score
                        "depression" = depression$score) # CESD-R score

composite$total_score <- composite$stress + composite$anxiety + composite$depression 
composite$date <- as.character(composite$date)

## Daily Data 
colnames(daily.data)

daily <- daily.data %>% 
  summarise(date = lubridate::as_date(StartDate),
            daily.anxiety = Q1_1,
            daily.depression = Q1_2,
            daily.focus = Q1_3,
            daily.lonley = Q1_4)

daily$daily.anxiety <- as.numeric(as.character(daily$daily.anxiety))
daily$daily.depression <- as.numeric(as.character(daily$daily.depression))
daily$daily.focus <- as.numeric(as.character(daily$daily.focus))
daily$daily.lonley <- as.numeric(as.character(daily$daily.lonley))

daily$daily_score <- daily$daily.anxiety + daily$daily.depression + daily$daily.focus + daily$daily.lonley

## Merge 

social_daily <- social.screen %>% 
  inner_join(daily, by = "date" ) %>% 
  select(-date)


  


  
