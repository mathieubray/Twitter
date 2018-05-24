setwd("C:/Users/braym/Box Sync/Personal/Twitter")

library(rtweet)
library(tidyverse)
library(lubridate)

tweets <- get_timeline("mathieubray", n = 3)

date <- as.character(now()) %>%
  str_replace_all(pattern=" ", replacement = "_") %>%
  str_replace_all(pattern=":", replacement = "_")

write_csv(tweets, paste0("AutoTrackedTweets/",date,".csv"))
