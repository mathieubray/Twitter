library(rtweet)
library(tidyverse)

# Collect Francois Gagnon Tweets and Save to Table

tweets <- get_timeline('gagnonfrancois', n=3200)

tweets.table <- tweets %>% 
  filter(!is_retweet) %>%
  select(text,favorite_count) %>% 
  rename(Text=text,Favorites=favorite_count)

tweets.text <- tweets.table %>%
  .$Text %>%
  str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
  str_replace_all(pattern = "[[:digit:]]", replacement = "") %>%
  iconv("latin1", "ASCII", sub="") %>% #Remove Emojis
  tolower %>%
  str_replace_all(pattern = "http\\w+ *", replacement = "") %>% #Remove links
  str_trim

tweets.table$Text <- tweets.text

tweets.table$Simonac <- str_detect(tweets.table$Text,pattern="simonac") # Find all tweets where he says "Simonac!"

pct.simonac <- round(sum(tweets.table$Simonac)/nrow(tweets.table)*100,2) # Calculate the percentage of tweets where he says Simonac!


# count the words used by Francois Gagnon

word.list <- tweets.table$Text %>%
  str_split(pattern=" ") %>%
  unlist %>%
  tibble %>%
  set_names(c("Word"))

# Remove individual letters
acceptableWord <- function(word){
  
  n <- nchar(word)
  
  if (n<=1){
    if (word %in% letters[-1] | n==0){
      return(FALSE)
    }
  }
  
  return(TRUE)
}

word.list$Keep <- map_lgl(word.list$Word, acceptableWord)

word.counts <- word.list %>%
  filter(Keep) %>% 
  select(Word) %>% 
  group_by(Word) %>% 
  summarize(Number = n()) %>% 
  arrange(desc(Number))

