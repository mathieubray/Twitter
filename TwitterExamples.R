library(rtweet)
library(tidyverse)

# Collect Tweets from User

tweets <- get_timeline('mathieubray', n=500)

tweets.table <- tweets %>% 
  select(text,favorite_count) %>% 
  rename(Text=text, Favorites=favorite_count)


# Collect User Followers
twitter.user <- lookup_users("mathieubray")

twitter.followers <- get_followers("mathieubray")
twitter.followers.data <- lookup_users(twitter.followers$user_id)


# Search Twitter
searched.tweets <- search_tweets('nhl+challenge', n=50)



# Sentiment Analysis, Based on https://github.com/pablobarbera/social-media-workshop

# Collect tweets

tweets <- search_tweets("Subban", n=3000)


# Save tweets to table

tweets.table <- tweets %>%
  filter(!is_retweet, is.na(in_reply_to_status_status_id))


# Clean tweets

tweets.text <- tweets.table$text %>%
  str_replace_all(pattern="[[:punct:]]",replacement="") %>%
  str_replace_all(pattern="[[:digit:]]",replacement="") %>%
  iconv("latin1", "ASCII", sub="") %>% #Remove Emojis
  tolower %>%
  str_replace_all(pattern="http\\w+ *",replacement="") %>% #Remove links
  str_replace_all(pattern="\n",replacement=" ") %>%
  str_trim %>%
  unique 

split.text <- tweets.text %>%
  str_split(" ")


# Load postive and negative words

sentiment <- sentiments %>%
  filter(lexicon == "bing")

pos.words <- sentiment %>%
  filter(sentiment =="positive") %>%
  .$word

neg.words <- sentiment %>%
  filter(sentiment =="negative") %>%
  .$word


classify <- function(words){
  
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  
  return(pos.matches - neg.matches)
}


# Obtain sentiment scores for each tweet

scores <- map_dbl(split.text, classify)

n <- length(scores)
positive <- sum(scores>0)/n*100
negative <- sum(scores<0)/n*100
neutral <- 100 - positive - negative
cat(n, "tweets:", positive, "% positive,",
    negative, "% negative,", neutral, "% neutral")

tweet.scores <- tibble(Tweet=tweets.text, Score=scores) # Merge into data frame

polarizing.tweets <- tweet.scores %>% filter(abs(Score) > 2) # Extract the most polarizing tweets



# Maps

tweets.df <- stream_tweets(lookup_coords("new york, NY", "country:US"), timeout=30)

tweets.coords <- tweets.df %>%
  filter(!is.na(coordinates)) %>%
  select(coordinates)

coerce.to.data.frame <- function(x){
  return(as.data.frame(t(x), stringsAsFactors=F))
}

coords <- tweets.coords %>%
  .$coordinates %>%
  str_split(" ") %>%
  map_df(coerce.to.data.frame) %>%
  mutate(lat = as.numeric(V1), lon = as.numeric(V2))

map.data <- map_data("state") %>%
  filter(region == "new york")

ggplot() +
  geom_polygon(data = map.data, aes(x = long, y = lat, group=group), fill=NA, color="black") +
  geom_point(data = coords, aes(x = lon, y = lat), size=3, color="blue") +
  theme_void()