library(tidytext)
library(tidyverse)
library(lubridate)
library(igraph)
library(ggraph)

news <- read_csv("india-news-headlines.csv")

news %>% pull(headline_category) %>% str_to_lower(.) ->news$headline_category
news %>% pull(headline_text) %>% str_to_lower(.) -> news$headline_text


# convert the date column into "date" data-type
news %>% pull(publish_date) %>% ymd(.) -> news$publish_date


# filter out headlines related to modi, bjp, congress
news %>% filter(str_detect(news$headline_text,"(narendra modi)|(bjp)|(congress)|(manmohan singh)")) -> politics_ind_tidy
  


# find out how many categories are there in the dataset
politics_ind_tidy %>% pull(headline_category) %>% unique(.) %>%
  as_data_frame(.) %>% count()

# build an alternative str_detect function
str_detect_politics <- function(string){
  politics_ind_tidy %>% pull(headline_text) %>%
    str_detect(., string)
}

# convert the categories into 3, congress, bjp and prime_minister 
politics_ind_tidy %>%
   mutate(headline_category = case_when(
     str_detect_politics("(narendra modi)|(manmohan singh)") ~ "prime_minister",
     str_detect_politics("(bjp)") ~ "political_party",
     str_detect_politics("(congress)") ~ "political_party"
   ), headline_sub_category = case_when(
     str_detect_politics("(manmohan singh)") ~ "manmohan",
     str_detect_politics("(narendra modi)") ~ "modi",
     str_detect_politics("(bjp)") ~ "bjp",
     str_detect_politics("(congress)") ~ "congress"
   ) ) -> politics_ind_tidy

# convert it to tidy format
politics_ind_tidy %>% group_by(headline_category) %>%
  unnest_tokens(., headline_words, headline_text) -> politics_ind_tidy



# merge categories into more managable ones
#politics_ind_tidy %>% pull(headline_category) %>% str_extract("[A-z0-9]{1,}\\.") %>%
 # str_remove("\\.") -> politics_ind_tidy$headline_category

# convert NA category into Unspecified
#politics_ind_tidy %>% replace_na(list(headline_category ="Unspecified")) -> politics_ind_tidy

# save the tidy data
write_csv(politics_ind_tidy, "politics_ind_tidy.csv")

# read the tidy news csv file
politics_ind_tidy <- read_csv("politics_ind_tidy.csv")


# calculate word frequencies for words that are not stop words
politics_ind_tidy %>%
  group_by(headline_category) %>%
  anti_join(stop_words, by = c("headline_words" = "word")) %>%
  count(headline_words, sort = TRUE) %>%
  filter(n > 1000) %>%
  ggplot(., aes(headline_words, n)) +
  geom_col(aes(fill = headline_category)) +
  coord_flip() +
  theme(legend.position = "bottom")

# sentiment analysis
politics_ind_tidy %>%
  #mutate( ind = as_factor(paste(year(publish_date),month(publish_date)))) %>%
  inner_join(get_sentiments("bing"), by = c("headline_words" = "word")) %>%
  count(headline_sub_category, index = year(publish_date) ,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) -> politics_sentiment

politics_sentiment %>%
  ggplot(., aes(index, sentiment, fill = headline_sub_category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~headline_sub_category, nrow = 3, scales = "free_y")
  
  
# calculate tf*idf scores and find out unique valuable words
# calculate the count for each group and each word
politics_ind_tidy %>%
  ungroup() %>%
  count(headline_sub_category, headline_words, sort = TRUE) %>%
  ungroup()-> head_words


  head_words %>%
  group_by(headline_sub_category) %>%
  summarise(total = sum(n)) -> total_words
  
  head_words <- left_join(head_words, total_words)
  
  head_words %>% bind_tf_idf(headline_words, headline_sub_category, n) -> head_words
  
  head_words %>% 
    arrange(desc(tf_idf)) %>%
    mutate(headline_words = factor(headline_words, levels = rev(unique(headline_words)))) %>%
    group_by(headline_sub_category) %>%
    #filter(tf_idf >=.1) %>%
    top_n(10) %>%
    ungroup() %>%
    ggplot(aes(headline_words, tf_idf, fill = headline_sub_category)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~headline_sub_category, ncol = 2, scales = "free") +
    coord_flip()
    
# see the total of counts over time mentioning modi and manmohan
news %>%
  filter(str_detect(news$headline_text,"(narendra modi)|(manmohan singh)")) %>%
  mutate(headline_category = str_extract(.$headline_text, "(narendra modi)|(manmohan singh)")) -> modi_manmohan
  
modi_manmohan %>%
  group_by(year =year(publish_date)) %>%
  count(year, headline_category) %>%
  ggplot(., aes(year , n, group = headline_category, color = headline_category)) +
  geom_line(size =1)


# bigrams
news %>%
  filter(str_detect(news$headline_text,"(narendra modi)|(bjp)|(congress)|(manmohan singh)")) %>%
  select(-headline_category) %>%
  unnest_tokens(bigram, headline_text, n = 2, token = "ngrams") %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) -> political_bigrams
  

# bigram count 
set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.05, "inches"))


political_bigrams %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 170) %>%
  graph_from_data_frame() %>%
  ggraph(., layout = "fr") +
  geom_edge_link(edge_alpha = .55, show.legend = FALSE,
                 arrow = a, end_cap = circle(.05, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# do topic modelling
