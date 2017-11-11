library(tidyverse)
library(tidytext)
library(RmecabKo)
library(stringr)
library(ggplot2)

songListLyrics <- read.csv("melon_ranking_lyrics_1964-2016.csv", stringsAsFactors = FALSE)

songListLyrics <- songListLyrics %>% unnest_tokens(word, lyric, token = token_words)

songListLyrics %>% anti_join(stopwords_ko) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

# wordcloud

library(wordcloud)

songListLyrics %>% anti_join(stopwords_ko) %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100, family = "Apple SD Gothic Neo"))

songListLyrics %>% anti_join(stopwords_ko) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head(20) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(family = "Apple SD Gothic Neo"))

# Words in Decade

songListLyrics <- songListLyrics %>% 
  na.omit() %>% 
  mutate(decade = floor(year / 10) * 10) 

lyricsWords <- songListLyrics %>% 
  unnest_tokens(word, lyric, token = token_words)

lyricsWords %>% 
  count(word, sort = TRUE)

wordsByDecade <- lyricsWords %>% 
  count(decade, word, sort = TRUE) %>% 
  ungroup()

wordsByDecade

tfIdf <- wordsByDecade %>% 
  bind_tf_idf(word, decade, n) %>% 
  arrange(desc(tf_idf))

tfIdf

tfIdf %>% 
  group_by(decade) %>% 
  top_n(12, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = decade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ decade, scales = "free") +
  ylab("tf-idf") +
  coord_flip() +
  theme(axis.text.y = element_text(family = "Apple SD Gothic Neo"))

library(widyr)

decadeCors <- wordsByDecade %>% 
  pairwise_cor(decade, word, n, sort = TRUE)

decadeCors

library(ggraph)
library(igraph)

decadeCors %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()