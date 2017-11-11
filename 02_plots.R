library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)

songListLyrics <- read.csv("melon_ranking_lyrics_1964-2016.csv", stringsAsFactors = FALSE)

# Basic

songListLyrics <- songListLyrics %>% 
  mutate(decade = floor(year / 10) * 10) %>% 
  mutate(wordCount = sapply(.$lyric, function(x) length(strsplit(x, " ")[[1]]))) %>% 
  mutate(uniqueWordCount = sapply(.$lyric, function(x) length(unique(strsplit(x, " ")[[1]])))) %>% 
  mutate(inverseDensity = round(wordCount / uniqueWordCount, 4)) %>% 
  mutate(density = round(uniqueWordCount / wordCount, 4) * 100) %>% 
  filter(wordCount > 5)

# Word counts

songListLyrics %>% 
  ggplot(aes(year, wordCount)) +
  geom_point(color = "#2DA58A", alpha = .4, size = 4) +
  labs(title = "Words per Song (Total)",
       x = NULL, y = "Count") +
  annotate("text", x= 1990, y = -10, label = "Melon Chart Year 1964-2016") +
  stat_smooth(color = "black", se = FALSE, method = "lm")
  
songListLyrics %>% 
  ggplot(aes(year, uniqueWordCount)) +
  geom_point(color = "#1B687E", alpha = .4, size = 4) +
  labs(title = "Words per Song (Unique)",
       x = NULL, y = "Count") +
  annotate("text", x= 1990, y = -10, label = "Melon Chart Year 1964-2016") +
  stat_smooth(color = "black", se = FALSE, method = "lm")

# Top artists

as_tibble(data.frame(table(songListLyrics$artist))) %>% 
  mutate(Artist = as.character(Var1)) %>% 
  select(-Var1) %>% 
  arrange(-Freq) %>% 
  top_n(20, Freq) %>% 
  ggplot(aes(reorder(Artist, Freq), Freq)) +
  geom_bar(stat = "identity", fill = "#1B687E") +
  labs(title = "Number of Songs, Top 20 Artists", y = NULL, x = NULL) +
  geom_text(aes(label = Freq) , hjust = -0.25) +
  annotate("text", y = 30, x = 4, label = "Melon Chart\n1964-2016") +
  theme(axis.text.y = element_text(family = "Apple SD Gothic Neo")) +
  coord_flip()

as_tibble(data.frame(table(songListLyrics$artist))) %>% 
  mutate(Artist = as.character(Var1)) %>% 
  select(-Var1) %>% 
  arrange(-Freq) %>% 
  ggplot(aes(Freq)) +
  geom_bar(fill = "#1B687E") +
  labs(title = "Number of Songs per Artists", y = NULL, x = NULL) +
  annotate("text", y = 150, x = 30, label = "Melon Chart\n1964-2016")

# 2+ artists

multiples <- songListLyrics[grepl(",", songListLyrics$artist), ]
as.tibble(data.frame(table(multiples$year))) %>% 
  rename(Year = Var1) %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  ggplot(aes(Year, Freq)) +
  geom_bar(stat = "identity", fill = "#2DA58A") +
  ylim(c(-2, 25)) +
  labs(title = "Songs Featuring 2+ Artists",
       y = NULL, x = NULL) +
  annotate("text", x = 1990, y = -2, label = "Melon Chart 1964-2016")

# Artists

artists <- as_tibble(data.frame(table(songListLyrics$artist))) %>% 
  mutate(artist = as.character(Var1)) %>% 
  select(-Var1) %>% 
  arrange(-Freq)
  
# Careers

keeps <- as_tibble(data.frame(table(songListLyrics$artist))) %>% 
  mutate(Artist = as.character(Var1)) %>% 
  select(-Var1) %>% 
  arrange(-Freq) %>% 
  top_n(20, Freq) %>% 
  select(Artist)

keeps2 <- as_tibble(data.frame(table(songListLyrics$artist))) %>% 
  mutate(Artist = as.character(Var1)) %>% 
  select(-Var1) %>% 
  arrange(-Freq) %>% 
  filter(Freq > 5) %>% 
  select(Artist)

careers <- NULL

for (Artist in keeps2[[1]]) {
  sub <- songListLyrics %>%
    filter(artist == Artist) %>% 
    na.omit() %>% 
    arrange(-year)
  start <- sub %>% select(year) %>% slice(n()) %>% as.numeric()
  end <- sub %>% select(year) %>% slice(1) %>% as.numeric()
  span <- end - start
  row <- data.frame(Artist, start, end, span)
  careers <- rbind(careers, row)
}

careers %>% 
  left_join(artists, by = c("Artist" = "Var1")) %>% 
  mutate(rate = round(Freq / span, 2)) %>% 
  ggplot(aes(span, rate)) +
  geom_point(color = "#2DA58A", size = 4, alpha = 0.75) +
  labs(y = "Avg. Songs per Year",
       x = "Career Span",
       title = "Song Frequency by Carrer Span") +
  annotate("text", hjust = 0, y = 4.25, x = 25, label = "Melon Chart 1964-2016\nartists with n > 5 songs\navg. song per year = \ncharted hits / career span (yrs)") +
  theme_bw()

as_tibble(careers) %>% 
  right_join(keeps) %>% 
  ggplot(aes(color = Artist)) +
  geom_segment(aes(x = start, xend = end, y = Artist, yend = Artist), color = "#20687d", size = 6) +
  geom_text(aes(x = start + .25, hjust = 0, vjust = 0.36, y = Artist, label = paste(Artist, " (", span, " yrs)")), color = "#FFFFFF", family = "Apple SD Gothic Neo") +
  theme_bw() +
  annotate("text", y = 2, x = 1967, label = "Melon Chart 1964-2016") +
  labs(y = NULL, x = NULL, 
       title = "Career Spans of Top 20 Most-Charted Artists") +
  scale_y_discrete(breaks = NULL) +
  theme(legend.position = "none", panel.grid.major = element_blank())
