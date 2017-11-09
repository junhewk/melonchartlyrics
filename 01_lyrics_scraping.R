devtools::install_github("johndharrison/wdman")
install.packages("RSelenium")
install.packages("rvest")
install.packages("tidyverse")
install.packages("stringr")
install.packages("httr")

library(wdman)
library(RSelenium)
library(rvest)
library(tidyverse)
library(stringr)
library(httr)

rD <- rsDriver(verbose = FALSE)

remDr <- rD$client

songList <- data_frame()

for (year in seq(1974, 2016, 1)) {
  remDr$navigate(paste0("http://www.melon.com/chart/age/index.htm?chartType=YE&chartGenre=KPOP&chartDate=", year))
  
  webElem <- remDr$findElement(using = "css selector", value = "form#frm")
  
  current <- data_frame(year = year,
                        title = read_html(webElem$getPageSource()[[1]]) %>% 
                          html_node("form#frm table") %>% 
                          html_nodes("div.wrap_song_info div.rank01") %>% 
                          html_text() %>% 
                          str_trim("both") %>% 
                          str_conv("UTF8"),
                        artist = read_html(webElem$getPageSource()[[1]]) %>% 
                          html_node("form#frm table") %>% 
                          html_nodes("span.checkEllipsis") %>% 
                          html_text() %>% 
                          str_conv("UTF8"),
                        song_id = read_html(webElem$getPageSource()[[1]]) %>% 
                          html_node("form#frm table") %>% 
                          html_nodes("div.wrap a.btn") %>% 
                          html_attr("onclick") %>% 
                          str_extract_all("[0-9]+") %>% 
                          unlist())
  
  songList <- dplyr::bind_rows(songList, current)
  Sys.sleep(60)
}

lyrics <- data_frame()

for (id in songList$song_id) {
  newLyrics <- data_frame(id = id,
                          lyric = read_html(content(GET(paste0("http://www.melon.com/song/detail.htm?songId=", id)), "text")) %>% 
                            html_node("div.lyric") %>% 
                            gsub(pattern = "<.*?>", replacement = "\n") %>% 
                            str_replace_all("\t", ""))
                            
  lyrics <- dplyr::bind_rows(lyrics, newLyrics)

  Sys.sleep(sample(seq(1, 6, 1), 1))
}

songListLyrics <- songList %>% 
  left_join(lyrics, by = c("song_id" = "id")) %>% 
  unique()

songListLyrics$lyric <- str_replace_all(songListLyrics$lyric, "[\t\r\n]+", " ") %>% 
  str_replace_all("&gt;", " ") %>% 
  str_replace_all("&lt;", " ") %>% 
  str_trim("both")

# write_csv(songListLyrics, path = "melon_ranking_lyrics_1964-2016.csv")
