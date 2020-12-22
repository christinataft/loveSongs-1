# First attempt at scraping songs

library(rvest)
library(tidyverse)

# We want to get a substantial list of modern pop songs (say for the past 10 years)
# First, let's scrape the billboard top 100 website for the top 100 songs each week for the past 10 years.
# We'll use the rvest package to help us with this.

# Let's try with one page
url.010110 <- "https://www.billboard.com/charts/hot-100/2010-01-01"

# Scrape for songs; you can get the html class by inspecting element in your web browser
read_html(url.010110) %>%
  html_nodes(".chart-element__information__song") %>%
  html_text() %>%
  view()

# Get the corresponding artists (since song names are not unique; e.g. "Let it go" by James Bay is a love song but "Let it go" from Frozen isn't)
read_html(url.010110) %>%
  html_nodes(".chart-element__information__artist") %>%
  html_text() %>%
  view()


# Let's try applying this to more dates. Billboard top 100 URLs have a very nice format which makes this easy by iterating over a list of dates.
date.list <- seq(as.Date("2010-01-01"), as.Date("2010-04-01"), by="weeks")

ScrapeSongs <- function(date) {
  url <- str_c("https://www.billboard.com/charts/hot-100/",date)
  songs <- read_html(url) %>%
    html_nodes(".chart-element__information__song") %>%
    html_text()
}  # function for scraping songs

ScrapeArtists <- function(date) {
  url <- str_c("https://www.billboard.com/charts/hot-100/",date)
  artists <- read_html(url) %>%
    html_nodes(".chart-element__information__artist") %>%
    html_text()
}  # function for scraping artists

output.songs <- vector("list", length = length("date.list")) 
for (i in seq_along(date.list)) {
  output.songs[[i]] <- ScrapeSongs(date.list[i])
}  

output.artists <- vector("list", length = length("date.list")) 
for (i in seq_along(date.list)) {
  output.artists[[i]] <- ScrapeArtists(date.list[i])
}  

song.tibble <- tibble(  
  song=unlist(output.songs),
  artist=unlist(output.artists)
) 


# From testing, it seems we can only scrape about 15 pages at a time before we get a HTTP error 429. 
# Let's try again, and stop the scraping for 60 seconds every 15 pages we scrape.
# WE CAN IMPROVE THE SCRAPING FUNCTION! If we scrape the htmls, save them and then get the tags from there we can cut the time in about half

date.list.10 <- seq(as.Date("2010-01-01"), as.Date("2020-11-23"), by="weeks") # List of dates for the last 10 years (569 weeks)

output.songs.10 <- vector("list", length = length(date.list.10))
count <- 0  # count how many pages we have scraped consecutively
for (i in seq_along(date.list.10)){
  if (count <= 14){
    output.songs.10[[i]] <- ScrapeSongs(date.list.10[i])
    count <- count + 1
  } else {
    Sys.sleep(60)  # suspends execution of R commands for a minute
    output.songs.10[[i]] <- ScrapeSongs(date.list.10[i])
    count <- 0  # reset count after 15 pages
  }
}  # Loop to scrape top100 songs for the last 10 years. This is going to take at least 38 minutes (not including actual scraping time)

#Repeat for artists
output.artists.10 <- vector("list", length = length(date.list.10))
count = 0
for (i in seq_along(date.list.10)){
  if (count <= 14){
    output.artists.10[[i]] <- ScrapeArtists(date.list.10[i])
    count <- count + 1
  } else {
    Sys.sleep(60)
    output.artists.10[[i]] <- ScrapeArtists(date.list.10[i])
    count <- 0
  }
}

# Put songs, artists and date into a tibble
song.tibble <- tibble(
  song = unlist(output.songs.10),  # collapse nested list of songs
  artist = unlist(output.artists.10),
  date = rep(date.list.10, each = 100)  # assign date of the scraped page to each song
) %>% distinct(across(c(song, artist)), .keep_all = TRUE)  # drop duplicate songs. Only first instance of the song will be in the final tibble.

# We have 5174 unique songs. Check for any duplicates
song.tibble %>% 
  count(song, artist) %>%
  arrange(desc(n))

write_csv(song.tibble, "./datasets/songs_unlabelled.csv", col_names = TRUE) # Write list of songs to csv for labelling
