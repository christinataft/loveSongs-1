library(rvest)
library(tidyverse)

# We want to get a substantial list of modern pop songs (say for the past 10 years)
# First, let's scrape the billboard top 100 website for the top 100 songs each week for the past 10 years.
# We'll use the rvest package to help us with this.

# Let's try with one page
url010110="https://www.billboard.com/charts/hot-100/2010-01-01"

# Scrape for songs; you can get the html class by inspecting element in your web browser
songs010110 <- read_html(url010110) %>%
  html_nodes(".chart-element__information__song") %>%
  html_text()

# Get the corresponding artists (since song names are not unique; e.g. "Let it go" by James Bay is a love song but "Let it go" from Frozen isn't)
artists010110 <- read_html(url010110) %>%
  html_nodes(".chart-element__information__artist") %>%
  html_text()


# Let's try applying this to more dates. Billboard top 100 URLs have a very nice format which makes this easy by iterating over a list of dates.
dateList <- seq(as.Date("2010-01-01"), as.Date("2010-04-01"), by="weeks")

scrapeSongs <- function(date) {
  url <- str_c("https://www.billboard.com/charts/hot-100/",date)
  songs <- read_html(url) %>%
    html_nodes(".chart-element__information__song") %>%
    html_text()
} # function for scraping songs

scrapeArtists <- function(date) {
  url <- str_c("https://www.billboard.com/charts/hot-100/",date)
  artists <- read_html(url) %>%
    html_nodes(".chart-element__information__artist") %>%
    html_text()
} # function for scraping artists

outputSongs <- vector("list", length = length("dateList")) 
for (i in seq_along(dateList)) {
  outputSongs[[i]] <- scrapeSongs(dateList[i])
} # Loop to scrape song names

outputArtists <- vector("list", length = length("dateList")) 
for (i in seq_along(dateList)) {
  outputArtists[[i]] <- scrapeArtists(dateList[i])
} # Loop to scrape artist names

songTibble <- tibble(  
  song=unlist(outputSongs),
  artist=unlist(outputArtists)
) # Tibble with song and artist names


# From testing, it seems we can only scrape about 15 pages at a time before we get a HTTP error 429. 
# Let's try again, and stop the scraping for 60 seconds every 15 pages we scrape.

dateList10 <- seq(as.Date("2010-01-01"), as.Date("2020-11-23"), by="weeks") # List of dates for the last 10 years (569 weeks)

outputSongs10 <- vector("list", length = length(dateList10))
count = 0 # count how many pages we have scraped consecutively
for (i in seq_along(dateList10)){
  if (count <= 14){
    outputSongs10[[i]] <- scrapeSongs(dateList10[i])
    count = count + 1
  } else {
    Sys.sleep(60) # suspends execution of R commands for a minute
    outputSongs10[[i]] <- scrapeSongs(dateList10[i])
    count = 0 # reset count after 15 pages
  }
} # Loop to scrape top100 songs for the last 10 years. This is going to take at least 38 minutes (not including actual scraping time)

#Repeat for artists
outputArtists10 <- vector("list", length = length(dateList10))
count = 0
for (i in seq_along(dateList10)){
  if (count <= 14){
    outputArtists10[[i]] <- scrapeArtists(dateList10[i])
    count = count + 1
  } else {
    Sys.sleep(60)
    outputArtists10[[i]] <- scrapeArtists(dateList10[i])
    count = 0
  }
}

# Put songs, artists and date into a tibble
songTibble <- tibble(
  song = unlist(outputSongs10), # collapse nested list of songs
  artist = unlist(outputArtists10),
  date = rep(dateList10, each = 100) # assign date of the scraped page to each song
) %>% distinct(across(c(song, artist)), .keep_all = TRUE) # drop duplicate songs. Only first instance of the song will be in the final tibble.

# We have 5174 unique songs. Check for any duplicates
songTibble %>% 
  count(song, artist) %>%
  arrange(desc(n))

write_csv(songTibble, "songs_unlabelled.csv", col_names = TRUE) # Write list of songs to csv for labelling
