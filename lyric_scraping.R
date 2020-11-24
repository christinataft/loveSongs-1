library(tidyverse)
library(geniusr)


songs <- read_csv("./datasets/songs_labelled.csv", #import dataset
                  col_types = cols(
                    song = col_character(),
                    artist = col_character(),
                    date = col_date(format = "%d/%m/%Y"),
                    loveSong = col_double()
                  ))

genius_token() # Set up genius API

scrapeLyrics <- function(song, artist) {
  tryCatch(
    {
      lyrics = ""
      while (lyrics == "") {
        songTibble = get_lyrics_search(artist_name = artist, song_title = song)
        lyrics = str_c(pull(songTibble, line), collapse = " ") # Get just lyrics, concat into one string
      }
      closeAllConnections() # Close the connection
      return(lyrics)
    }, 
    error=function(cond) {
      # If error 404 (genius can't find the lyrics), return NA
      closeAllConnections()
      return(NA)
    }
  )
} # Function to scrape lyrics

# Let's prepare the dataset for scraping (Artist and song names should fit the values on genius)
view(songs)

# By doing a few searches, it seems genius uses both artist names for collaborations (xxx & yyy)
# but only uses the first artist for features (xxx featuring yyy)
# Also, xxx + yyy and xxx X yyy are usually replaced with xxx & yyy

songs <- songs %>% 
  mutate(artist = str_remove(artist, " [Ff]eaturing.*| Duet.*"),  # Remove features 
         artist = str_replace(artist, " \\+ | X ", " & ")) # Change format for colabs

# Start scraping!
start <- Sys.time()
songsLyrics <- songs %>%
    mutate(lyrics = map2_chr(song, artist, scrapeLyrics))
end <- Sys.time()
end-start

# Write to csv, fill in missing lyrics
write_csv(songsLyrics, "./datasets/songs_labelled_lyrics.csv", col_names = TRUE)