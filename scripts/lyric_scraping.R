library(tidyverse)
library(geniusr)


songs.tibble <- read_csv("./datasets/songs_labelled.csv",  #import dataset
                  col_types = cols(
                    song = col_character(),
                    artist = col_character(),
                    date = col_character(),
                    loveSong = col_double()
                  ))

view(songs.tibble)

genius_token()  # Set up genius API

# Let's prepare the dataset for scraping (Artist and song names should fit the values on genius)
view(songs.tibble)

# The scraper seems to just pull requests from the URL.

# By doing a few searches, it seems genius uses both artist names for collaborations (xxx & yyy)
# but only uses the first artist for features (xxx featuring yyy)
# Also, xxx + yyy and xxx X yyy are usually replaced with xxx & yyy

songs.tibble <- songs.tibble %>% 
  mutate(artist = str_remove(artist, " [Ff]eaturing.*| Duet.*"),  # Remove features 
         artist = str_replace(artist, " \\+ | X ", " & "), # Change format for colabs
         song = str_replace(song, ":", " "),
         song = str_remove(song, "\\(.+\\)"))

songs.tibble.lyrics <- tibble(song = 'NULL',
                              artist = 'NULL',
                              date = 'NULL',
                              lyrics = 'NULL',
                              loveSong = 0) # output


#Test
songs.tibble <- songs.tibble[1:10,]

ScrapeLyrics <- function(song, artist) {  # Function to scrape lyrics
  tryCatch( {
    lyrics <- ""
    while (lyrics == "") {
      song.tibble <- get_lyrics_search(artist_name = artist, song_title = song)
      lyrics <- str_c(pull(song.tibble, line), collapse = " ")  # Get just lyrics, concat into one string
    }
    closeAllConnections()  # Close the connection
    return(lyrics)
  }, 
  error=function(cond) {
    # If error 404 (genius can't find the lyrics), return NA
    closeAllConnections()
    return(NA)
  }
  )
}  

# scrape
for (i in seq_along(songs.tibble)) {
  rolling.pct <-  i / length(songs.tibble) * 100
  cat(rolling.pct, "% is complete. ", sep='')
  song <- songs.tibble[[i,1]]
  artist <- songs.tibble[[i,2]]
  
  lyrics <- ScrapeLyrics(song, artist)
 
  songs.tibble.lyrics %>% add_row(song = songs.tibble[[i,1]],
                                  artist = songs.tibble[[i,2]],
                                  date = songs.tibble[[i,3]],
                                  lyrics = lyrics,
                                  loveSong = songs.tibble[[i,4]])
}

# Some problems with this - try using python instead. 
write_csv(songs.tibble.lyrics, "./datasets/songs_labelled_lyrics.csv", col_names = TRUE)