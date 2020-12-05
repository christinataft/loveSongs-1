library(tidyverse)
library(lexicon)

song.data <- read_csv("./datasets/songs_lyrics_short.csv",
                      col_names = c(
                        "album",
                        "chart.date",
                        "love.song",
                        "lyrics",
                        "release.date",
                        "song.title",
                        "artist"
                      ),
                      col_types = cols(
                        album = col_character(),
                        chart.date = col_date("%d/%m/%Y"),
                        love.song = col_double(),
                        lyrics = col_character(),
                        release.date = col_date("%d/%m/%Y"),
                        song.title = col_character(),
                        artist = col_character()
                      ),
                      skip = 1)

view(song.data)

# Let's work on cleaning the dataset. First, 

song.data.clean <- song.data %>%
  mutate(lyrics = str_remove_all(lyrics, "\\[.+?\\]")) %>%
  mutate(lyrics = str_remove_all(lyrics, "\\(.+?\\)")) %>%
  mutate(lyrics = str_replace_all(lyrics, "â€™", "'")) %>% 
  mutate(lyrics = str_to_lower(lyrics)) %>%
  view()

# grady_augmented is a dataset containing 122,806 english words.

song.data.clean %>%
  filter(lyrics)

# Function to expand contractions - Look deeper into this

FixContractions <- function(string) {
  string <- str_replace_all(string, "won't", "will not")
  string <- str_replace_all(string, "can't", "cannot")
  string <- str_replace_all(string, "n't", " not")
  string <- str_replace_all(string, "'ll", " will")
  string <- str_replace_all(string, "'re", " are")
  string <- str_replace_all(string, "'ve", " have")
  string <- str_replace_all(string, "'m", " am")
  string <- str_replace_all(string, "'d", " would")
  string <- str_replace_all(string, "n'", "ng")
  string <- str_replace_all(string, "'cause", "because")
  string <- str_replace_all(string, "it's", "it is")
  string <- str_replace_all(string, "'s", "")
  string <- str_replace_all(string, "'less", "unless")
  return(string)
}

FixContractions(song.data.clean[[6,4]])

song.data.clean[[6,4]] %>%
  FixContractions() %>%
  str_extract_all("[^ -.,!]+")

