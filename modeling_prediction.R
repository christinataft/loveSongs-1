library(tidyverse)
library(lubridate)
library(tidytext)
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
                        love.song = col_factor(),
                        lyrics = col_character(),
                        release.date = col_date("%d/%m/%Y"),
                        song.title = col_character(),
                        artist = col_character()
                      ),
                      skip = 1)

song.data.clean <- song.data %>%
  mutate(lyrics = str_remove_all(lyrics, "\\[.+?\\]")) %>%
  mutate(lyrics = str_remove_all(lyrics, "\\(.+?\\)")) %>%
  mutate(lyrics = str_replace_all(lyrics, "â€™", "'")) %>% 
  mutate(lyrics = str_to_lower(lyrics))


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

# remove contractions and punctuation
song.data.clean <- song.data.clean %>%
  mutate(lyrics = lyrics %>% FixContractions() %>% str_replace_all("[^a-zA-Z0-9\\- ]", " ")) 

# Convert chart.dates to lubridate dates

song.data.clean <- song.data.clean %>%
  mutate(chart.date = as_date(chart.date),
         release.date = as_date(release.date))

song.data.tidy <- song.data.clean %>%
  unnest_tokens(word, lyrics) %>%
  filter(word %in% grady_augmented | word %in% profanity_zac_anger) %>% # Keep only words in the lexicon
  anti_join(stop_words) %>%
  distinct() %>% 
  filter(nchar(word) >= 3)

# Goal - we have a string separated by spaces in each column of song.data.clean. We want to turn that into a string that contains only
# words that are found in these two lists: grady_augmented and profanity_zac_anger. Poor algorithm design and use of loops is fine
# since this may be the best we can do with R. 

ExtractLyrics <- function(lyrics){
  for (i in seq_along(lyrics)) {
    output = vector(mode="character", length=length(lyrics))
    output[[i]] <- ifelse(lyrics[[i]] %in% grady_augmented | lyrics[[i]] %in% profanity_zac_anger, lyrics[[i]], NULL)
    return (output)
  } # This will be horrible in R! extremely slow.
} # This function is not optimized AT ALL!!! But I don't know how to make it run better until I take a proper algorithms course
