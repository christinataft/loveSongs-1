library(tidyverse)
library(lubridate)
library(tidytext)

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

view(song.data)

# Let's work on cleaning the dataset. First, 

song.data.clean <- song.data %>%
  mutate(lyrics = str_remove_all(lyrics, "\\[.+?\\]")) %>%
  mutate(lyrics = str_remove_all(lyrics, "\\(.+?\\)")) %>%
  mutate(lyrics = str_replace_all(lyrics, "â€™", "'")) %>% 
  mutate(lyrics = str_to_lower(lyrics)) %>%
  view()

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
  str_replace_all("[^a-zA-Z0-9\\- ]", " ")

# remove contractions and punctuation
song.data.clean <- song.data.clean %>%
  mutate(lyrics = lyrics %>% FixContractions() %>% str_replace_all("[^a-zA-Z0-9\\- ]", " "))

# Convert chart.dates to lubridate dates

song.data.clean <- song.data.clean %>%
  mutate(chart.date = as_date(chart.date),
         release.date = as_date(release.date))

## Exploratory data analysis
# What percentage of songs charting each year are love songs?

song.data.clean %>%
  count(year = year(chart.date), love.song)%>%
  ggplot(aes(year,n)) +)
  geom_col(position = "fill", aes(fill = love.song) # Fix the date labels, change colors

# Love songs by each week, zoomed in on 2010

song.data.clean %>%
  filter(year(chart.date) == 2010) %>%
  ggplot(aes(chart.date)) +
  geom_histogram(aes(fill = love.song), binwidth = (7)) + 
  scale_x_date(date_labels = "%B")

# Tokenization with tidy text. Now, each word is present as a row. We will use this in some models (but not all)
song.data.tidy <- song.data.clean %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) >= 3)

song.data.tidy
