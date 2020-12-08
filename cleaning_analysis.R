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
  ggplot(aes(chart.date)) +
  geom_histogram(position = "fill", aes(fill = love.song), bins = 10) # Fix the date labels, change colors

# Amongst songs that turn out to be top 100 songs in the past decade, when are they released? and 
# how many of them are love songs?

song.data.clean %>%
  filter(release.date >= date("01-01-2010")) %>%
  mutate(release.date = update(release.date, year=2010)) %>%
  ggplot(aes(release.date)) +
  geom_histogram(binwidth = (1)) +
  scale_x_date(date_labels="%B", date_breaks = "1 month")

song.data.clean %>%
  filter(release.date >= date("01-01-2010")) %>%
  mutate(release.date = update(release.date, year=2010)) %>%
  ggplot(aes(release.date)) +
  geom_histogram(aes(fill=love.song), binwidth = (1)) +
  scale_x_date(date_labels="%B", date_breaks = "1 month")

# Is it possible that releaseing songs at a particular time leads to them being more successful? Probably not.
# Let's look at the dates with the most top 100 songs:

hot.dates <- song.data.clean %>%
  count(release.date) %>%
  filter(n > 5) %>% .$release.date

song.data.clean %>%
  filter(release.date %in% hot.dates) %>% view()

# We can see that they come from largely the same artists. OK, but is there a best month to release albums?

song.data.clean %>%
  filter(release.date >= date("01-01-2010")) %>%
  mutate(release.date = update(release.date, year=2010)) %>%
  ggplot(aes(release.date)) +
  geom_histogram(binwidth = (30)) +
  scale_x_date(date_labels="%B", date_breaks = "1 month")

# For songs that end up charting on the top 100, is there any correlation between them being love songs and the 
# time of the year they first become popular? 
# Find a way to group by month!
song.data.clean %>%
  filter(chart.date > date("07-01-2010")) %>% # Take out the first week of jan 2010 because of "spillover from prev year" from last year
  mutate(chart.date = update(chart.date, year=2010)) %>%
  ggplot(aes(chart.date)) +
  geom_histogram(aes(fill=love.song), binwidth = (7)) +
  scale_x_date(date_labels="%B", date_breaks = "1 month")


# Now let's do some visualizations with the words themselves!

# Tokenization with tidy text. Now, each word is present as a row. We will use this in some models (but not all)
song.data.tidy <- song.data.clean %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) >= 3)

# The most popular words, depending on whether they are love songs or not

popular.words <- song.data.tidy %>%
  group_by(love.song) %>%
  count(word, love.song, sort=TRUE) %>%
  mutate(word = reorder_within(word, n, love.song)) %>%
  filter(rank(desc(n)) <= 20)


popular.words %>% view()

popular.words %>%
  ggplot(aes(word, n, fill = love.song)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~love.song, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Total words in all songs",
       title = "What were the most popular words in love songs and other songs?")

# We see alot of words like n***a in the non-love songs - It's probably because rap music tends to be much
# more verbose, resulting in them being overrepresented here. To compensate for this, we can instead use
# a metric like word DENSITY per each song.
