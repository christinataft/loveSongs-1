library(tidyverse)
library(lubridate)
library(tidytext)
library(lexicon)

song.data <- read_csv("./datasets/songs_lyrics_labelled.csv",
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
  mutate(lyrics = str_to_lower(lyrics))

view(song.data.clean)


# Function to expand contractions - Look deeper into this. *I used a list of all words to achieve this)

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
  mutate(lyrics = lyrics %>% 
           FixContractions() %>% 
           str_replace_all("[^a-zA-Z0-9\\-' ]", " ") %>%
           str_replace_all("([a-zA-Z])\\1\\1",""))

song.data.clean %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  count(word) %>% 
  arrange(desc(n)) %>%
  view()

# Convert chart.dates to lubridate dates

song.data.clean <- song.data.clean %>%
  mutate(chart.date = as_date(chart.date),
         release.date = as_date(release.date))

# Write to csv
write_csv(song.data.clean, "datasets/song_data_clean.csv", col_names = TRUE)

## Exploratory data analysis
# Filter out not yet labelled songs.
song.data.explore <- song.data.clean %>%
  filter(!is.na(love.song))

library(RColorBrewer)

# What percentage of songs charting each year are love songs?

install.packages("RColorBrewer")

song.data.explore %>%
  count(year = year(chart.date), love.song) %>%
  mutate(love.song = if_else(love.song==0, "No", "Yes")) %>%
  ggplot(aes(year, n)) +
  geom_bar(stat = "identity", position = "fill", aes(fill = love.song)) + 
  labs(x = "Year", y = "Proportion", fill = "Love Song", title = "Yearly proportion of top-100 songs by song category") +
  scale_x_continuous(breaks = c(2010:2020)) + 
  scale_fill_brewer(palette = "PuRd") + 
  theme_dark()

# Amongst songs that turn out to be top 100 songs in the past decade, when are they released? and 
# how many of them are love songs?

song.data.explore%>%
  filter(release.date >= date("01-01-2010")) %>%
  mutate(release.date = update(release.date, year=2010)) %>%
  ggplot(aes(release.date)) +
  geom_histogram(binwidth = (1)) +
  scale_x_date(date_labels="%B", date_breaks = "1 month")

song.data.explore %>%
  filter(release.date >= date("01-01-2010")) %>%
  mutate(release.date = update(release.date, year=2010)) %>%
  ggplot(aes(release.date)) +
  geom_histogram(aes(fill=love.song), binwidth = (1)) +
  scale_x_date(date_labels="%B", date_breaks = "1 month")

# Is it possible that releaseing songs at a particular time leads to them being more successful? Probably not.
# Let's look at the dates with the most top 100 songs:

hot.dates <- song.data.explore %>%
  count(release.date) %>%
  filter(n > 5) %>% .$release.date

song.data.explore %>%
  filter(release.date %in% hot.dates) %>% view()

# We can see that they come from largely the same artists. OK, but is there a best month to release albums?

song.data.explore %>%
  filter(release.date >= date("01-01-2010")) %>%
  count(month = factor(month(release.date))) %>%
  ggplot(aes(month, n, fill=month)) +
  geom_bar(stat = "identity") + 
  scale_x_discrete("Month", labels = month(c(1:12), label = TRUE)) + 
  labs(y = "Count", title = "Number of top 100 hits per month across 10 years") +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "none")

# Now let's do some visualizations with the words themselves!

# Tokenization with tidy text. Now, each word is present as a row. We will use this in some models (but not all)
song.data.tidy <- song.data.explore %>%
  unnest_tokens(word, lyrics) %>%
  filter(word %in% grady_augmented | word %in% profanity_zac_anger) %>% # Keep only words in the lexicon
  distinct()

# The most popular words, depending on whether they are love songs or not

popular.words <- song.data.tidy %>%
  group_by(love.song) %>%
  count(word, love.song, sort=TRUE) %>%
  mutate(word = reorder_within(word, n, love.song)) %>%
  filter(rank(desc(n)) <= 45) %>%
  mutate(love.song = if_else(love.song == 0, "Other Songs", "Love Songs"))

popular.words %>% view()

popular.words %>%
  ggplot(aes(word, n, fill = love.song)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~love.song, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Count",
       title = "What were the most popular words in love songs and other songs?") +
  scale_fill_brewer(palette = "PuRd") + 
  theme_dark()

# Wee see some patterns here which are good! Love songs tend to have fewer explicit words. 
# However, the popular words in love songs tend to be popular in non-love songs as well.
# We see alot of words like n***a in the non-love songs - It's probably because rap music tends to be much
# more verbose, resulting in them being overrepresented here. To compensate for this, we can instead use
# a metric like word average DENSITY per each song - the number of time a word appears in a song divided 
# by the total number of lyrics in the song

# Now, let's choose the most popular 100 words in each song category, and sort them by their highest average
# density across songs.
word.density <- song.data.tidy %>%
  count(word, love.song) %>%
  group_by(love.song) %>%
  filter(rank(desc(n)) <= 100) %>% 
  left_join(song.data.clean) %>%
  mutate(total.words = str_count(lyrics, boundary("word"))) %>% 
  mutate(density = str_count(lyrics, word)/total.words) %>%
  select(love.song, word, density) %>%
  group_by(word, love.song) %>% 
  filter(n()>2) %>%
  summarise(mean.density = mean(density))

word.density %>%
  arrange(desc(mean.density)) %>%
  group_by(love.song) %>%
  filter(rank(desc(mean.density)) <= 45) %>%
  mutate(word = reorder_within(word, mean.density, love.song)) %>%
  mutate(love.song = if_else(love.song == 0, "Other Songs", "Love Songs")) %>%
  ggplot(aes(word, mean.density, fill = love.song)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~love.song, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Average density of each word across songs",
       title = "Which words had the highest average density in songs across song types?") + 
  scale_fill_brewer(palette = "PuBu") + 
  theme_dark()
# We see roughly the same patterns as before, and can use this method to find 
# Stop words we might want to exclude from our dataset. I exported it as a pdf
# and used it to find stop words, which are included below.

stop.words <- c(
  'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s',
  't','u','v','w','x','y','z','in','the','it','you','on','no','me','at','to','is',
  'am','and','go','or','do','be','not','my','as','we','all','so','ai','that',
  'up','oh','now','like','your','one','of','out','yeah','for','got','can','if',
  'get','are','em','but','know','here','will','every','ever','always',
  'same','done','with','just','have','this','when','because'
)

# Now, we can do some visualizations with the dataset excluding stopwords:
song.data.tidy.2 <- song.data.tidy %>%
  filter(!word %in% stop.words)

popular.words.2 <- song.data.tidy.2 %>%
  group_by(love.song) %>%
  count(word, love.song, sort=TRUE) %>%
  mutate(word = reorder_within(word, n, love.song)) %>%
  filter(rank(desc(n)) <= 50)

popular.words.2 %>% view()

popular.words.2 %>%
  ggplot(aes(word, n, fill = love.song)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~love.song, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Total words in all songs",
       title = "What were the most popular words in love songs and other songs?")

word.density.2 <- song.data.tidy.2 %>%
  count(word, love.song) %>%
  group_by(love.song) %>%
  filter(rank(desc(n)) <= 100) %>% 
  left_join(song.data.clean) %>%
  mutate(total.words = str_count(lyrics, boundary("word"))) %>% 
  mutate(density = str_count(lyrics, word)/total.words) %>%
  select(love.song, word, density) %>%
  group_by(word, love.song) %>% 
  filter(n()>2) %>%
  summarise(mean.density = mean(density))

word.density.2 %>%
  arrange(desc(mean.density)) %>%
  group_by(love.song) %>%
  filter(rank(desc(mean.density)) <= 45) %>%
  mutate(word = reorder_within(word, mean.density, love.song)) %>%
  mutate(love.song = if_else(love.song == 0, "Other Songs", "Love Songs")) %>%
  ggplot(aes(word, mean.density, fill = love.song)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~love.song, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Average density of each word across songs",
       title = "Which words had the highest average density in songs across song types?") +
  scale_fill_brewer(palette = "RdPu") + 
  theme_dark()

# Previously, we removed stop words from our dataset and then used term frequency/density as a measure
# of how significant these words are. A more sophisticated approach is to use TF-IDF
# This is term frequency (how often a word appears in a song) divided by document frequency 
# (how many songs the word is in). Ths assumption is that terms that appear more frequently in
# a document should be given higher weight, unless they also appear in many documents.

tfidf.words <- song.data.clean %>%
  filter(!is.na(love.song)) %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(word %in% grady_augmented | word %in% profanity_zac_anger) %>% # Keep only words in the lexicon
  filter(! word %in% stop.words) %>%
  filter(nchar(word)>3) %>%
  count(love.song, word, sort=TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, love.song, n)
# We see that idf and tf-idf are 0 for extremely common words.
# Now, let's plot important words using TF-IDF by song type
tfidf.words %>%
  arrange(desc(tf_idf)) %>%
  group_by(love.song) %>%
  filter(rank(desc(tf_idf)) <= 20) %>%
  mutate(word = reorder_within(word, tf_idf, love.song)) %>%
  ggplot(aes(word, tf_idf, fill=love.song)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~love.song, scales="free") +
  coord_flip() + 
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Important words using TF-IDF by song type?")

# We can also look at how important words have changed over the years
tfidf.words.yearly <- song.data.clean %>%
  mutate(year = year(chart.date)) %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(word %in% grady_augmented | word %in% profanity_zac_anger) %>% # Keep only words in the lexicon
  filter(nchar(word)>3) %>%
  count(year, word, sort=TRUE) %>% 
  ungroup() %>%
  bind_tf_idf(word, year, n)

tfidf.words.yearly %>%
  group_by(year) %>%
  filter(rank(desc(tf_idf)) <= 20) %>%
  mutate(word = reorder_within(word, tf_idf, year)) %>%
  ggplot(aes(word, tf_idf, fill=year)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol=5, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x=NULL)
# Interesting, but not very useful.

