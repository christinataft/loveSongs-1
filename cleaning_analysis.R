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

## Exploratory data analysis
# Filter out not yet labelled songs.
song.data.explore <- song.data.clean %>%
  filter(!is.na(love.song))

# What percentage of songs charting each year are love songs?

song.data.explore %>%
  ggplot(aes(chart.date)) +
  geom_histogram(position = "fill", aes(fill = love.song), bins = 10) # Fix the date labels, change colors

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
  mutate(release.date = update(release.date, year=2010)) %>%
  ggplot(aes(release.date)) +
  geom_histogram(binwidth = (30)) +
  scale_x_date(date_labels="%B", date_breaks = "1 month")

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
  filter(rank(desc(n)) <= 50)

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
  filter(rank(desc(mean.density)) <= 50) %>%
  mutate(word = reorder_within(word, mean.density, love.song)) %>%
  ggplot(aes(word, mean.density, fill = love.song)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~love.song, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Average density of each word across songs",
       title = "Which words had the highest average density in songs across song types?") 
# We see roughly the same patterns as before, and can use this method to find 
# Stop words we might want to exclude from our dataset. I exported it as a pdf
# and used it to find stop words, which are included below.

stop.words <- c(
  'a','b','c','d','e','f','g','h','j','k','l','m','n','o','p','q','r','s',
  't','u','v','w','x','y','z','in','the','it','you','on','no','me','at','to','is',
  'am','and','go','or','do','be','not','my','as','we','all','so','ai','that',
  'up','oh','now','like','your','one','of','out','yeah','for','got','can','if',
  'get','are','em','but','know','here','will','every','ever','always',
  'same','done'
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
  filter(rank(desc(mean.density)) <= 50) %>%
  mutate(word = reorder_within(word, mean.density, love.song)) %>%
  ggplot(aes(word, mean.density, fill = love.song)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~love.song, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Average density of each word across songs",
       title = "Which words had the highest average density in songs across song types?") 

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

# Next, let's do some feature engineering so we can feed something into our model. (This has been called the hardest part of nlp!!)
library(tm)

# Create the corpus
corpus <- VCorpus(VectorSource(song.data.clean$lyrics))
# document-term matrix, applying TF-IDF
tfidf.dtm <- DocumentTermMatrix(corpus, control = list(weighting= weightTfIdf,
                                                            stopwords = stop.words,
                                                            removeNumbers = TRUE,
                                                            removePunctuation = TRUE,
                                                 stemming=TRUE))
# Look at a summary of the dtm
tfidf.dtm

# Reduce sparsity to GREATLY improve performance by reducing dimensionality. This takes out terms that are 
# only present in 0.5% of documents.
tfidf.dtm <- removeSparseTerms(tfidf.dtm, 0.995)

# Get a data.frame for modelling
tfidf.df <- data.frame(as.matrix(tfidf.dtm), 
                       stringsAsFactors = FALSE)
# Add labels
tfidf.df$song.label <- song.data.clean$love.song

#load mlr3 and algorithms. I chose mlr3 because of the ability to train multiple different algorithms at once
# to find the best one, and quickly do cross validation and resampling, so that more time can be spent on 
# feature engineering and tweaking features.
library(mlr3)
library(mlr3learners)
library(mlr3viz)

# Drop unlabelled columns
tfidf.df <- tfidf.df[!is.na(tfidf.df$song.label),]

#Prepare classification task
tfidf.task <- TaskClassif$new(id = "tfidf", 
                              backend = tfidf.df, 
                              target = 'song.label')

# set a random forest learner
learner.rf <- lrn("classif.ranger")

# train-test split of 80:20
tfidf.train <- sample(tfidf.task$nrow, 0.8*tfidf.task$nrow)
tfidf.test <- setdiff(seq_len(tfidf.task$nrow), tfidf.train)

# train the model
learner.rf$train(tfidf.task, row_ids = tfidf.train)
prediction <- learner.rf$predict(tfidf.task, row_ids = tfidf.test)

# Get test accuracy
measure = msr("classif.acc")
prediction$score(measure)

# We get ~81% test accuracy using random forests.

autoplot(prediction, type="roc") # Use this to plot ROC curves when u have 2 variables

# Now, let's use benchmark() to compare different learners and resampling methods

learners <- c("classif.kknn", "classif.naive_bayes", "classif.ranger", "classif.svm", "classif.xgboost")
learners <- lapply(learners, lrn, predict_sets=c("train", "test"))

#use 3-fold cross validation
resamplings <- rsmp("cv", folds=3)
design <- benchmark_grid(tfidf.task, learners, resamplings)

# Evaluate benchmark
bmr <- benchmark(design)

measures <- list(
  msr("classif.ce", id = "ce_train", predict_sets = "train"),
  msr("classif.ce", id = "ce_test")
)

bmr$aggregate(measures)
# We using a random forest seems to give us the best results by far - followed by xgboost.
# However, random forest seems to be overfitting dramatically - we can improve on that by tuning our 
# parameters.

library(mlr3tuning)
learner.rf$param_set
# Set parameters to tune
library(paradox)

tune_ps <- ParamSet$new(list(
  ParamInt$new("num.trees", lower=1, upper = 5000),
  ParamInt$new("max.depth", lower=1, upper = 32),
  ParamInt$new("min.node.size", lower=1, upper=1000)
))

learner.rf$param_set

# Select a performance measure and a resampling strategy
measure <- msr("classif.ce")
hout <- rsmp("holdout")

# select a budget! We will terminate when tuning does not improve
evals <- trm("stagnation")

instance <- TuningInstanceSingleCrit$new(
  task = tfidf.task,
  learner = learner.rf,
  resampling = hout,
  measure = measure,
  search_space = tune_ps,
  terminator = evals
)
instance

# Next, we need to choose a tuning algorithm. Let's start with grid search
tuner <- tnr("grid_search")

# start the tuning
tuner$optimize(instance)

# Looks like we can reach 78% test accuracy with a combination of tf-idf and random forests

# Let's try different methods for feature engineering - may allow us to extract information otherwise lost
# using word counts

## Bigrams

# Function to tokenize bigrams
BigramTokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

# Make bigrams document term matrix
bigrams.dtm <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))

# We have a sparsity of 100%. Let's take out bigrams that are only present in 1% of the songs. ( we can tweak this)
bigrams.dtm <- removeSparseTerms(bigrams.dtm, 0.99)

bigrams.dtm # Now, we only have 97% sparsity!

# Get df
bigrams.df <- data.frame(as.matrix(bigrams.dtm), 
                       stringsAsFactors = FALSE)
# Add labels
bigrams.df$song.label <- song.data.clean$love.song

# Drop unlabelled columns
bigrams.df <- bigrams.df[!is.na(bigrams.df$song.label),]

bigrams.df

bigrams.task <- TaskClassif$new(id = "bigrams", 
                              backend = bigrams.df, 
                              target = 'song.label')

# naive bayes to try out
learner.nb <- lrn("classif.naive_bayes")

# train-test split of 80:20
bigrams.train <- sample(bigrams.task$nrow, 0.8*bigrams.task$nrow)
bigrams.test <- setdiff(seq_len(bigrams.task$nrow), bigrams.train)

# train the model
learner.nb$train(bigrams.task, row_ids = bigrams.train)
prediction <- learner.nb$predict(bigrams.task, row_ids = bigrams.test)

# Get test accuracy
measure = msr("classif.acc")
prediction$score(measure)
# LOL we get 48 % accuracy which is worse than chance. nevermind, try with a bunch of different models.

learners <- c("classif.kknn", "classif.naive_bayes", "classif.ranger", "classif.svm", "classif.xgboost")
learners <- lapply(learners, lrn, predict_sets=c("train", "test"))

#use 3-fold cross validation
resamplings <- rsmp("cv", folds=3)
design <- benchmark_grid(bigrams.task, learners, resamplings)

# Evaluate benchmark
bmr <- benchmark(design)

measures <- list(
  msr("classif.ce", id = "ce_train", predict_sets = "train"),
  msr("classif.ce", id = "ce_test")
)

bmr$aggregate(measures)

# Random forest still gives us the best test accuracy at 78%.
# What if we reduce the sparsity?
bigrams.df.95 <- corpus %>%
  DocumentTermMatrix(control = list(tokenize = BigramTokenizer)) %>%
  removeSparseTerms(0.95) %>%
  as.matrix() %>%
  data.frame(stringsAsFactors = FALSE)
bigrams.df.95$song.label <- song.data.clean$love.song
bigrams.df.95 <- bigrams.df.95[!is.na(bigrams.df.95$song.label),]

bigrams.df.90 <- corpus %>%
  DocumentTermMatrix(control = list(tokenize = BigramTokenizer)) %>%
  removeSparseTerms(0.90) %>%
  as.matrix() %>%
  data.frame(stringsAsFactors = FALSE)
bigrams.df.90$song.label <- song.data.clean$love.song
bigrams.df.90 <- bigrams.df.90[!is.na(bigrams.df.90$song.label),]

task.bigrams.90 <- TaskClassif$new(id="bigrams90", bigrams.df.90, "song.label")
task.bigrams.95 <- TaskClassif$new(id="bigrams95", bigrams.df.95, "song.label")

learner.rf <- lrn("classif.ranger")
resampling <- rsmp("cv", folds = 3L)

rr90 <- resample(task.bigrams.90, learner.rf, resampling)
rr90$aggregate(msr("classif.ce"))
rr95 <- resample(task.bigrams.90, learner.rf, resampling)
rr95$aggregate(msr("classif.ce"))

# train the model
learner.rf$train(tfidf.task, row_ids = tfidf.train)
prediction <- learner.rf$predict(tfidf.task, row_ids = tfidf.test)

# Get test accuracy
measure = msr("classif.acc")
prediction$score(measure)

# It seems we get a nice balance of performance and accuracy with our 97% sparsity. Now,we can tune parameters
# for our random forest

tune_ps <- ParamSet$new(list(
  ParamInt$new("num.trees", lower=1, upper = 5000),
  ParamInt$new("max.depth", lower=1, upper = 32),
  ParamInt$new("min.node.size", lower=1, upper=1000)
))

# Select a performance measure and a resampling strategy
measure <- msr("classif.ce")
hout <- rsmp("holdout")

# select a budget! We will terminate when tuning does not improve
evals <- trm("stagnation")

instance <- TuningInstanceSingleCrit$new(
  task = bigrams.task,
  learner = learner.rf,
  resampling = hout,
  measure = measure,
  search_space = tune_ps,
  terminator = evals
)

tuner <- tnr("grid_search")

# start the tuning
tuner$optimize(instance)

# Try with trigrams!
TrigramTokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}

# Make trigrams document term matrix
trigrams.dtm <- DocumentTermMatrix(corpus, control = list(tokenize = TrigramTokenizer))

# We have a sparsity of 100%. Let's take out bigrams that are only present in 1% of the songs. ( we can tweak this)
trigrams.dtm <- removeSparseTerms(trigrams.dtm, 0.99)

trigrams.dtm # Now, we only have 97% sparsity!

# Get df
trigrams.df <- data.frame(as.matrix(trigrams.dtm), 
                         stringsAsFactors = FALSE)

trigrams.df

# Add labels
trigrams.df$song.label <- song.data.clean$love.song

# Drop unlabelled columns
trigrams.df <- trigrams.df[!is.na(trigrams.df$song.label),]

trigrams.df

trigrams.task <- TaskClassif$new(id = "trigrams", 
                                backend = trigrams.df, 
                                target = 'song.label')
# Tuned learner
trigrams.autotuner <- AutoTuner$new(
  learner = lrn("classif.ranger"),
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  search_space = ParamSet$new(list(
    ParamInt$new("num.trees", lower=1, upper = 5000),
    ParamInt$new("max.depth", lower=1, upper = 1000),
    ParamInt$new("min.node.size", lower=1, upper=1000)
  )),
  terminator = trm("stagnation"),
  tuner = tnr("grid_search")
)

# Benchmark tuned learner against a default value learner
trigrams.grid = benchmark_grid(
  task = trigrams.task,
  learner = list(trigrams.autotuner, lrn("classif.ranger")),
  resampling = rsmp("cv", folds = 3)
)

bmr = benchmark(trigrams.grid)
bmr$aggregate(msrs("classif.ce"))
# We see that the tuned model has MARGINALLY better accuracy than an untuned model, and they both do worse than bigrams!

## Next, we try word2vec. Word2vec involves training a model on a dataset, which will then convert these 
## Words to vectors

library(word2vec)

# First, train the word2vec model
word2vec.train <- txt_clean_word2vec(song.data.clean$lyrics)
word2vec.model <- word2vec(word2vec.train, stopwords = stop.words)

summary(word2vec.model, type="vocabulary")

# Word2vec should allow us to find similar words. Let's find word embeddings and nearest neighbours
embeddings <- as.matrix(word2vec.model)
nearestneighbours <- predict(word2vec.model, c("hate", "love"), type="nearest", top_n=10)
nearestneighbours
# We can see how word2vec finds similar terms 

# Prepare data frame for doc2vec

doc2vec.df <- song.data.clean[, c('song.title', 'lyrics')]
stop.regex <- str_c("(?=(",str_c(stop.words, collapse="|"),"))")
stop.regex ## Implement this eventuall!

doc2vec.df <- doc2vec.df %>%
  rename(doc_id = song.title,
         text = lyrics) %>%
  mutate(text = txt_clean_word2vec(text))

doc2vec.df <- doc2vec(word2vec.model, doc2vec.df)

doc2vec.df

# After that, we can focus on different methods for feature engineering!
# To benchmark - 
# 1. Features, produce several tasks with different features
# 2. Models
# 3. Resampling methods

mlr_measures
dtm.bigrams <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
dtm.bigrams <- 