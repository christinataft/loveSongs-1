# Remember: Use lexicons to filter out words and get the good words only!

library(tidyverse)
library(lexicon)
song.data.clean <- read_csv("datasets/song_data_clean.csv", col_names = TRUE, cols(love.song = col_factor()
))

stop.words <- c(
  'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s',
  't','u','v','w','x','y','z','in','the','it','you','on','no','me','at','to','is',
  'am','and','go','or','do','be','not','my','as','we','all','so','ai','that',
  'up','oh','now','like','your','one','of','out','yeah','for','got','can','if',
  'get','are','em','but','know','here','will','every','ever','always',
  'same','done'
)

FilterLexicon <- function(string) {
  string.list <- str_split(string, boundary("word"))
  for (i in seq_along(string.list)){
    string.list[[i]] <- if_else(string.list[[i]] %in% grady_augmented | string.list[[i]] %in% profanity_zac_anger, string.list[[i]], "")
  }
  return(str_c(unlist(string.list), collapse = " "))
}

lyrics <- song.data.clean$lyrics

song.data.clean <- song.data.clean %>%
  mutate(lyrics = map_chr(lyrics, FilterLexicon))

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

# Get a data.frame for modelling
tfidf.df <- data.frame(as.matrix(tfidf.dtm), 
                       stringsAsFactors = FALSE)
# Add labels
tfidf.df$song.label <- song.data.clean$love.song

# Repeat for tf
tf.dtm <- DocumentTermMatrix(corpus, control = list(weighting= weightTf,
                                                       stopwords = stop.words,
                                                       removeNumbers = TRUE,
                                                       removePunctuation = TRUE,
                                                       stemming=TRUE))
# Look at a summary of the dtm
tf.dtm

# Get a data.frame for modelling
tf.df <- data.frame(as.matrix(tf.dtm), 
                       stringsAsFactors = FALSE)
# Add labels
tf.df$song.label <- song.data.clean$love.song


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
library(paradox)

# Set parameters to tune
tune_ps <- ParamSet$new(list(
  ParamInt$new("max.depth", lower=1, upper = 32),
  ParamInt$new("min.node.size", lower=1, upper=1000)
))

learner.rf <- lrn("classif.rpart")

# Select a performance measure and a resampling strategy
measure <- msr("classif.ce")
resamplings <- rsmp("cv", folds=3)

# Select a budget - We will terminate when tuning does not improve
evals <- trm("stagnation")

# Select a tuner
tuner <- tnr("grid_search")

rf.tuned.model <- AutoTuner$new(
  learner = learner.rf,
  resampling = resamplings,
  measure = measure,
  search_space = tune_ps,
  terminator = evals,
  tuner = tuner
)

# Compare tuned learner against default value learner
grid <-  benchmark_grid(
  task = tfidf.task,
  learner = list(rf.tuned.model, lrn("classif.rpart")),
  resampling = rsmp("cv", folds = 3)
)

bmr <- benchmark(grid)
bmr$aggregate(msrs(c("classif.ce", "time_train")))

# Looks like we can reach 78% test accuracy with a combination of tf-idf and random forests

# Let's try different methods for feature engineering - may allow us to extract information otherwise lost
# using word counts

# Term frequency

# Drop unlabelled columns
tf.df <- tf.df[!is.na(tf.df$song.label),]

#Prepare classification task
tf.task <- TaskClassif$new(id = "tf", 
                              backend = tf.df, 
                              target = 'song.label')

learners <- c("classif.naive_bayes", "classif.ranger", "classif.xgboost")
learners <- lapply(learners, lrn, predict_sets=c("train", "test"))

#use 3-fold cross validation
resamplings <- rsmp("cv", folds=3)
design <- benchmark_grid(tf.task, learners, resamplings)

# Evaluate benchmark
bmr <- benchmark(design)

measures <- list(
  msr("classif.ce", id = "ce_train", predict_sets = "train"),
  msr("classif.ce", id = "ce_test")
)

bmr$aggregate(measures)

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

learners <- c("classif.naive_bayes", "classif.ranger","classif.xgboost")
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

