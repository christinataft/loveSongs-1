song.data.clean <- read_csv("datasets/song_data_clean.csv", col_names = TRUE, cols(love.song = col_factor()
))

stop.words <- c(
  'a','b','c','d','e','f','g','h','j','k','l','m','n','o','p','q','r','s',
  't','u','v','w','x','y','z','in','the','it','you','on','no','me','at','to','is',
  'am','and','go','or','do','be','not','my','as','we','all','so','ai','that',
  'up','oh','now','like','your','one','of','out','yeah','for','got','can','if',
  'get','are','em','but','know','here','will','every','ever','always',
  'same','done'
)

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

# Next, we can try using bert in R

Sys.setenv(TF_KERAS=1)

reticulate::use_python('C:/Users/Ethan/AppData/Local/Programs/Python/Python38')
reticulate::py_config()

tensorflow::tf_version()
library(reticulate)
k_bert = import("keras_bert")
token_dict = k_bert$load_vocabulary(vocab_path)

# After that, we can focus on different methods for feature engineering!
# To benchmark - 
# 1. Features, produce several tasks with different features
# 2. Models
# 3. Resampling methods
