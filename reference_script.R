# Measures of accuracy
mlr_measures
# Learners
mlr_learners
# Resampling methods
mlr_resamplings


# Make simple tree learner
learner.tree <- lrn("classif.rpart", maxdepth = 3, predict_type = "prob")

# 3-folds CV for resampling method
resampling <- rsmp("cv", folds = 3L)

# Resample the dataset
resampling$instantiate(tfidf.task)

# Apply learner
rr = resample(tfidf.task, learner.tree, resampling, store_models=TRUE)

# get mean classification error (or any other performance measure)
rr$aggregate(msr("classif.ce"))

# Get classification error for each iteration
rr$score(msr("classif.ce"))

# Check for warnings
rr$warnings

# Check for errors
rr$errors

# Look at the training/test set indices for each particular itearation
str(rr$resampling$train_set(2))
str(rr$resampling$test_set(2))
str(rr$resampling$train_set(3))
str(rr$resampling$test_set(3))

# Retrieve learner of a specific iteration
lrn <- rr$learners[[1]]
autoplot(lrn$model)

# Note: If you want to compare multiple learners in a fair manner, ensure that each learner operates on the 
# same resampling instance. 
# If we want to compare different task, learners or resampling, we can use the benchmark() function.


