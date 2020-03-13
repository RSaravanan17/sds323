library(mosaic)
library(tidyverse)
library(FNN)

set.seed(1)

# load the data
online_news = read.csv('./data/online_news.csv')

# the variables involved
summary(online_news)

# threshold on shares > 1400
online_news$viral = (online_news$shares > 1400)

viral = subset(online_news, viral == "TRUE")
dim(viral)

not_viral = subset(online_news, viral == "FALSE")
dim(not_viral)

# define a helper function for calculating RMSE
rmse = function(y, ypred) {
  sqrt(mean(data.matrix((y-ypred)^2)))
}


### Baseline KNN Model ###
library(foreach)

n = nrow(online_news)
n_train = round(0.8*n)
n_test = n - n_train

# k values to try out
#k_grid = exp(seq(log(1), log(100), length=100)) %>% round %>% unique
k_grid = c(1, 3, 5, 7, 9, 11)
k_values = c(k_grid)
percentIncorrect_k = c()
final_confusion_matrix_knn = c()
min_percentIncorrect = 9999999

# for each k value, store the average percent incorrect
for (k_val in k_values) {
  iterations = 0
  sum_percentError = 0
  
  # average the percent incorrect for k over 100 iterations
  while (iterations < 10) {
    # select instances to be included in the training and testing sets
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    
    # separate the instances for training and testing sets
    training_set = online_news[train_cases,]
    testing_set = online_news[test_cases,]
    
    # separate X and y for training and testing sets
    X_train = model.matrix(shares ~ 1, data = training_set)
    X_test = model.matrix(shares ~ 1, data = testing_set)
    y_train = training_set$shares
    y_test = testing_set$shares
    
    y_test_viral = ifelse(y_test > 1400, "Viral", "Not viral")
    
    # scale the training and testing set features
    #scale_factors = apply(X_train, 2, sd)
    #X_train_sc = scale(X_train, scale = scale_factors)
    #X_test_sc = scale(X_test, scale = scale_factors)
    
    # Fit KNN model and calculate percent incorrect
    knn_model = knn.reg(train = X_train, test = X_test, y_train, k = k_val)
    knn_model$predViral = ifelse(knn_model$pred > 1400, "Viral", "Not viral")
    
    confusion_table = table(Actual = y_test_viral, Predicted = knn_model$predViral)
    
    # calculate perent error
    percentIncorrect = (sum(y_test_viral != knn_model$predViral) / length(y_test_viral))
    
    # add the current rmse to the running sum
    sum_percentError = sum_percentError + percentIncorrect
    
    iterations = iterations + 1
  }
  
  # calculate and store the percent error for the prediction
  percentIncorrect_k[which(k_val == k_values)[[1]]] <- (sum_percentError / iterations)
  
  if ((sum_percentError / iterations) < min_percentIncorrect) {
    final_confusion_matrix_knn = confusion_table
    min_percentIncorrect = (sum_percentError / iterations)
  }
}

# identify which k value produced the smallest rmse value for the KNN model
print(min(percentIncorrect_k))
print(k_values[which.min(percentIncorrect_k)])
print(final_confusion_matrix_knn)

viral_viral = ifelse(is.na(final_confusion_matrix_knn[4]), 0, final_confusion_matrix_knn[4])
notViral_viral = ifelse(is.na(final_confusion_matrix_knn[3]), 0, final_confusion_matrix_knn[3])
viral_notViral = ifelse(is.na(final_confusion_matrix_knn[2]), 0, final_confusion_matrix_knn[2])
notViral_notViral = ifelse(is.na(final_confusion_matrix_knn[1]), 0, final_confusion_matrix_knn[1])

cat("            Predicted")
cat("Actual        Viral        Not viral")
cat("  Viral      ", viral_viral, "       ", viral_notViral)
cat("  Not viral  ", notViral_viral, "       ", notViral_notViral)

accuracy_rate = (viral_viral + notViral_notViral)/(viral_viral + notViral_viral + viral_notViral + notViral_notViral)
error_rate = 1 - accuracy_rate
cat("The accuracy rate for the baseline KNN regression model is", accuracy_rate)
cat("The error rate for the baseline KNN regression model is", error_rate)

true_positive_rate = notViral_notViral/(notViral_viral+viral_notViral)
cat("The true positive rate for the baseline KNN regression model is", true_positive_rate)

false_positive_rate = notViral_viral/(notViral_viral+viral_viral)
cat("The false positive rate for the baseline KNN regression model is", false_positive_rate)



### KNN Model ###
library(foreach)

n = nrow(online_news)
n_train = round(0.8*n)
n_test = n - n_train

# k values to try out
k_grid = exp(seq(log(1), log(100), length=100)) %>% round %>% unique
#k_grid = c(1, 3, 5, 7, 9, 11)
k_values = c(k_grid)
percentIncorrect_k = c()
final_confusion_matrix_knn = c()
min_percentIncorrect = 9999999

# for each k value, store the average percent incorrect
for (k_val in k_values) {
  iterations = 0
  sum_percentError = 0
  
  # average the percent incorrect for k over 100 iterations
  while (iterations < 100) {
    # select instances to be included in the training and testing sets
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    
    # separate the instances for training and testing sets
    training_set = online_news[train_cases,]
    testing_set = online_news[test_cases,]
    
    # separate X and y for training and testing sets
    X_train = model.matrix(shares ~ (n_tokens_title + n_tokens_content + average_token_length + num_keywords
                                     + avg_positive_polarity + global_rate_positive_words + global_rate_negative_words
                                     + title_subjectivity + title_sentiment_polarity + self_reference_min_shares
                                     + self_reference_max_shares + self_reference_avg_sharess + weekday_is_monday
                                     + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday
                                     + weekday_is_saturday + weekday_is_sunday + data_channel_is_bus + is_weekend), data = training_set)
    X_test = model.matrix(shares ~ (n_tokens_title + n_tokens_content + average_token_length + num_keywords
                                    + avg_positive_polarity + global_rate_positive_words + global_rate_negative_words
                                    + title_subjectivity + title_sentiment_polarity + self_reference_min_shares
                                    + self_reference_max_shares + self_reference_avg_sharess + weekday_is_monday
                                    + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday
                                    + weekday_is_saturday + weekday_is_sunday + data_channel_is_bus + is_weekend), data = testing_set)
    y_train = training_set$shares
    y_test = testing_set$shares
    
    y_test_viral = ifelse(y_test > 1400, "Viral", "Not viral")
    
    # scale the training and testing set features
    #scale_factors = apply(X_train, 2, sd)
    #X_train_sc = scale(X_train, scale = scale_factors)
    #X_test_sc = scale(X_test, scale = scale_factors)
    
    # Fit KNN model and calculate percent incorrect
    knn_model = knn.reg(train = X_train, test = X_test, y_train, k = k_val)
    knn_model$predViral = ifelse(knn_model$pred > 1400, "Viral", "Not viral")
    
    confusion_table = table(Actual = y_test_viral, Predicted = knn_model$predViral)
    
    # calculate perent error
    percentIncorrect = (sum(y_test_viral != knn_model$predViral) / length(y_test_viral))

    # add the current rmse to the running sum
    sum_percentError = sum_percentError + percentIncorrect
    
    iterations = iterations + 1
  }
  
  # calculate and store the percent error for the prediction
  percentIncorrect_k[which(k_val == k_values)[[1]]] <- (sum_percentError / iterations)
  
  if ((sum_percentError / iterations) < min_percentIncorrect) {
    final_confusion_matrix_knn = confusion_table
    min_percentIncorrect = (sum_percentError / iterations)
  }
}

# identify which k value produced the smallest rmse value for the KNN model
print(min(percentIncorrect_k))
print(k_values[which.min(percentIncorrect_k)])
print(final_confusion_matrix_knn)

viral_viral = final_confusion_matrix_knn[4]
notViral_viral = final_confusion_matrix_knn[3]
viral_notViral = final_confusion_matrix_knn[2]
notViral_notViral = final_confusion_matrix_knn[1]

cat("            Predicted")
cat("Actual        Viral        Not viral")
cat("  Viral      ", viral_viral, "       ", viral_notViral)
cat("  Not viral  ", notViral_viral, "       ", notViral_notViral)

accuracy_rate = (viral_viral + notViral_notViral)/(viral_viral + notViral_viral + viral_notViral + notViral_notViral)
error_rate = 1 - accuracy_rate
cat("The accuracy rate for the KNN regression model is", accuracy_rate)
cat("The error rate for the KNN regression model is", error_rate)

true_positive_rate = notViral_notViral/(notViral_viral+viral_notViral)
cat("The true positive rate for the KNN regression model is", true_positive_rate)

false_positive_rate = notViral_viral/(notViral_viral+viral_viral)
cat("The false positive rate for the KNN regression model is", false_positive_rate)


### Logistic Regression ###
n = nrow(online_news)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

confusion_tables_logit = do(100)*{
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train + 1, replace = FALSE)
  test_cases = setdiff(1:n, train_cases)
  
  news_train = online_news[train_cases,]
  news_test = online_news[test_cases,]
  
  newstrain_viral = ifelse(news_train$shares > 1400, 1, 0)
  news_train$viral <- newstrain_viral
  
  # run a logistic regression on viral status
  logit_news1 = glm(viral ~ (n_tokens_title + n_tokens_content + average_token_length + num_keywords
                             + avg_positive_polarity + global_rate_positive_words + global_rate_negative_words
                             + title_subjectivity + title_sentiment_polarity + self_reference_min_shares
                             + self_reference_max_shares + self_reference_avg_sharess + weekday_is_monday
                             + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday + weekday_is_friday
                             + weekday_is_saturday + weekday_is_sunday + data_channel_is_bus + is_weekend), 
                    data = news_train, family = binomial())
  
  coef(logit_news1)
  
  newstest_viral = ifelse(news_test$shares > 1400, "Viral", "Not viral")
  phat_test = predict(logit_news1, newdata = news_test, type='response')
  yhat_test = ifelse(phat_test > 0.5, "Viral", "Not viral")
  
  confusion_table = table(Actual = newstest_viral, Predicted = yhat_test)
  c(confusion_table)
}


viral_viral = colMeans(confusion_tables_logit[1])
notViral_viral = colMeans(confusion_tables_logit[2])
viral_notViral = colMeans(confusion_tables_logit[3])
notViral_notViral = colMeans(confusion_tables_logit[4])

cat("            Predicted")
cat("Actual        Viral        Not viral")
cat("  Viral      ", viral_viral, "       ", viral_notViral)
cat("  Not viral  ", notViral_viral, "       ", notViral_notViral)

accuracy_rate = (viral_viral + notViral_notViral)/(viral_viral + notViral_viral + viral_notViral + notViral_notViral)
error_rate = 1 - accuracy_rate
cat("The accuracy rate for the logistic regression model is", accuracy_rate)
cat("The error rate for the logistic regression model is", error_rate)

true_positive_rate = notViral_notViral/(notViral_viral+viral_notViral)
cat("The true positive rate for the logistic regression model is", true_positive_rate)

false_positive_rate = notViral_viral/(notViral_viral+viral_viral)
cat("The false positive rate for the logistic regression model is", false_positive_rate)

