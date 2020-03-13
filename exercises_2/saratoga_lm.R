library(tidyverse)
library(mosaic)
require(reshape2)

set.seed(1)

data(SaratogaHouses)

summary(SaratogaHouses)

# Baseline model
lm_small = lm(price ~ bedrooms + bathrooms + lotSize, data=SaratogaHouses)

# 11 main effects
lm_medium = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
		fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=SaratogaHouses)

# Sometimes it's easier to name the variables we want to leave out
# The command below yields exactly the same model.
# the dot (.) means "all variables not named"
# the minus (-) means "exclude this variable"
lm_medium2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data=SaratogaHouses)

coef(lm_medium)
coef(lm_medium2)

# All interactions
# the ()^2 says "include all pairwise interactions"
lm_big = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data=SaratogaHouses)

coef(lm_big)


####
# Compare out-of-sample predictive performance
####

# easy averaging over train/test splits
n = nrow(SaratogaHouses)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

# loop for 100 iterations
rmse_vals = do(200)*{
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,]
  
  # Fit to the training data
  lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data = saratoga_train)
  lm2 = lm(price ~ . - sewer - waterfront - landValue - newConstruction, data = saratoga_train)
  lm3 = lm(price ~ (. - sewer - waterfront - landValue - newConstruction)^2, data = saratoga_train)
  lmBeast = lm(price ~ (livingArea * bedrooms * bathrooms * rooms) + (heating * centralAir) + (fuel + pctCollege), data = saratoga_train)
  
  # Predictions out of sample
  yhat_test1 = predict(lm1, saratoga_test)
  yhat_test2 = predict(lm2, saratoga_test)
  yhat_test3 = predict(lm3, saratoga_test)
  yhat_testBeast = predict(lmBeast, saratoga_test)
  
  c(rmse(saratoga_test$price, yhat_test1),
    rmse(saratoga_test$price, yhat_test2),
    rmse(saratoga_test$price, yhat_test3),
    rmse(saratoga_test$price, yhat_testBeast))
}

rmse_vals
colMeans(rmse_vals)
boxplot(rmse_vals)
print(colMeans(rmse_vals)[4] - colMeans(rmse_vals)[2])

extractAIC(lm1)
extractAIC(lm2)
extractAIC(lm3)
extractAIC(lmBeast)


### KNN model ###

library(foreach)

k_grid = exp(seq(log(1), log(100), length=100)) %>% round %>% unique

lowest_mean_rsme = 9999999
all_k_rmse = c()

# average the RMSE for each k over 100 train/test splits
err_grid = foreach(k = k_grid,  .combine='c') %do% {
  out = do(200)*{
    n = nrow(SaratogaHouses)
    
    n_train = round(0.8*n)
    n_test = n - n_train
    
    # select instances to be included in the training and testing sets
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    
    # separate the instances for training and testing sets
    training_set = SaratogaHouses[train_cases,]
    testing_set = SaratogaHouses[test_cases,]
    
    # separate X and y for training and testing sets
    X_train = model.matrix(price ~ (. - heating - fuel - sewer - waterfront - newConstruction - centralAir) - 1, data = training_set)
    X_test = model.matrix(price ~ (. - heating - fuel - sewer - waterfront - newConstruction - centralAir) - 1, data = testing_set)
    y_train = training_set$price
    y_test = testing_set$price
    
    # scale the training and testing set features
    scale_factors = apply(X_train, 2, sd)
    X_train_sc = scale(X_train, scale = scale_factors)
    X_test_sc = scale(X_test, scale = scale_factors)
    
    # Fit KNN model and calculate RMSE
    knn_model = knn.reg(train = X_train_sc, test = X_test_sc, y_train, k = k)
    rmse(y_test, knn_model$pred)
  }
  
  mean_rsme = mean(out$result)
  
  if (mean_rsme < lowest_mean_rsme) {
    all_k_rmse <- out$result
    lowest_mean_rsme = mean_rsme
  }
  
  mean_rsme
}

# identify which k produced the smallest rmse value for the KNN model
print(min(err_grid))
print(k_grid[which.min(err_grid)])

rmse_vals[5] <- all_k_rmse

colnames(rmse_vals) = c("Linear Regression Model #1", "Linear Regression Model #2", "Linear Regression Model #3", "Our Linear Regression Model", "Our KNN Regression Model")


cat("Mean RMSE of lm1:", colMeans((rmse_vals[1])), "\n")
cat("Mean RMSE of lm2:", colMeans((rmse_vals[2])), "\n")
cat("Mean RMSE of lm3:", colMeans((rmse_vals[3])), "\n")
cat("Mean RMSE of lmBeast:", colMeans((rmse_vals[4])), "\n")
cat("Mean RMSE of knn_model:", colMeans((rmse_vals[5])), "\n")

# boxplots of the rmses for each regression model (with and without outliers)
ggplot(data = melt(rmse_vals), aes(x = variable, y = value, fill = 'orange')) + 
  geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2, notch=FALSE) + 
  theme(legend.position="none") + 
  xlab("Regression model") + 
  ylab("RMSE") + 
  ggtitle("RMSEs by Regression Model (With Outliers)")

ggplot(data = melt(rmse_vals), aes(x = variable, y = value, fill='orange')) + 
  geom_boxplot(outlier.colour="grey", outlier.shape=NA, notch=FALSE) +
  scale_y_continuous(limits=c(50000,100000), breaks=seq(50000,100000,10000), expand = c(0, 0)) + 
  theme(legend.position="none") + 
  xlab("Regression model") + 
  ylab("RMSE") + 
  ggtitle("RMSEs by Regression Model (Without Outliers)")

# plot rmse vs. K
RMSEvsK_Saratoga_df <- data.frame(k_grid, err_grid)
ggplot(data = RMSEvsK_Saratoga_df) + 
  geom_line(mapping = aes(x = k_grid, y = err_grid), color='red') +
  xlab("K") +
  ylab("RMSE") +
  ggtitle("RMSE vs. K for Optimal KNN Model for Predicting Saratoga House Prices")

