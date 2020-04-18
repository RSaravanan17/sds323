library(mosaic)
library(tidyverse)
library(FNN)
library(glmnet)


# import data
greenbuildings = read.csv('./data/greenbuildings.csv', header=TRUE)

# rmse function
rmse = function(y, yhat) {
  sqrt(mean((y - yhat)^2, na.rm=TRUE))
}

# variables that control how long the program takes to run
num_splits = 100
k_limit = 30

#model 1: linear regression model (RMSE)
#80% training data, 20% test data
n = nrow(greenbuildings)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

#100 different random splits
lm_vals = do(num_splits)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = greenbuildings[train_cases, 2:ncol(greenbuildings)]
  on_test = greenbuildings[test_cases, 2:ncol(greenbuildings)]
  
  lm_ajjarapu = lm(Rent ~ ., data=on_train)
  lm_ajjarapu_2 = lm(Rent ~ .^2, data=on_train)
  
  # Predictions out of sample + convert to binary
  y_test = predict(lm_ajjarapu, on_test)
  y_test_2 = predict(lm_ajjarapu_2, on_test)
  
  c(rmse(y_test, on_test$Rent),rmse(y_test_2, on_test$Rent))
}
lm_avg = unname(colMeans(lm_vals))

#model 2: knn (RMSE)
k_vals = 2:k_limit
knn_vals = matrix(0, k_limit - 1, 2)

for (k_val in k_vals) {
  rmse_vals_iter = do(num_splits)*{
    
    # re-split into train and test cases with the same sample sizes
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    on_train = greenbuildings[train_cases,2:ncol(greenbuildings)]
    on_test = greenbuildings[test_cases,2:ncol(greenbuildings)]

    #create KNN model
    Xtrain_temp = model.matrix(Rent ~ . - 1, data = on_train)
    Xtest_temp = model.matrix(Rent ~ . - 1, data = on_test)
    
    ytrain = on_train$Rent
    ytest = on_test$Rent
    
    #standardize data
    scale_amount = apply(Xtrain_temp, 2, sd)
    Xtrain = scale(Xtrain_temp, scale=scale_amount)
    Xtest = scale(Xtest_temp, scale=scale_amount)
    
    #train k model
    knn_model = knn.reg(Xtrain, Xtest, ytrain, k=k_val)
    
    # Predictions out of sample + convert to binary
    yhat_test1_pred = knn_model$pred
    
    c(k_val,rmse(yhat_test1_pred, ytest))
  }
  rmse_vals_avg = colMeans(rmse_vals_iter)
  knn_vals[k_val - 1,] = rmse_vals_avg
}
plot(knn_vals[,1], knn_vals[,2], ty = "l")
knn_rmse = unname(knn_vals[which.min(knn_vals[,2]),])

#model 3: lasso regression/ridge regression (RMSE)
vals_lr_rr = do(num_splits)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = greenbuildings[train_cases,2:ncol(greenbuildings)]
  on_test = greenbuildings[test_cases,2:ncol(greenbuildings)]
  
  temp_train = model.matrix.lm(Rent ~ . - 1, data = on_train, na.action=na.pass)
  temp_test = model.matrix.lm(Rent ~ . - 1, data = on_test, na.action=na.pass)
  x_train = temp_train[complete.cases(temp_train),]
  y_train = on_train$Rent[complete.cases(temp_train)]
  x_test = temp_test[complete.cases(temp_test),]
  y_test = on_test$Rent[complete.cases(temp_test)]
  
  # ridge regression
  cv_fit_r = cv.glmnet(x_train, y_train, family="gaussian", alpha = 0)
  # lasso regression
  cv_fit_l = cv.glmnet(x_train, y_train, family="gaussian", alpha = 1)
  
  opt_lambda_r = cv_fit_r$lambda.min
  opt_lambda_l = cv_fit_l$lambda.min
   
  y_pred_r = predict(cv_fit_r$glmnet.fit, s = opt_lambda_r, newx = x_test)
  y_pred_l = predict(cv_fit_l$glmnet.fit, s = opt_lambda_l, newx = x_test)
  
  c(rmse(y_pred_r, y_test), rmse(y_pred_l, y_test))
}
lr_model_avg = min(vals_lr_rr[,2])
rr_model_avg = min(vals_lr_rr[,1])

#model 4: logistic regression
vals_logm = do(num_splits)*{
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = greenbuildings[train_cases,2:ncol(greenbuildings)]
  on_test = greenbuildings[test_cases,2:ncol(greenbuildings)]
  
  lm_ajjarapu = glm(Rent ~ ., data=on_train, family=gaussian, maxit = 100)
  lm_ajjarapu_2 = glm(Rent ~ .^2, data=on_train, family=gaussian, maxit = 100)
  
  # Predictions out of sample + convert to binary
  y_test = predict(lm_ajjarapu, on_test)
  y_test_2 = predict(lm_ajjarapu_2, on_test)
  
  c(rmse(y_test, on_test$Rent), rmse(y_test_2, on_test$Rent))
}
logm_vals = unname(colMeans(vals_logm))


# plot(on_test$Rent, ty = "l")
# lines(lm(Rent ~ ., data=on_test)$fitted.values, ty = "l", col="red")
# lines(glm(Rent ~ ., data=on_test, family=gaussian, maxit = 100)$fitted.values, ty = "l", col="blue")


paste("MODEL SUCCESS: ")
paste("1) LINEAR REGRESSION MODEL - no squared RMSE val: ", lm_avg[1], " squared RMSE val: ", lm_avg[2])
paste("2) kNN - k=",knn_rmse[1]," RMSE val: ", knn_rmse[2])
paste("3) LASSO REGRESSION - RMSE val: ", lr_model_avg[1])
print("coefficients for lasso regression: ")
print(coef(cv_fit_l$glmnet.fit,s = cv_fit_l$lambda.min))
paste("3) RIDGE REGRESSION - RMSE val: ", rr_model_avg[1])
print("coefficients for ridge regression: ")
print(coef(cv_fit_r$glmnet.fit,s = cv_fit_r$lambda.min))
paste("5) LOGISTIC REGRESSION - no squared RMSE val: ", logm_vals[1], " squared RMSE val: ", logm_vals[2])

