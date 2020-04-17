library(ggplot2)
library(mosaic)
library(tidyverse)
library(FNN)
library(gamlr)

# import data
greenbuildings = read.csv('./data/greenbuildings.csv', header=TRUE)


# rmse function
rmse = function(y, yhat) {
  sqrt(mean( (y - yhat)^2, na.rm=TRUE))
}

#model 1: linear regression model (RMSE)
#80% training data, 20% test data
n = nrow(greenbuildings)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train

#100 different random splits
lm_vals = do(100)*{
  
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = greenbuildings[train_cases,2:ncol(greenbuildings)]
  on_test = greenbuildings[test_cases,2:ncol(greenbuildings)]
  
  lm_ajjarapu = lm(Rent ~ ., data=on_train) 
  
  # Predictions out of sample + convert to binary
  y_test = predict(lm_ajjarapu, on_test)
  
  rmse(y_test, on_test$Rent)
}
lm_avg = unname(colMeans(lm_vals))

#model 2: knn (RMSE)
k_limit = 3
k_vals = 2:k_limit
knn_vals = matrix(0, k_limit - 1, 2)

for (k_val in k_vals) {
  rmse_vals_iter = do(100)*{
    
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
knn_avg = unname(knn_vals[which.min(knn_vals[,2]),])

#model 3: lasso regression (RMSE)
scx = model.matrix(Rent ~ .-1, model.frame(~ ., data=greenbuildings, na.action=na.pass)) # do -1 to drop intercept!
rowsToExclude =  as.numeric(rownames(scx[rowSums(is.na(scx))!=0,]))
scx = scx[-rowsToExclude,]
scy = greenbuildings[-rowsToExclude,]$Rent # pull out `y' too just for convenience

# fit the lasso across a range of penalty pars
sclasso = gamlr(scx, scy, family="binomial")
plot(sclasso) # the path plot!

# COPIED CODE FROM SEMICONDUCTOR.R

##
# AIC selected coef
AICc(sclasso)
plot(sclasso$lambda, AICc(sclasso))
plot(log(sclasso$lambda), AICc(sclasso))
# the coefficients at the AIC-optimizing value
# note the sparsity
scbeta_aic = coef(sclasso) 
# optimal lambda
log(sclasso$lambda[which.min(AICc(sclasso))])
##

#model 4: logistic regression
vals_logm = do(100)*{
  # re-split into train and test cases with the same sample sizes
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  on_train = greenbuildings[train_cases,2:ncol(greenbuildings)]
  on_test = greenbuildings[test_cases,2:ncol(greenbuildings)]
  
  lm_ajjarapu = glm(Rent ~ ., data=on_train, family=gaussian, maxit = 100) 
  
  # Predictions out of sample + convert to binary
  y_test = predict(lm_ajjarapu, on_test)
  
  rmse(y_test, on_test$Rent)
}
logm_vals = unname(colMeans(vals_logm))

print(paste("MODEL SUCCESS: "))
print(paste("1) LINEAR REGRESSION MODEL - RMSE val: ", lm_avg))
print(paste("2) kNN - k=",knn_avg[1]," RMSE val: ", knn_avg[2]))
print(paste("3) LASSO REGRESSION - N/A"))
print(paste("4) LOGISTIC REGRESSION - RMSE val: ", logm_vals))
