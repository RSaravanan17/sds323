library(mosaic)
library(tidyverse)
library(FNN)

set.seed(1)

# load the data
sclass = read.csv('./data/sclass.csv')

# the variables involved
summary(sclass)

# focus on 2 trim levels: 350 and 65 AMG
sclass350 = subset(sclass, trim == '350')
dim(sclass350)

sclass65AMG = subset(sclass, trim == '65 AMG')
summary(sclass65AMG)

# look at price vs mileage for each trim level
plot(price ~ mileage, data = sclass350)
plot(price ~ mileage, data = sclass65AMG)

# plot the data aesthetically
ggplot(data = sclass350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='blue') +
  xlab("Mileage") +
  ylab("Price") +
  ggtitle("Mileage vs. Price for Trim Level 350")

ggplot(data = sclass65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='red') +
  xlab("Mileage") +
  ylab("Price") +
  ggtitle("Mileage vs Price for Trim Level 65 AMG")


### trim: sclass350 ###

# Make a train-test split
N_350 = nrow(sclass350)  # number of rows in sclass350
N_train_350 = floor(0.8*N_350)  # store 80% of the number of rows
N_test_350 = N_350 - N_train_350  # store the remaining number of rows

# define a helper function for calculating RMSE
rmse = function(y, ypred) {
  sqrt(mean(data.matrix((y-ypred)^2)))
}


k_grid = exp(seq(log(3), log(100), length=100)) %>% round %>% unique

knn_values_350 = c(k_grid)
rmse_knn_350 = c()

# predict a KNN model for each value of k for 3 <= k <= 100 and store rmse value
for (k_val in knn_values_350) {
  iterations = 0
  sum_rmse = 0
  
  while (iterations < 200) {
    # randomly sample a set of data points to include in the training set
    train_ind_350 = sample.int(N_350, N_train_350, replace=FALSE)
    
    # Define the training and testing set
    D_train_350 = sclass350[train_ind_350,]
    D_test_350 = sclass350[-train_ind_350,]
    
    # Now separate the training and testing sets into features (X) and outcome (y)
    X_train_350 = select(D_train_350, mileage)
    y_train_350 = select(D_train_350, price)
    X_test_350 = select(D_test_350, mileage)
    y_test_350 = select(D_test_350, price)
    
    knn_350 = knn.reg(X_train_350, X_test_350, y_train_350, k=k_val)
    
    # make a prediction on the testing set
    ypred_knn_350 = knn_350$pred
    
    # add the current rmse to the running sum
    sum_rmse = sum_rmse + rmse(y_test_350, ypred_knn_350)
    
    iterations = iterations + 1
  }
  
  # calculate and store the average rmse values for the prediction
  rmse_knn_350[which(k_val == knn_values_350)[[1]]] <- (sum_rmse / iterations)
}

# identify which k value produced the smallest rmse value for the KNN model
print(min(rmse_knn_350))
print(knn_values_350[which.min(rmse_knn_350)])


# randomly sample a set of data points to include in the training set
train_ind_350 = sample.int(N_350, N_train_350, replace=FALSE)

# Define the training and testing set
D_train_350 = sclass350[train_ind_350,]
D_test_350 = sclass350[-train_ind_350,]

# Now separate the training and testing sets into features (X) and outcome (y)
X_train_350 = select(D_train_350, mileage)
y_train_350 = select(D_train_350, price)
X_test_350 = select(D_test_350, mileage)
y_test_350 = select(D_test_350, price)

# run KNN model for optimal K value
knn_350 = knn.reg(X_train_350, X_test_350, y_train_350, k=knn_values_350[which.min(rmse_knn_350)])

# make a prediction on the testing set
D_test_350$predictedPrice = knn_350$pred


# plot rmse vs. K
RMSEvsK_350_df <- data.frame(knn_values_350, rmse_knn_350)
ggplot(data = RMSEvsK_350_df) + 
  geom_line(mapping = aes(x = knn_values_350, y = rmse_knn_350), color='red') +
  xlab("K") +
  ylab("RMSE") +
  ggtitle("rmse vs. K for Trim Level 350")

# plot of the fitted model for optimal value of K on testing set
ggplot(data = D_test_350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='blue') +
  geom_line(mapping = aes(x = mileage, y = predictedPrice), color='red') +
  xlab("Mileage (mi)") +
  ylab("Price ($)") +
  ggtitle("Fitted Model of KNN on Trim Level 350 on Testing Set")





### trim: sclass65AMG ###

# Make a train-test split
N_65AMG = nrow(sclass65AMG)  # number of rows in sclass350
N_train_65AMG = floor(0.8*N_65AMG)  # store 80% of the number of rows
N_test_65AMG = N_65AMG - N_train_65AMG  # store the remaining number of rows

# define a helper function for calculating RMSE
rmse = function(y, ypred) {
  sqrt(mean(data.matrix((y-ypred)^2)))
}


k_grid = exp(seq(log(3), log(100), length=100)) %>% round %>% unique

knn_values_65AMG = c(k_grid)
rmse_knn_65AMG = c()

# predict a KNN model for each value of k for 3 <= k <= 100 and store rmse value
for (k_val in knn_values_65AMG) {
  iterations = 0
  sum_rmse = 0
  
  while (iterations < 200) {
    # randomly sample a set of data points to include in the training set
    train_ind_65AMG = sample.int(N_65AMG, N_train_65AMG, replace=FALSE)
    
    # Define the training and testing set
    D_train_65AMG = sclass65AMG[train_ind_65AMG,]
    D_test_65AMG = sclass65AMG[-train_ind_65AMG,]
    
    # Now separate the training and testing sets into features (X) and outcome (y)
    X_train_65AMG = select(D_train_65AMG, mileage)
    y_train_65AMG = select(D_train_65AMG, price)
    X_test_65AMG = select(D_test_65AMG, mileage)
    y_test_65AMG = select(D_test_65AMG, price)
    
    knn_65AMG = knn.reg(X_train_65AMG, X_test_65AMG, y_train_65AMG, k=k_val)
    
    # make a prediction on the testing set
    ypred_knn_65AMG = knn_65AMG$pred
    
    # add the current rmse to the running sum
    sum_rmse = sum_rmse + rmse(y_test_65AMG, ypred_knn_65AMG)
    
    iterations = iterations + 1
  }
  
  # calculate and store the average rmse values for the prediction
  rmse_knn_65AMG[which(k_val == knn_values_65AMG)[[1]]] <- (sum_rmse / iterations)
}

# identify which k value produced the smallest rmse value for the KNN model
print(min(rmse_knn_65AMG))
print(knn_values_65AMG[which.min(rmse_knn_65AMG)])


# randomly sample a set of data points to include in the training set
train_ind_65AMG = sample.int(N_65AMG, N_train_65AMG, replace=FALSE)

# Define the training and testing set
D_train_65AMG = sclass65AMG[train_ind_65AMG,]
D_test_65AMG = sclass65AMG[-train_ind_65AMG,]

# Now separate the training and testing sets into features (X) and outcome (y)
X_train_65AMG = select(D_train_65AMG, mileage)
y_train_65AMG = select(D_train_65AMG, price)
X_test_65AMG = select(D_test_65AMG, mileage)
y_test_65AMG = select(D_test_65AMG, price)

# run KNN model for optimal K value
knn_65AMG = knn.reg(X_train_65AMG, X_test_65AMG, y_train_65AMG, k=knn_values_65AMG[which.min(rmse_knn_65AMG)])

# make a prediction on the testing set and it to the data set
D_test_65AMG$predictedPrice = knn_65AMG$pred


# plot rmse vs. K
RMSEvsK_65AMG_df <- data.frame(knn_values_65AMG, rmse_knn_65AMG)
ggplot(data = RMSEvsK_65AMG_df) + 
  geom_line(mapping = aes(x = knn_values_65AMG, y = rmse_knn_65AMG), color='red') +
  xlab("K") +
  ylab("RMSE") +
  ggtitle("rmse vs. K for Trim Level 65 AMG")

# plot of the fitted model for optimal value of K on testing set
ggplot(data = D_test_65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='blue') +
  geom_line(mapping = aes(x = mileage, y = predictedPrice), color='red') +
  xlab("Mileage (mi)") +
  ylab("Price ($)") +
  ggtitle("Fitted Model of KNN on Trim Level 65 AMG on Testing Set")
