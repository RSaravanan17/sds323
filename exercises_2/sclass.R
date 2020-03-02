library(mosaic)
library(tidyverse)
library(FNN)

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
  geom_point(mapping = aes(x = mileage, y = price), color='blue')

ggplot(data = sclass65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='red')
