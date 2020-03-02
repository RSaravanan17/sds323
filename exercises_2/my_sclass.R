library(tidyverse)
library(FNN)

# load the data
sclass = read.csv('./data/sclass.csv')
summary(sclass)

# filter by trim (350 and 65 AMG)
df_trim_350 <- sclass %>%
  filter(trim == "350")

df_trim_65AMG <- sclass %>%
  filter(trim == "65 AMG")

# plot the data
ggplot(data = df_trim_350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='blue')

ggplot(data = df_trim_65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='red')
