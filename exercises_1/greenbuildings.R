library(mosaic)
library(tidyverse)

#read in data
greenbuildings = read.csv('./data/greenbuildings.csv')

#filter out low occupancy buildings
gb_modified <- greenbuildings %>%
  filter(leasing_rate > 10)

#boxplots of green vs non green housing
#rent
ggplot(gb_modified, aes(x = as.factor(green_rating), y = Rent)) +
  geom_boxplot() #+
  #scale_y_continuous(limits = quantile(gb_modified$Rent, c(0.1, 0.9))) #to remove outliers

#size - green buildings higher
ggplot(gb_modified, aes(x = as.factor(green_rating), y = size)) +
  geom_boxplot() #+
#scale_y_continuous(limits = quantile(gb_modified$size, c(0.1, 0.9))) #to remove outliers

#employment growth rate - green buildings higher
ggplot(gb_modified, aes(x = as.factor(green_rating), y = empl_gr)) +
  geom_boxplot() #+
  #scale_y_continuous(limits = quantile(gb_modified$empl_gr, c(0.1, 0.9))) #to remove outliers

#leasing rate - green buildings higher
ggplot(gb_modified, aes(x = as.factor(green_rating), y = leasing_rate)) +
  geom_boxplot() #+
#scale_y_continuous(limits = quantile(gb_modified$leasing_rate, c(0.1, 0.9))) #to remove outliers

#stories - green buildings higher
ggplot(gb_modified, aes(x = as.factor(green_rating), y = stories)) +
  geom_boxplot() #+
#scale_y_continuous(limits = quantile(gb_modified$leasing_rate, c(0.1, 0.9))) #to remove outliers

#age - green buildings newer
ggplot(gb_modified, aes(x = as.factor(green_rating), y = age)) +
  geom_boxplot() #+
#scale_y_continuous(limits = quantile(gb_modified$leasing_rate, c(0.1, 0.9))) #to remove outliers

#renovated


#class a

#class b
