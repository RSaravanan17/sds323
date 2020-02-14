library(mosaic)
library(tidyverse)

#read in data
greenbuildings = read.csv('./data/greenbuildings.csv')

#filter out low occupancy buildings
gb_modified <- greenbuildings %>%
  filter(leasing_rate > 10 & net == 0)

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
  geom_boxplot() +
  ggtitle("The Employment Growth Rate of Tenants of Green vs Non-Green Buildings") +
  xlab("Non-Green (0) vs Green (1)") + 
  ylab("Employment Growth Rate") +
  theme(plot.title = element_text(hjust = 0.5)) #+
  #(limits = c(20,-20)) #to remove outliers
  
#cluster rent rate - green buildings higher
ggplot(gb_modified, aes(x = as.factor(green_rating), y = cluster_rent)) +
  geom_boxplot() +
  ggtitle("The Cluster Rent of Green vs Non-Green Buildings") +
  xlab("Non-Green (0) vs Green (1)") + 
  ylab("Cluster Rent") +
  theme(plot.title = element_text(hjust = 0.5)) #+
  #scale_y_continuous(limits = quantile(gb_modified$cluster_rent, c(0.1, 0.9))) #to remove outliers

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
  geom_boxplot() +
  ggtitle("The Age of Green vs Non-Green Buildings") +
  xlab("Non-Green (0) vs Green (1)") + 
  ylab("Age") +
  theme(plot.title = element_text(hjust = 0.5))
#scale_y_continuous(limits = quantile(gb_modified$leasing_rate, c(0.1, 0.9))) #to remove outliers

#electricity costs - green buildings expensive
ggplot(gb_modified, aes(x = as.factor(green_rating), y = Electricity_Costs)) +
  geom_boxplot() +
  ggtitle("The Electricity Costs of Green vs Non-Green Buildings") +
  xlab("Non-Green (0) vs Green (1)") + 
  ylab("Electricity Costs") +
  theme(plot.title = element_text(hjust = 0.5))
#scale_y_continuous(limits = quantile(gb_modified$leasing_rate, c(0.1, 0.9))) #to remove outliers


##SCATTER PLOTS WITH DISTINCT COLORS

#age vs rent
ggplot(data = gb_modified) +
  geom_point(mapping=aes(x = age, y = Rent, colour = as.factor(green_rating))) +
  labs(color=guide_legend(title="Green vs Non-Green buildings")) +
  ggtitle("Rent vs Age of Green vs Non-Green Buildings") +
  theme(plot.title = element_text(hjust = 0.5))

#employment growth vs rent
ggplot(data = gb_modified) +
  geom_point(mapping=aes(x = empl_gr, y = Rent, colour = as.factor(green_rating))) +
  labs(color=guide_legend(title="Green vs Non-Green buildings")) +
  ggtitle("Employment Growth Rate vs Age of Green vs Non-Green Buildings") +
  theme(plot.title = element_text(hjust = 0.5))

#electricity costs vs rent
ggplot(data = gb_modified) +
  geom_point(mapping=aes(x = Electricity_Costs, y = Rent, colour = as.factor(green_rating))) +
  labs(color=guide_legend(title="Green vs \nNon-Green buildings")) +
  ggtitle("Rent vs Electricity_Costs of \nGreen vs Non-Green Buildings") +
  theme(plot.title = element_text(hjust = 0.5))

#cluster rent vs rent
ggplot(data = gb_modified) +
  geom_point(mapping=aes(x = cluster_rent, y = Rent, colour = as.factor(green_rating))) +
  labs(color=guide_legend(title="Green vs \nNon-Green buildings")) +
  ggtitle("Rent vs Cluster Rent of \nGreen vs Non-Green Buildings") +
  theme(plot.title = element_text(hjust = 0.5))

#size vs rent
ggplot(data = gb_modified) +
  geom_point(mapping=aes(x = size, y = Rent, colour = as.factor(green_rating)))

#stories vs rent
ggplot(data = gb_modified) +
  geom_point(mapping=aes(x = stories, y = Rent, colour = as.factor(green_rating)))

#cluster rent vs rent
ggplot(data = gb_modified) +
  geom_point(mapping=aes(x = cluster_rent, y = Rent, colour = as.factor(green_rating)))
ggplot(gb_modified, aes(x = as.factor(green_rating), y = cluster_rent)) +
  geom_boxplot()


#correlations
cor(gb_modified)
