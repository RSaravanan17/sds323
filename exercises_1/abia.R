library(mosaic)
library(tidyverse)

# read in data
abia = read.csv('./data/ABIA.csv')

# plotting departure time vs scheduled departure time
ggplot(data = abia) + 
  geom_point(mapping = aes(x = CRSDepTime, y = DepTime, color = UniqueCarrier))

# filter data
abia_filtered_1 <- abia %>%
  filter(Dest == "AUS" & !is.na(CarrierDelay))

# show filtered data
abia_filtered_1

# box plot of filtered data (removed outliers, modified scale)
ggplot(data = abia_filtered_1) + 
  geom_boxplot(mapping=aes(x = UniqueCarrier, y = CarrierDelay), outlier.shape = NA) +
  scale_y_continuous(limits = quantile(abia_filtered_1$CarrierDelay, c(0.1, 0.9)))

