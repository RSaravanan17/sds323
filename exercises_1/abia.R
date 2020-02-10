library(mosaic)
library(tidyverse)

# read in data
abia = read.csv('./data/ABIA.csv')


# plotting departure time vs scheduled departure time
ggplot(data = abia) + 
  geom_point(mapping = aes(x = CRSDepTime, y = DepTime, color = UniqueCarrier))


# filter data
abia_filtered_1 <- abia %>%
  filter(Origin == "AUS")

abia_filtered_2 <- abia %>%
  filter(Dest == "AUS")

abia_filtered_3 <- abia %>%
  filter(Origin == "AUS" & !is.na(CarrierDelay))

abia_filtered_4 <- abia %>%
  filter(Dest == "AUS" & !is.na(CarrierDelay))

abia_filtered_5 <- abia %>%
  filter(Cancelled == 1)

abia_filtered_6 <- abia %>%
  filter(Dest == "DFW")

abia_filtered_7 <- abia %>%
  filter(Dest == "SFO")

abia_filtered_8 <- abia %>%
  filter(Dest == "LAX")

abia_filtered_9 <- abia %>%
  filter(Dest == "JFK")


# show filtered data
abia_filtered_4


# box plot of filtered data (removed outliers, modified scale)
ggplot(data = abia_filtered_4) + 
  geom_boxplot(mapping=aes(x = UniqueCarrier, y = CarrierDelay), outlier.shape = NA) +
  scale_y_continuous(limits = quantile(abia_filtered_4$CarrierDelay, c(0.1, 0.9)))

ggplot(data = abia_filtered_1) + 
  geom_point(mapping = aes(x = CRSDepTime, y = DepTime)) + 
  facet_wrap(~ UniqueCarrier, nrow = 2)


# number of flights of each unique carrier departing from AUS
ggplot(data = abia_filtered_1, aes(x = UniqueCarrier, fill = UniqueCarrier)) + 
  geom_bar()


# number of flights of each unique carrier arriving in AUS
ggplot(data = abia_filtered_2, aes(x = UniqueCarrier, fill = UniqueCarrier)) + 
  geom_bar()


# bar plot of carrier delay by unique carrier
abia_carrier_delay = abia_filtered_3 %>%
  group_by(UniqueCarrier)  %>%  # group the data points by model name
  summarize(CarrierDelay.mean = mean(CarrierDelay))  # calculate a mean for each model

ggplot(abia_carrier_delay, aes(x=reorder(UniqueCarrier, CarrierDelay.mean), y=CarrierDelay.mean)) + 
  geom_bar(stat='identity', fill = "#BF5700") +
  ylab("Mean Carrier Delay") +
  xlab("Unique Carrier") +
  coord_flip()


# which month do people fly the farthest?
abia_month_by_distance = abia %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(Distance.mean = mean(Distance))  # calculate a mean distance for each month

ggplot(abia_month_by_distance, aes(x=reorder(Month, Distance.mean), y=Distance.mean)) + 
  geom_bar(stat='identity', fill = "#BF5700") +
  ylab("Distance") +
  xlab("Month") +
  coord_flip()


# which day of the week do people fly the farthest?
abia_dayWeek_by_distance = abia %>%
  group_by(DayOfWeek)  %>%  # group the data points by day of week
  summarize(Distance.mean = mean(Distance))  # calculate a mean distance for each week

ggplot(abia_dayWeek_by_distance, aes(x=reorder(DayOfWeek, Distance.mean), y=Distance.mean)) + 
  geom_bar(stat='identity', fill = "#BF5700") +
  ylab("Distance") +
  xlab("Day Of Week") +
  coord_flip()


# which day of the month do people fly the farthest?
abia_dayMonth_by_distance = abia %>%
  group_by(DayofMonth)  %>%  # group the data points by day of month
  summarize(Distance.mean = mean(Distance))  # calculate a mean distance for each month

ggplot(abia_dayMonth_by_distance, aes(x=reorder(DayofMonth, Distance.mean), y=Distance.mean)) + 
  geom_bar(stat='identity', fill = "#BF5700") +
  ylab("Distance") +
  xlab("Day Of Month") +
  coord_flip()


# taxi in times at ABIA vs taxi out times at ABIA (facet by unique carrier?)
ggplot(data = abia) + 
  geom_point(mapping = aes(x = TaxiIn, y = TaxiOut, color = UniqueCarrier))

# taxi out and in times at ABIA vs taxi out and in times at other airports


# which airline had the most cancelled flights?
abia_cancelled_by_carrier = abia %>%
  group_by(UniqueCarrier)  %>%  # group the data points by unique carrier
  summarize(Cancelled.sum = sum(Cancelled))  # calculate sum of cancelled flights

ggplot(abia_cancelled_by_carrier, aes(x=reorder(UniqueCarrier, Cancelled.sum), y=Cancelled.sum)) + 
  geom_bar(stat='identity', fill = "#BF5700") +
  ylab("Number of Cancelled Flights") +
  xlab("Unique Carrier") +
  coord_flip()


# which flight number had the most cancelled flights?
abia_cancelled_by_flight = abia_filtered_5 %>%
  group_by(FlightNum)  %>%  # group the data points by flight number
  summarize(Cancelled.sum = sum(Cancelled))  # calculate sum of cancelled flights

ggplot(abia_cancelled_by_flight, aes(x=reorder(FlightNum, Cancelled.sum), y=Cancelled.sum)) + 
  geom_bar(stat='identity', fill = "#BF5700") +
  ylab("Number of Cancelled Flights") +
  xlab("Flight Number") +
  coord_flip()


# which airline had the longest mean departure delay?
abia_depDelay_by_carrier = abia %>%
  group_by(UniqueCarrier)  %>%  # group the data points by unique carrier
  summarize(DepDelay.mean = mean(DepDelay))  # calculate sum of cancelled flights

ggplot(abia_depDelay_by_carrier, aes(x=reorder(UniqueCarrier, DepDelay.mean), y=DepDelay.mean)) + 
  geom_bar(stat='identity', fill = "#BF5700") +
  ylab("Mean Departure Delay") +
  xlab("Unique Carrier") +
  coord_flip()


# which holidays (weeks) have the most flights out of ABIA?



# what is the best day of the year to fly out of ABIA? (histogram of number of flights per day/month/season)



# which month has the longest departure delays?
abia_depDelay_by_month = abia %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(DepDelay.mean = mean(DepDelay))  # calculate a mean distance for each month

ggplot(abia_depDelay_by_month, aes(x=reorder(Month, DepDelay.mean), y=DepDelay.mean)) + 
  geom_bar(stat='identity', fill = "#BF5700") +
  ylab("Mean Departure Delay") +
  xlab("Month") +
  coord_flip() +
  scale_y_continuous(limits = c(-500, 500))


# incoming flights at DFW over time
abia_flights_to_dfw = abia_filtered_6 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_dfw, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month")


# incoming flights at SFO over time
abia_flights_to_sfo = abia_filtered_7 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_dfw, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month")


# incoming flights at LAX over time
abia_flights_to_dfw = abia_filtered_8 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_dfw, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month")


# incoming flights at JFK over time
abia_flights_to_dfw = abia_filtered_9 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_dfw, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month")


# facet number of flights per month by destination
abia_flights_by_month_to_dest = abia %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_by_month_to_dest, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  facet_wrap(~ Dest, nrow = 2)
