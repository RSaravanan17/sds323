library(mosaic)
library(tidyverse)

# read in data
abia = read.csv('./data/ABIA.csv')

# adding a month name column to the original dataset
abia$Month_name <- rep(0)
abia$Month_name[which(abia$Month==1)] <- "Jan"
abia$Month_name[which(abia$Month==2)] <- "Feb"
abia$Month_name[which(abia$Month==3)] <- "Mar"
abia$Month_name[which(abia$Month==4)] <- "Apr"
abia$Month_name[which(abia$Month==5)] <- "May"
abia$Month_name[which(abia$Month==6)] <- "Jun"
abia$Month_name[which(abia$Month==7)] <- "Jul"
abia$Month_name[which(abia$Month==8)] <- "Aug"
abia$Month_name[which(abia$Month==9)] <- "Sep"
abia$Month_name[which(abia$Month==10)] <- "Oct"
abia$Month_name[which(abia$Month==11)] <- "Nov"
abia$Month_name[which(abia$Month==12)] <- "Dec"


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

abia_filtered_10 <- abia %>%
  filter(Dest == "CLE")

abia_filtered_11 <- abia %>%
  filter(Dest == "MCO")

abia_filtered_12 <- abia %>%
  filter(Dest == "FLL")

abia_filtered_13 <- abia %>%
  filter(Dest == "CVG")

abia_filtered_14 <- abia %>%
  filter(Dest == "IAH")


# box plot of filtered data (removed outliers, modified scale)
ggplot(data = abia_filtered_4) + 
  geom_boxplot(mapping=aes(x = reorder(UniqueCarrier, CarrierDelay), y = CarrierDelay, fill = UniqueCarrier), outlier.shape = NA) +
  scale_y_continuous(limits = quantile(abia_filtered_4$CarrierDelay, c(0.1, 0.9)))

ggplot(data = abia_filtered_1) + 
  geom_point(mapping = aes(x = CRSDepTime, y = DepTime)) + 
  facet_wrap(~ UniqueCarrier, nrow = 2)


# number of flights of each unique carrier departing from AUS
ggplot(data = abia_filtered_1, aes(x = UniqueCarrier, fill = UniqueCarrier)) + 
  geom_bar() +
  xlab("Unique Carrier") +
  ylab("Number of Flights Departing from AUS") +
  coord_flip()


# number of flights of each unique carrier arriving in AUS
ggplot(data = abia_filtered_2, aes(x = UniqueCarrier, fill = UniqueCarrier)) + 
  geom_bar() +
  xlab("Unique Carrier") +
  ylab("Number of Flights Arriving in AUS") +
  coord_flip()


##########################################################################################################
# bar plot of carrier delay by unique carrier

#abia_carrier_delay = abia_filtered_3 %>%
  #group_by(UniqueCarrier)  %>%  # group the data points by model name
  #summarize(CarrierDelay.mean = mean(CarrierDelay))  # calculate a mean for each model

df  <- data.frame(abia$UniqueCarrier, abia$CarrierDelay)
colnames(df) <- c("UniqueCarrier","CarrierDelay")
df <- na.omit(df)
mean_delay_by_carrier <- aggregate(df$CarrierDelay, list(df$UniqueCarrier), mean)
colnames(mean_delay_by_carrier) <- c("UniqueCarrier", "MeanCarrierDelay")

mean_delay_by_carrier$UniqueCarrierName <- rep(0,nrow(mean_delay_by_carrier)) 
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="YV")] <- "Mesa Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="EV")] <- "ExpressJet"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="NW")] <- "Northwest Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="XE")] <- "JetSuiteX"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="UA")] <- "United Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="OH")] <- "PSA Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="DL")] <- "Delta Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="OO")] <- "SkyWest Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="F9")] <- "Frontier Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="B6")] <- "JetBlue"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="US")] <- "US Airways"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="9E")] <- "Endeavor Air"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="AA")] <- "American Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="CO")] <- "Continental Express"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="WN")] <- "Southwest Airlines"
mean_delay_by_carrier$UniqueCarrierName[which(mean_delay_by_carrier$UniqueCarrier=="MQ")] <- "American Eagle Airlines"

ggplot(mean_delay_by_carrier, aes(x = reorder(UniqueCarrierName, MeanCarrierDelay), y = MeanCarrierDelay, fill = UniqueCarrierName)) + 
  geom_bar(stat='identity') +
  ylab("Mean Carrier Delay") +
  xlab("Unique Carrier") +
  ggtitle("Mean Carrier Delay by Unique Carrier in 2008") +
  coord_flip()

##########################################################################################################
# which month do people fly the farthest?

#abia_month_by_distance = abia %>%
  #group_by(Month_name)  %>%  # group the data points by month
  #summarize(Distance.mean = mean(Distance))  # calculate a mean distance for each month

df  <- data.frame(abia$Month, abia$Distance)
colnames(df) <- c("Month_Num","Distance")
df <- na.omit(df)
mean_distance_monthly <- aggregate(df$Distance, list(df$Month_Num), mean)
colnames(mean_distance_monthly) <- c("Month_Num", "Mean_Distance")

mean_distance_monthly$Month_Name <- rep(0,nrow(mean_distance_monthly)) 
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==1)] <- "Jan"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==2)] <- "Feb"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==3)] <- "Mar"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==4)] <- "Apr"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==5)] <- "May"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==6)] <- "Jun"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==7)] <- "Jul"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==8)] <- "Aug"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==9)] <- "Sep"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==10)] <- "Oct"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==11)] <- "Nov"
mean_distance_monthly$Month_Name[which(mean_distance_monthly$Month_Num==12)] <- "Dec"

ggplot(mean_distance_monthly, aes(x = reorder(Month_Name, Month_Num), y = Mean_Distance, fill = reorder(Month_Name, Month_Num))) + 
  geom_bar(stat = 'identity') +
  ylab("Mean Distance Travelled (miles)") +
  xlab("Month") +
  ggtitle("Mean Distance Traveled by Month in 2008")

##########################################################################################################
# which day of the week do people fly the farthest?

#abia_dayWeek_by_distance = abia %>%
  #group_by(DayOfWeek)  %>%  # group the data points by day of week
  #summarize(Distance.mean = mean(Distance))  # calculate a mean distance for each week

df  <- data.frame(abia$DayOfWeek, abia$Distance)
colnames(df) <- c("DayOfWeek_Num","Distance")
df <- na.omit(df)
mean_distance_daily_by_week <- aggregate(df$Distance, list(df$DayOfWeek_Num), mean)
colnames(mean_distance_daily_by_week) <- c("DayOfWeek_Num", "Mean_Distance")

mean_distance_daily_by_week$DayOfWeek_Name <- rep(0,nrow(mean_distance_daily_by_week)) 
mean_distance_daily_by_week$DayOfWeek_Name[which(mean_distance_daily_by_week$DayOfWeek_Num==1)] <- "Sun"
mean_distance_daily_by_week$DayOfWeek_Name[which(mean_distance_daily_by_week$DayOfWeek_Num==2)] <- "Mon"
mean_distance_daily_by_week$DayOfWeek_Name[which(mean_distance_daily_by_week$DayOfWeek_Num==3)] <- "Tue"
mean_distance_daily_by_week$DayOfWeek_Name[which(mean_distance_daily_by_week$DayOfWeek_Num==4)] <- "Wed"
mean_distance_daily_by_week$DayOfWeek_Name[which(mean_distance_daily_by_week$DayOfWeek_Num==5)] <- "Thu"
mean_distance_daily_by_week$DayOfWeek_Name[which(mean_distance_daily_by_week$DayOfWeek_Num==6)] <- "Fri"
mean_distance_daily_by_week$DayOfWeek_Name[which(mean_distance_daily_by_week$DayOfWeek_Num==7)] <- "Sat"

ggplot(mean_distance_daily_by_week, aes(x = reorder(DayOfWeek_Name, DayOfWeek_Num), y = Mean_Distance, fill = reorder(DayOfWeek_Name, DayOfWeek_Num))) + 
  geom_bar(stat = 'identity') +
  ylab("Mean Distance Travelled (miles)") +
  xlab("Day of Week") +
  ggtitle("Mean Distance Traveled by Day of Week in 2008") +
  coord_cartesian(ylim = c(650, 750))

##########################################################################################################

# which day of the month do people fly the farthest?
abia_dayMonth_by_distance = abia %>%
  group_by(DayofMonth)  %>%  # group the data points by day of month
  summarize(Distance.mean = mean(Distance))  # calculate a mean distance for each month

ggplot(abia_dayMonth_by_distance, aes(x = DayofMonth, y = Distance.mean, fill = DayofMonth)) + 
  geom_bar(stat='identity') +
  ylab("Distance") +
  xlab("Day Of Month") +
  coord_cartesian(ylim = c(675, 725))


# taxi in times at ABIA vs taxi out times at ABIA (facet by unique carrier?)
ggplot(data = abia) + 
  geom_point(mapping = aes(x = TaxiIn, y = TaxiOut, color = UniqueCarrier))

# taxi out and in times at ABIA vs taxi out and in times at other airports

##########################################################################################################
# which airline had the most cancelled flights?

df  <- data.frame(abia$UniqueCarrier, abia$Cancelled)
colnames(df) <- c("UniqueCarrier","Cancelled")
df <- na.omit(df)
num_cancelled_by_carrier <- aggregate(df$Cancelled, list(df$UniqueCarrier), sum)
colnames(num_cancelled_by_carrier) <- c("UniqueCarrier", "NumCancelled")

num_cancelled_by_carrier$UniqueCarrierName <- rep(0,nrow(num_cancelled_by_carrier)) 
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="YV")] <- "Mesa Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="EV")] <- "ExpressJet"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="NW")] <- "Northwest Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="XE")] <- "JetSuiteX"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="UA")] <- "United Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="OH")] <- "PSA Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="DL")] <- "Delta Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="OO")] <- "SkyWest Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="F9")] <- "Frontier Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="B6")] <- "JetBlue"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="US")] <- "US Airways"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="9E")] <- "Endeavor Air"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="AA")] <- "American Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="CO")] <- "Continental Express"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="WN")] <- "Southwest Airlines"
num_cancelled_by_carrier$UniqueCarrierName[which(num_cancelled_by_carrier$UniqueCarrier=="MQ")] <- "American Eagle Airlines"

ggplot(num_cancelled_by_carrier, aes(x = reorder(UniqueCarrierName, NumCancelled), y = NumCancelled, fill = UniqueCarrierName)) + 
  geom_bar(stat='identity') +
  ylab("Number of Cancelled Flights") +
  xlab("Unique Carrier") +
  ggtitle("Number of Cancelled Flights by Unique Carrier in 2008") +
  coord_flip()

##########################################################################################################


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

##########################################################################################################
# incoming flights at DFW over time
abia_flights_to_dfw = abia_filtered_6 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_dfw, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to DFW")

##########################################################################################################
# incoming flights at SFO over time
abia_flights_to_sfo = abia_filtered_7 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_sfo, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to SFO")

##########################################################################################################
# incoming flights at LAX over time
abia_flights_to_lax = abia_filtered_8 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_lax, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to LAX")

##########################################################################################################
# incoming flights at JFK over time
abia_flights_to_jfk = abia_filtered_9 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_jfk, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to JFK")

##########################################################################################################
# incoming flights at CLE over time

abia_flights_to_cle = abia_filtered_10 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_cle, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to CLE")

##########################################################################################################

# incoming flights at MCO over time
abia_flights_to_mco = abia_filtered_11 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_mco, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to MCO")

##########################################################################################################
# incoming flights at FLL over time
abia_flights_to_fll = abia_filtered_12 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_fll, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to FLL")

##########################################################################################################
# incoming flights at CIN over time

abia_flights_to_cin = abia_filtered_13 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_cin, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to CIN")

##########################################################################################################
# incoming flights at IAH over time

abia_flights_to_iah = abia_filtered_14 %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_to_iah, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  ggtitle("Flights by Month to IAH")

##########################################################################################################

# facet number of flights per month by destination
abia_flights_by_month_to_dest = abia %>%
  group_by(Month)  %>%  # group the data points by month
  summarize(NotCancelled.sum = sum(Cancelled == 0))  # calculate sum of non-cancelled flights

ggplot(abia_flights_by_month_to_dest, aes(x=Month, y=NotCancelled.sum)) + 
  geom_density(stat='identity', fill = "#BF5700") +
  ylab("Number of Flights") +
  xlab("Month") +
  facet_wrap(~ Dest, nrow = 2)
