# Uber Pickups in New York City dataset.
# a data visualization project.

library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

#color vector
colors = c("#CC1011", "#000080", "#228B22", "#BC8F8F", "#f5e840", "#665555", "##F0F8FF")

# read data from .csv file
apr_data <- read.csv("G:/2-Study/Uber-dataset/uber-raw-data-apr14.csv")
may_data <- read.csv("G:/2-Study/Uber-dataset/uber-raw-data-may14.csv")
jun_data <- read.csv("G:/2-Study/Uber-dataset/uber-raw-data-jun14.csv")
jul_data <- read.csv("G:/2-Study/Uber-dataset/uber-raw-data-jul14.csv")
aug_data <- read.csv("G:/2-Study/Uber-dataset/uber-raw-data-aug14.csv")
sep_data <- read.csv("G:/2-Study/Uber-dataset/uber-raw-data-sep14.csv")

# combine six dataframe into one dataframe by rows
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

# formatting foe the data and time
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

# factors of date -> day, month, year, day of week
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
#factor of time -> hour, minute, second
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))


#1.trips every hour
# use the ggplot function to plot the number of trips that the passengers had made in a day
# use dplyr to aggregate our data
# %>% --> allow pipes and makes code readable/writable from left-to-right.
hour_data <- data_2014 %>% group_by(hour) %>%
  dplyr::summarize(Total = n())  # a variable named Total will be assigned the number of observations in the summarized data.
datatable(hour_data)

# trips every hour
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "orange", color = "blue") +
  ggtitle("Total Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# trips by hour and month
month_hour <- data_2014 %>% group_by(month, hour) %>%
  dplyr::summarize(Total = n())
ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Total Trips by Hour and Month") +
  scale_y_continuous(labels = comma)


#2.trips during every day of the month
day_group <- data_2014 %>% group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "orange",color = "blue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())
ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


#3. Number of Trips taking place during months in a year
month_group <- data_2014 %>% group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

ggplot(month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

colors2 = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
           
month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())
ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors2)

# 4. number of trips by base stations
ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors2)

# 5. heatmap day, hour, month
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

month_base <-  data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 
day0fweek_bases <-  data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 
ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")

ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

