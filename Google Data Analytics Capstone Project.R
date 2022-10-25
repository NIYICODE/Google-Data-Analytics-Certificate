#--------Good Data Analytics Capstone Project-------
## OLANIYI ELUWOLE
## 06-10-2022


#----- The Data Cleaning Process

# Step One:Load Libraries

install.packages("tidyverse")
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)

options(scipen = 999)

# Step Two:Import all data individually

January  <- read_csv("202101-divvy-tripdata.csv")
February <- read_csv("202102-divvy-tripdata.csv")
March    <- read_csv("202103-divvy-tripdata.csv")
April    <- read_csv("202104-divvy-tripdata.csv")
May      <- read_csv("202105-divvy-tripdata.csv")
June     <- read_csv("202106-divvy-tripdata.csv")
July     <- read_csv("202107-divvy-tripdata.csv")
August   <- read_csv("202108-divvy-tripdata.csv")
September<- read_csv("202109-divvy-tripdata.csv")
October  <- read_csv("202110-divvy-tripdata.csv")
November <- read_csv("202111-divvy-tripdata.csv")
December <- read_csv("202112-divvy-tripdata.csv")

# Step Three:Check Column Names for Consistency

colnames(January)
colnames(February)
colnames(March)
colnames(April)
colnames(May)
colnames(June)
colnames(July)
colnames(August)
colnames(September)
colnames(October)
colnames(November)
colnames(December)

#-------- Merge all data together

all_months <- rbind(January,February,March,April,May,June,July,August,September,October,November,December)

#------- Remove Monthly data to clear working environment
remove(January,February,March,April,May,June,July,August,September,October,November,December)

#------ Inspect dataframes and look out for incompatibility

str(all_months)

#------- Convert ride_length to numerical for proper calculation

all_months <- mutate(all_months, ride_length=as.numeric(ride_length))

#------- Remove rows with missing data

all_months <- na.omit(all_months)

#------- Graphical Representations

# Total rides per year by casual riders and members

all_months %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual) %>%
  ggplot(aes(x = member_casual,y = number_of_rides, fill = member_casual)) +
  labs(fill = 'Rider Group') +
  geom_col(position = 'dodge') +
  ggtitle(label = 'Total Rides per Year', subtitle = 'Casual Riders Versus Members') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Rider Group') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides), vjust = 0)

# Rides per day by casual riders and members

all_months$day_of_week <- ordered(all_months$day_of_week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
all_months %>%
  group_by(member_casual,day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual,day_of_week) %>%
  ggplot(aes(x = day_of_week,y = number_of_rides,fill = member_casual)) +
  labs(fill = 'Rider Group') +
  geom_col(position = 'dodge') +
  ggtitle(label = 'CUMULATIVE RIDES PER DAY',subtitle = 'Casual Riders vs Members') +
  theme(plot.title = element_text(hjust = 0.6)) +
  theme(plot.subtitle = element_text(hjust = 0.6)) +
  xlab('Weekdays') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides, hjust = 'right'),position = position_dodge(width = 0.8),size = 4,angle = 90)

# Average ride length by casual riders and members

all_months%>%
  group_by(member_casual) %>% 
  summarise(average_duration = mean(ride_length)) %>% 
  arrange(member_casual)  %>% 
  ggplot(aes(x = member_casual, 
             y = average_duration/60, 
             fill = member_casual)) +
  labs(fill='Rider Group') +
  geom_col(position = "dodge") +
  ggtitle(label = 'Average Ride Length', subtitle = 'Casual Riders vs Members') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Rider Group') + ylab('Trip Duration in Minutes') +
  geom_text(aes(label = round(average_duration/60, digits = 2)), vjust = 0)

# Weekly average ride length by Casual riders and Members

all_months%>%
  group_by(member_casual, day_of_week)%>%
  summarise(average_duration = mean(ride_length))%>%
  arrange(member_casual, day_of_week)%>%
  ggplot(aes(x = day_of_week, y=average_duration/60, fill = member_casual)) +
  labs(fill = 'Rider Group') +
  geom_col(position = 'dodge') +
  ggtitle(label = 'Weekly Average Ride Length', subtitle = 'Casual Riders vs Members') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('day_of_week') + ylab('Trip length in minutes') +
  geom_text(aes(label = round(average_duration/60, digits = 2), hjust = 'right'),position = position_dodge(width = 0.8),size = 4,angle = 90)

# Bike Preference by Rider group

all_months%>%
  group_by(member_casual, rideable_type)%>%
  summarise(number_of_rides = n())%>%
  arrange(member_casual, rideable_type)%>%
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  labs(fill = 'Rider Group') +
  geom_col(position = 'dodge') +
  ggtitle(label = 'Total Rides per Year by Bike Type', subtitle = 'Casual Riders vs Members') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Bike Type') + ylab('Number of Rides') +
  geom_text(aes(label = number_of_rides),position = position_dodge(width = 0.8), size = 4, vjust = 0)

# Average Ride Length per bike type

all_months%>%
  group_by(member_casual, rideable_type)%>%
  summarise(average_duration = mean(ride_length))%>%
  arrange(member_casual, rideable_type)%>%
  ggplot(aes(x = rideable_type, y=average_duration/60, fill = member_casual)) +
  labs(fill = 'Rider Group') +
  geom_col(position = 'dodge') +
  ggtitle(label = 'Average Ride Length by Bike Type', subtitle = 'Casual Riders vs Members') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  xlab('Bike Type') + ylab('Trip length in minutes') +
  geom_text(aes(label = round(average_duration/60, digits = 2)), position = position_dodge(width = 0.8), vjust = 0)
