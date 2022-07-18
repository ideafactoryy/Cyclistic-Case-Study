#ggplot2 tutorial------------------
# Setup
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
library(dplyr)
library(grid, lib.loc = "C:/Program Files/R/R-4.2.1/library")
library(janitor)
library(labeling)
library(lubridate)
library(magrittr)
library(readr)
library(tidyr)
library(tidyselect)
library(tidyverse)    
data("midwest", package = "ggplot2")  # load the data
# midwest <- read.csv("http://goo.gl/G1K41K") # alt source 

ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=2) + 
  geom_smooth(method="lm", col="firebrick", span = 1) + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", 
       y="Population", x="Area", caption="Midwest Demographics") +
  scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11])


# Setup
options(scipen=999)
library(ggplot2)

data("midwest", package = "ggplot2")
theme_set(theme_bw())
# midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

# Add plot components
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")
plot(gg)

#----------
mc_ride_length <- year_of_data %>%
  group_by(day_of_week, member_casual) %>%
  summarise_at(vars(ride_length), list(ride_time_total = sum))

mc_avg <- year_of_data %>%
  group_by(day_of_week, member_casual) %>%
  summarise_at(vars(ride_length), list(ride_time_avg = mean))



#average ride times by day---------
mc_avg_ridetime_by_day <- year_of_data %>%
  group_by(ride_date, member_casual) %>%
  summarise_at(vars(ride_length), list(ride_time_avg = mean))
#output as data visual:
#scatterplot showing how daily ride habits change over the year 
ggplot(mc_avg_ridetime_by_day, aes(x = ride_date, y = ride_time_avg)) +
  geom_point(aes(col=member_casual)) +
  scale_color_manual(values = c("member" = "deepskyblue2", "casual" = "red")) +
  geom_smooth(formula = y ~ x, span=.4, method = "loess", 
              aes(col=member_casual), show.legend = FALSE) +
  coord_cartesian(ylim=c(600, 6000), xlim=c(as.Date("2021-5-18"), as.Date("2022-5-14")))  +
  scale_x_continuous(breaks = seq(as.Date("2021-5-1"), as.Date("2022-6-1"), "1 month"),
                     labels = scales::label_date("'%y %b"),
                     limits = c(as.Date("2021-5-1"), as.Date("2022-5-31"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(300, 6000, 900), label = c(" ", "15 min", "30 min", "45 min",
                                                             "1 hour", "1:15", "1:30")) +
  labs(title="Average Ridetime Each Day", subtitle="Each point is a daily average", 
       y="Average Time in Minutes", x="Months", col="Rider Type")
#------------

#total of rides by day dataframe----------
mc_trips_per_day <- year_of_data %>%
  group_by(ride_date = as.Date(ride_date), member_casual) %>%
  summarise(rides_per_day = n())
#rides per day chart
ggplot(mc_trips_per_day, aes(x=ride_date, y=rides_per_day)) + 
  geom_point(aes(col=member_casual)) +
  scale_color_manual(values = c("member" = "deepskyblue2", "casual" = "red")) +
  geom_smooth(formula = y ~ x, span=.4, method = "loess", 
              aes(col=member_casual), show.legend = FALSE) +
  coord_cartesian(xlim=c(as.Date("2021-5-18"), as.Date("2022-5-14")))  +
  scale_x_continuous(breaks = seq(as.Date("2021-5-1"), as.Date("2022-6-1"), "1 month"),
                     labels = scales::label_date("'%y %b"),
                     limits = c(as.Date("2021-5-1"), as.Date("2022-5-31"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(title="Number of Uses per Day", subtitle="Each Point is a Daily Total", 
     y="Number of Rides per Day", x="Months", col="Rider Type")
#---------------

#average of the middle of each trip-------
mc_middle_of_trip_avg <- year_of_data %>%
  group_by(ride_date, member_casual) %>%
  mutate(mid_trip_time = ended_at - time_length(interval(started_at, ended_at)/2)) %>%
  summarise_at(vars(mid_trip_time), list(mid_trip_time = mean)) %>%
  mutate(mid_trip_time = format(as.POSIXct(mid_trip_time, format='%Y-%m-%d %H:%M:%S'),format='%H:%M:%S'))
#avg time of day during trips
ggplot(mc_middle_of_trip_avg, aes(x=ride_date, y=as.POSIXct(mid_trip_time, format='%H:%M:%S'))) +
  geom_point(aes(col=member_casual)) +
  scale_color_manual(values = c("member" = "deepskyblue2", "casual" = "red")) +
   geom_smooth(formula = y ~ x, span=.4, method = "loess", 
               aes(col=member_casual), show.legend = FALSE) +
  coord_cartesian(xlim=c(as.Date("2021-5-18"), as.Date("2022-5-14")), 
                  ylim=c(as.POSIXct("11:00:00", format='%H:%M:%S'), as.POSIXct("19:00:00", format='%H:%M:%S')))  +
  scale_x_continuous(breaks = seq(as.Date("2021-5-1"), as.Date("2022-6-1"), "1 month"),
                     labels = scales::label_date("'%y %b"),
                     limits = c(as.Date("2021-5-1"), as.Date("2022-5-31"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Time of day ridership average", subtitle="Each Point is a Daily Average", 
       y="Time of Day", x="Months", col="Rider Type")
#-------

#average of the start of each trip-------
mc_start_of_trip_avg <- year_of_data %>%
  group_by(ride_date, member_casual) %>%
  mutate(started_at_time = started_at) %>%
  summarise_at(vars(started_at_time), list(start_trip_time_avg = mean)) %>%
  mutate(start_trip_time_avg = format(as.POSIXct(start_trip_time_avg, format='%Y-%m-%d %H:%M:%S'),
                                      format='%H:%M:%S')) 
#avg time of day during trips
ggplot(mc_start_of_trip_avg, aes(x=ride_date, y=as.POSIXct(start_trip_time_avg, format='%H:%M:%S'))) +
  geom_point(aes(col=member_casual)) +
  scale_color_manual(values = c("member" = "deepskyblue2", "casual" = "red")) +
  geom_smooth(formula = y ~ x, span=.4, method = "loess", 
              aes(col=member_casual), show.legend = FALSE) +
  coord_cartesian(xlim=c(as.Date("2021-5-18"), as.Date("2022-5-14")), 
                  ylim=c(as.POSIXct("10:00:00", format='%H:%M:%S'), as.POSIXct("18:00:00", format='%H:%M:%S')))  +
  scale_x_continuous(breaks = seq(as.Date("2021-5-1"), as.Date("2022-6-1"), "1 month"),
                     labels = scales::label_date("'%y %b"),
                     limits = c(as.Date("2021-5-1"), as.Date("2022-5-31"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Time of day ridership start average", subtitle="Each Point is a Daily Average", 
       y="Time of Day", x="Months", col="Rider Type")
#-----
#plot points by latitude and longitude--------
casual_start_loc <- year_of_data %>%
  filter(member_casual == "casual") %>%
  drop_na(start_station_id) %>%
  group_by(start_lat, start_lng) %>%
  summarise(num_uses = n()) %>%
  arrange(desc(num_uses))

ggplot(casual_start_loc, aes(x=start_lng, y=start_lat)) +
  geom_point(aes(size = num_uses, col='pink')) 
  
#-------

#most used pickups by casual members
casual_station_pickup <- year_of_data %>%
  drop_na(start_station_id) %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_name) %>%
  summarise(station_uses = n()) %>%
  mutate(station_use_percentage = percent(station_uses / sum(station_uses), accuracy = .01)) %>% 
  arrange(desc(station_uses))
head(casual_station_pickup, 30)
#the code above throws an error in rmarkdown.  the percent conversion trips up the arrange function
#try again, doing percent manually
station_uses <- year_of_data %>%
  filter(member_casual == "casual") %>%
  drop_na(start_station_name) %>%  
  group_by(start_station_name) %>%
  summarize(total_station_uses = n()) %>%
  filter(total_station_uses > 5) %>%
  mutate(station_use_percent = sprintf("%.2f%%", 
                                       round((total_station_uses / sum(total_station_uses)*100), digits = 2))) %>%
  filter(total_station_uses > 50) %>%
  arrange(desc(total_station_uses))
head(station_uses)

#member and casual avg ride time by week
mc_avg_ridetime_by_week <- year_of_data %>%
  mutate(week = format(started_at, format = "%U")) %>%
  group_by(week, member_casual) %>%
  summarise_at(vars(ride_length), list(ride_time_avg = mean)) 
mc_avg_ridetime_by_week
#output bar chart showing avg ridetime by week
ggplot(mc_avg_ridetime_by_week, aes(x=week, y=ride_time_avg)) +
  geom_bar(aes(fill = member_casual), stat = "identity", position = "dodge2") 

#member and casual avg ride time by month
mc_avg_ridetime_by_month <- year_of_data %>%
  mutate(month_avg = format(started_at, format = "%m")) %>%
  group_by(month_avg, member_casual) %>%
  summarise_at(vars(ride_length), list(ride_time_avg = mean)) 
mc_avg_ridetime_by_month
#output bar chart showing avg ride time by month
ggplot(mc_avg_ridetime_by_month, aes(x=month_avg, y=ride_time_avg)) +
  geom_bar(aes(fill = member_casual), stat = "identity", position = "dodge2") 


#----- count for bar graph example


#-----

# Cyclistic data
# (get df from cyclistic file)

#mc_ride_length <- aggregate(year_of_data$ride_length, by=list(year_of_data$day_of_Week), FUN=mean)


ggplot(year_of_data, aes(x=started_at, y=ride_length)) + 
  geom_point(aes(col=member_casual)) + 
  theme_classic() +
  coord_cartesian(ylim=c(360, 7200) , 
                  xlim=c(as.POSIXct("2021-5-26 05:00:07"), 
                         as.POSIXct("2022-5-30 023:32:07")))  +
  labs(title="Ride length in May, 2022", subtitle="Between one minute and two hours", 
       y="seconds", x="Timeframe", caption="Cyclistic") +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2021-5-26 05:00:07"),
                 as.POSIXct("2022-5-30 05:00:07"), "1 month"),
    labels = scales::label_date("%m-%d/%H%M"),
    limits = c(
      as.POSIXct("2021-5-01 05:00:07"),
      as.POSIXct("2022-6-30 023:32:07"))) +
  scale_y_continuous(breaks = seq(0, 7200, 3600), label = c(" ", "1 hour", "2 hours"))

# plot of avg ride time by day
ggplot(mc_avg_ridetime_by_day, aes(x=ride_date, y=ride_time_avg)) + 
  geom_point(aes(col=member_casual)) +
  coord_cartesian(ylim=c(0, 21600))  +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, 21600, 9000), label = c(" ", "1 hour", "2 hours")) +
  labs(title="Avg ride length by day", subtitle="Between one minute and two hours", 
       y="Time", x="Date", caption="Cyclistic") +
  theme(axis.text.x = element_text(angle=1, vjust=0.6))

#bar graph of avg rides by day
ggplot(mc_avg_ridetime_by_day, aes(x=ride_date, y=ride_time_avg, fill = member_casual)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, 7200, 3600), label = c(" ", "1 hour", "2 hours"))

#bar chart for avg trip time by weekday
ggplot(mc_avg, aes(x=day_of_week, y=ride_time_avg, fill = member_casual)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  scale_y_continuous(breaks = seq(0, 2000, 300), label = c(" ", "5", "10", "15", "20", "25","30")) +
  labs(title="Avg Trip Time each Weekday", subtitle="Over one year", 
       y="Time in Minutes", x="Weekday", caption="Cyclistic", fill="Rider type") +
  theme(axis.text.x = element_text(angle=1, vjust=0.6))

#bar chart for amount of trips each weekday over a year
ggplot(mc_avg, aes(x=day_of_week, y=ride_time_avg, fill = member_casual)) + 
  geom_bar(stat = "identity", position = "dodge2") +
  scale_y_continuous(breaks = seq(0, 2000, 300), label = c(" ", "5", "10", "15", "20", "25","30")) +
  labs(title="Avg Trip Time each Weekday", subtitle="Over one year", 
       y="Time in Minutes", x="Weekday", caption="Cyclistic", fill="Rider type") +
  theme(axis.text.x = element_text(angle=1, vjust=0.6))

#bar chart for number of trips each weekday
ggplot(year_of_data, aes(x=day_of_week)) +
  geom_bar(aes(fill = member_casual), position = "dodge2") +
  scale_fill_manual(values=c("member" = "deepskyblue2", "casual" = "red")) +
  labs(title="Number of Trips on each Weekday", subtitle="Over one year", 
       y="Number of Trips", x="Weekday", caption="Cyclistic", fill="Rider type")








