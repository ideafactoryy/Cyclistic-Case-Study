
#Markdown code block

# 12 files are added to a list
  file_list <- list('202105-divvy-tripdata.csv', '202106-divvy-tripdata.csv', '202107-divvy-tripdata.csv',
                    '202108-divvy-tripdata.csv', '202109-divvy-tripdata.csv', '202110-divvy-tripdata.csv', 
                    '202111-divvy-tripdata.csv', '202112-divvy-tripdata.csv', '202201-divvy-tripdata.csv', 
                    '202202-divvy-tripdata.csv', '202203-divvy-tripdata.csv', '202204-divvy-tripdata.csv', 
                    '202205-divvy-tripdata.csv')
  #files used are tallied
  number_of_files = length(file_list)
  
  #variable used in parsing loop
  number_of_loops = 1
  
  #loop for parsing data files
  #using this method means only 1 file at a time is kept in memory
  #one year worth of data is several gigabytes.  
  #While the system I am using can handle that much data, others may not be able to
  #This also makes the system more scaleable

  while (number_of_files >= number_of_loops) 
    {
    # one month of data is read
    monthly_data <- read_csv(file_list[number_of_loops]) %>%
      
      #entries with no geolocation are dropped
      drop_na(start_lat, start_lng, end_lat, end_lng)
    
    # we shuffle the dataframe before we take a sample to ensure we are not creating a bias
    monthly_data = monthly_data[sample(1:nrow(monthly_data)), ] 
    
    # the number of data entries is counted and a number equal to 2% or 1/50th is stored
    # this number is used to reduce the data to a 2% sample size
    sample_size = as.integer(round(count(monthly_data)*.020, digits = 0)) 
    monthly_data = head(monthly_data, n = sample_size)
    
    #the month of data is used to create a file and consecutive months are added to the file
    if(number_of_loops == 1) 
      write_csv(monthly_data, file = "monthly_data_total.csv", append = TRUE, col_names = TRUE) 
       else   write_csv(monthly_data, file = "monthly_data_total.csv", append = TRUE, col_names = FALSE)
    
    #the loop repeats
    number_of_loops = number_of_loops + 1 
    }

  
  
  #-----------------------------
  monthly_data <- read_csv('202105-divvy-tripdata.csv')
# this shuffles the data in the dataframe
  monthly_data = monthly_data[sample(1:nrow(monthly_data)), ] 
# the number of data entries is counted and a number equal to 2% or 1/50th is stored
  sample_size = as.integer(round(count(monthly_data)*.020, digits = 0)) 
# dataframe is cut down to a sample size of 2% or 1/50th
  monthly_data = head(monthly_data, n = sample_size)
  
# ------------------------------
  if(exists("trip_data_sample")) trip_data_sample <- bind_rows(trip_data_sample, trip_data) else
    trip_data_sample <- trip_data
  trip_data %<>%  #pipe begins
  mutate(started_at = as_datetime(started_at)) %>%
  mutate(ended_at = as_datetime(ended_at)) %>%  #start time and end time are converted to time interval (seconds)
  mutate(ride_length = (ended_at - started_at)/86400) %>% #seconds are converted into days
  filter(ride_length > .00069 & ride_length < 1) %>% #only trips longer than 1 minute and shorter than 1 day are kept
  mutate(day_of_week = wday(started_at)) %>% # date is changed into a numeric 1-7
  select(rideable_type, member_casual,#new columns are selected for the dataframe
        ride_length, day_of_week, start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id, 
        start_station_name, end_station_name) #pipe ends, dataframe is written
write_csv(trip_data_sample, file = "trip_data_snippet.csv", append = TRUE, col_names = TRUE) #dataframe is output to a csv
#------code for rMarkdown report---------------------  
#dataframe is built from file
year_of_data <- read_csv('monthly_data_total.csv')
year_of_data = year_of_data[sample(1:nrow(year_of_data)), ] 
year_of_data = head(year_of_data, 12000)


#this is the start of the "pipe" that will clean and alter our table
year_of_data %<>% 
  # null values in the longitude and latitude are removed
  drop_na(start_lat, start_lng, end_lat, end_lng) %>%
  #the date fields are changed into a different format
  mutate(started_at = as.POSIXct(started_at)) %>%
  mutate(ended_at = as.POSIXct(ended_at)) %>% 
  #columns for time and day are created
  mutate(ride_date = as.Date(started_at)) %>%
  mutate(ride_time = format(as.POSIXct(started_at, format='%Y-%m-%d %H:%M:%S'),format='%H:%M:%S')) %>%
  #a column for day of the week is created
  mutate(day_of_week = wday(started_at, label = TRUE, week_start = 1)) %>% 
  #ride time for each trip is calculated and returned as seconds
  mutate(ride_length = time_length(interval(started_at, ended_at))) %>%
  #trips shorter than 1 minute and longer than 1 day are dropped
  filter(ride_length > 60 & ride_length < 86400) %>% 
  #data is finalized and written to the dataframe
  select(rideable_type, member_casual, started_at, ended_at,
         ride_length, day_of_week, ride_date, ride_time, start_lat, start_lng, end_lat, end_lng, 
         start_station_name, end_station_name, start_station_id, end_station_id) 

#ggplot2 tutorial------------------
# Setup
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
data("midwest", package = "ggplot2")  # load the data
# midwest <- read.csv("http://goo.gl/G1K41K") # alt source 

ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=2) + 
  geom_smooth(method="lm", col="firebrick", span = 1) + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", 
       y="Population", x="Area", caption="Midwest Demographics") +
  scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = letters[1:11])

# Cyclistic data
ggplot(year_of_data, aes(x=started_at, y=ride_length)) + 
  geom_point(aes(col=member_casual)) + 
  scale_color_brewer(palette = "Set1") +
  geom_smooth()  +
  coord_cartesian(ylim=c(0, 7200) , 
                          xlim=c(as.POSIXct("2022-5-01 05:00:07"), 
                                 as.POSIXct("2022-5-30 023:32:07")))  +
  labs(title="Ride length in May, 2022", subtitle="Between one minute and two hours", 
       y="seconds", x="Timeframe", caption="Cyclistic") +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2022-5-01 05:00:07"),
                 as.POSIXct("2022-5-30 05:00:07"), "1 week"),
    labels = scales::label_date("%m-%d\n%H:%M"),
    limits = c(
      as.POSIXct("2022-4-01 05:00:07"),
      as.POSIXct("2022-6-30 023:32:07"))) +
  scale_y_continuous(breaks = seq(0, 7200, 3600), label = c(" ", "1 hour", "2 hours"))



#----------------------end

#v--makes a table of ride time averages by day of week--v
average_ride_time <- year_of_data
average_ride_time %<>%
  group_by(day_of_week, member_casual) %>%


average_ride_time %<>%
  group_by(day_of_week, member_casual) %>%
  summarise_at(vars(ride_length), list(ride_time = mean))

tibble(average_ride_time)
  select(day_of_week, member_casual, ride_length)
ggplot(average_ride_time, aes(day_of_week, (ride_time))) +
  geom_bar(stat = 'identity') + facet_wrap(~member_casual) + 
  labs(x = "Weekday", y = "Ride Time in Minutes")

oct28_wtf <- year_of_data %>%
  filter(started_at > as.POSIXct("2021-10-28 00:00:00") & started_at < as.POSIXct("2021-10-28 23:59:59"))

#---- total Ride length by weekday
ggplot(year_of_data, aes(day_of_week, ride_length)) +
   geom_bar(stat = 'identity') + facet_wrap(~member_casual)
#----


  # trip_data <- read_csv('202105-divvy-tripdata.csv') %>%
  #   select(member_casual, started_at, ended_at, ) %>%
  #   mutate(started_at = as_datetime(started_at)) %>%
  #   mutate(ended_at = as_datetime(ended_at)) %>%
  #   arrange()
  #   head(5000) %>%
    
trip_data <- read_csv("trip_data_snippet.csv")
    
# head(freeny.x)
#     
# tibble(trip_data)
# trip_data <- read_csv('202204-divvy-tripdata.csv')
# trip_data_snippet <- head(trip_data, 2000) %>%
#   select(member_casual, started_at, ended_at) %>%
#   mutate(started_at = as_datetime(started_at)) %>%
#   mutate(ended_at = as_datetime(ended_at)) %>%
#   mutate(ride_length = seconds_to_period(ended_at - started_at)) %>%
#   mutate(day_of_week = wday(started_at)) %>%
#   filter(ride_length > 60)
# write.csv(trip_data_snippet, file = "trip_data2.csv", row.names = FALSE)

#  arrange(ride_length) 

# set directory
setwd("C:/Users/Steve/Desktop/Cyclistic Case Study/Data")

library(dplyr)
library(ggplot2)
library(grid, lib.loc = "C:/Program Files/R/R-4.2.1/library")
library(janitor)
library(labeling)
library(lubridate)
library(magrittr)
library(readr)
library(tidyr)
library(tidyselect)
library(tidyverse)    
    
i <- 1
while (i < 6) {
  print(i)
  i = i+1
}

    # create a dataframe of students with id,name and marks
    data=data.frame(id=c(1,2,3,4,5,6),
                    name=c("sravan","bobby","ojaswi","gnanesh",
                           "rohith","satwik"),
                    marks=c(89,90,98,78,98,78))
    
    # display dataframe
    print(data)
    print("_______________________________________________________")
    
    # shuffle the dataframe by rows
    shuffled_data= data[sample(1:nrow(data)), ]
    
    # display
    print(shuffled_data)
    
    
    

    
    
    
    
    