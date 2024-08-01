## Install and load necessary packages
library(tidyverse)
library(janitor)
library(ggmap)
library(geosphere)
library(lubridate)
library(leaflet)

# Importing all datasets
file_names <- c("202307-divvy-tripdata.csv", "202308-divvy-tripdata.csv", "202309-divvy-tripdata.csv",
                "202310-divvy-tripdata.csv", "202311-divvy-tripdata.csv", "202312-divvy-tripdata.csv",
                "202401-divvy-tripdata.csv", "202402-divvy-tripdata.csv", "202403-divvy-tripdata.csv",
                "202404-divvy-tripdata.csv", "202405-divvy-tripdata.csv", "202406-divvy-tripdata.csv")

# Reading all data sets into a list
data_list <- lapply(file_names, read.csv)

# Combining all data sets into a single data set
raw_data <- do.call(rbind, data_list)

head(raw_data)
dim(raw_data)
str(raw_data)

# Convert started_at and ended_at to POSIXct
raw_data <- raw_data %>%
  mutate(started_at = as.POSIXct(started_at, format = "%d/%m/%Y %H:%M"),
         ended_at = as.POSIXct(ended_at, format = "%d/%m/%Y %H:%M"))

## Adding columns for date, month, year, and day of week.
trip_data <- raw_data %>% 
  mutate(year = format(started_at, "%Y")) %>%  # extract year
  mutate(month = format(started_at, "%B")) %>% # extract month
  mutate(date = format(started_at, "%d")) %>%  # extract date
  mutate(day_of_week = format(started_at, "%A")) %>% # extract day of week
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, units = "secs")) / 60) %>% # calculate ride length in minutes
  mutate(start_time = format(started_at, "%H"))

# Convert ride_length to numeric
trip_data <- trip_data %>% 
  mutate(ride_length = as.numeric(ride_length))

# Add ride distance in km
trip_data$ride_distance <- distGeo(matrix(c(trip_data$start_lng, trip_data$start_lat), ncol = 2), matrix(c(trip_data$end_lng, trip_data$end_lat), ncol = 2))

trip_data$ride_distance <- trip_data$ride_distance / 1000 # distance in km

# Remove "bad" data
# Some entries include when bikes were taken out of docks and checked for quality 
# by Divvy where ride_length was negative or 'zero'
clean_tripdata <- trip_data[!(trip_data$ride_length <= 0),]

# Check the cleaned data frame
str(clean_tripdata)

# Check summarized details about the cleaned dataset 
summary(clean_tripdata)

## Conduct descriptive analysis
# descriptive analysis on 'ride_length'
# mean = straight average (total ride length / total rides)
# median = midpoint number of ride length array
# max = longest ride
# min = shortest ride

clean_tripdata %>% 
  summarise(average_ride_length = mean(ride_length, na.rm = TRUE),
            median_length = median(ride_length, na.rm = TRUE),
            max_ride_length = max(ride_length, na.rm = TRUE),
            min_ride_length = min(ride_length, na.rm = TRUE))

## Need to check member_casual for NA values
# Check for NA values in member_casual
sum(is.na(clean_tripdata$member_casual))

# Remove rows with NA values in member_casual
clean_tripdata <- clean_tripdata %>%
  filter(!is.na(member_casual))

# Summarize the data after removing NA values
summary_data <- clean_tripdata %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id), 
            ride_percentage = (length(ride_id) / nrow(clean_tripdata)) * 100)

print(summary_data)

# Plot the data
ggplot(clean_tripdata, aes(x = member_casual, fill = member_casual)) +
  geom_bar() +
  labs(x = "Casuals vs Members", y = "Number Of Rides", title = "Casuals vs Members distribution")

clean_tripdata %>%
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))

## See total rides and average ride time by each day for members vs casual riders
# Fix the days of the week order.
clean_tripdata$day_of_week <- ordered(clean_tripdata$day_of_week, 
                                      levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

clean_tripdata %>% 
  group_by(member_casual, day_of_week) %>%  #groups by member_casual
  summarise(number_of_rides = n() #calculates the number of rides and average duration 
            ,average_ride_length = mean(ride_length),.groups="drop") %>% # calculates the average duration
  arrange(member_casual, day_of_week) #sort

# Visualize total rides data by type and day of week
clean_tripdata %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Visualize average ride time data by type and day of week
clean_tripdata %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time by Members and Casual riders Vs. Day of the week")

## See total rides and average ride time by each month for members vs casual riders
# First lets fix the days of the week order.
clean_tripdata$month <- ordered(clean_tripdata$month, 
                                levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

clean_tripdata %>% 
  group_by(member_casual, month) %>%  
  summarise(number_of_rides = n(), average_ride_length = mean(ride_length), .groups="drop") %>% 
  arrange(member_casual, month)

# Visualize total rides data by type and month
clean_tripdata %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Month", x = "Month", y= "Number Of Rides") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Visualize average ride time data by type and month
clean_tripdata %>%  
  group_by(member_casual, month) %>% 
  summarise(average_ride_length = mean(ride_length),.groups="drop") %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride length by Members and Casual riders Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))

# Comparison between Members and Casual riders depending on ride distance
clean_tripdata %>% 
  group_by(member_casual) %>% drop_na() %>%
  summarise(average_ride_distance = mean(ride_distance)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by Members and Casual riders", x="Member and Casual riders", y="Average distance In Km")

# Bike demand by hour in a day
clean_tripdata %>%
  ggplot(aes(start_time, fill= member_casual)) +
  labs(x="Hour of the day", title="Cyclistic's Bike demand by hour in a day") +
  geom_bar()

# Bike demand per hour by day of the week
clean_tripdata %>%
  ggplot(aes(start_time, fill=member_casual)) +
  geom_bar() +
  labs(x="Hour of the day", title="Cyclistic's bike demand per hour by day of the week") +
  facet_wrap(~ day_of_week)

# Analysis and visualization of Rideable type Vs. total rides by Members and casual riders
clean_tripdata %>%
  group_by(rideable_type) %>% 
  summarise(count = length(ride_id))

ggplot(clean_tripdata, aes(x=rideable_type, fill=member_casual)) +
  labs(x="Rideable type", title="Rideable type Vs. total rides by Members and casual riders") +
  geom_bar()

# Lets check the coordinates data of the rides
# Adding a new data frame only for the most popular routes >200 rides
coordinates_df <- clean_tripdata %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>%
  summarise(total_rides = n(),.groups="drop") %>%
  filter(total_rides > 200)

# Creating two different data frames depending on rider type (member_casual)

casual_riders <- coordinates_df %>% filter(member_casual == "casual")
member_riders <- coordinates_df %>% filter(member_casual == "member")

# Setup ggmap and store map of Chicago (bbox, stamen map)
chicago <- c(left = -87.700424, bottom = 41.790769, right = -87.554855, top = 41.990119)

# Fetch the map using get_stadiamap
chicago_map <- get_stadiamap(bbox = chicago, zoom = 12, maptype = "outdoors")

# Plot the map
ggmap(chicago_map)

# Map on casual riders
ggmap(chicago_map,darken = c(0.1, "white")) +
  geom_point(casual_riders, mapping = aes(x = start_lng, y = start_lat, color=rideable_type), size = 2) +
  coord_fixed(0.8) +
  labs(title = "Most used routes by Casual riders",x=NULL,y=NULL) +
  theme(legend.position="none")

# Map on member riders
ggmap(chicago_map,darken = c(0.1, "white")) +
  geom_point(member_riders, mapping = aes(x = start_lng, y = start_lat, color=rideable_type), size = 2) +  
  coord_fixed(0.8) +
  labs(title = "Most used routes by Member riders",x=NULL,y=NULL) +
  theme(legend.position="none")


