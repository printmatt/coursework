library(tidyverse)
library(lubridate)

########################################
#     READ AND TRANSFORM THE DATA      #
########################################

# read one month of data
trips <- read_csv('201402-citibike-tripdata.csv')

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender = factor(gender, levels=c(0,1,2), labels = c("Unknown","Male","Female")))


########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)
nrow(trips)

# find the earliest and latest birth years (see help for max and min to deal with NAs)
trips$birth_year <- gsub("\\\\N",NA,trips$birth_year)
max(trips$birth_year, na.rm = TRUE)
min(trips$birth_year, na.rm = TRUE)


# use filter and grepl to find all trips that either start or end on broadway
broadway_either <- trips %>% filter(grepl('Broadway',start_station_name) | grepl('Broadway',end_station_name))

# do the same, but find all trips that both start and end on broadway
broadway_and <- trips %>% filter(grepl('Broadway',start_station_name)) %>% filter(grepl('Broadway',end_station_name))

# find all unique station names
unique_stations <- summarize(group_by(trips,start_station_name))

# count the number of trips by gender, the average trip time by gender, and the standard deviation in trip time by gender
# do this all at once, by using summarize() with multiple arguments
gender_summarize <- summarize(group_by(trips,gender),
                              count = n(),
                              average_trip_time = mean(trips$tripduration),
                              sd_trip_time = sd(trips$tripduration))

# find the 10 most frequent station-to-station trips
start_end_stations <- summarize(group_by(trips,start_station_name,end_station_name),
                                count = n()) %>% arrange(desc(count)) %>% head(10)

# find the top 3 end stations for trips starting from each start station
top_3 <- summarize(group_by(trips,start_station_name,end_station_name), count=n()) %>% arrange(start_station_name,desc(count)) %>% slice_head(n=3)

# find the top 3 most common station-to-station trips by gender
top_3_gender <- trips %>% group_by(gender,start_station_name,end_station_name) %>% summarize(count = n()) %>% group_by(gender) %>% arrange(desc(count)) %>% slice_head(n=3)

# find the day with the most trips
# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)

# compute the average number of trips taken during each of the 24 hours of the day across the entire month
# what time(s) of day tend to be peak hour(s)?