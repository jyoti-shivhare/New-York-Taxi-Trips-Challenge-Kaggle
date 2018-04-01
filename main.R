# For visualisations
library('ggplot2') 
library('scales')
library('grid')
library('RColorBrewer')
library('corrplot') 
library('alluvial')

# data manipulation
library('dplyr')
library('data.table')

# input/output
library('readr')

# data wrangling
library('tibble')
library('tidyr')

# string manipulation
library('stringr')

# factor manipulation
library('forcats')

# date and time
library('lubridate')

# geospatial locations
library('geosphere')

# maps
library('leaflet')
library('leaflet.extras')
library('maps')

# modelling
library('xgboost')
library('caret')


train <- as.tibble(fread('train.csv'))
test <- as.tibble(fread('test.csv'))

summary(train)
glimpse(train)

summary(test)
glimpse(test)

sum(is.na(train))
sum(is.na(test))

combine <- bind_rows(train %>% mutate(dset = "train"), 
                     test %>% mutate(dset = "test",
                                     dropoff_datetime = NA,
                                     trip_duration = NA))
combine <- combine %>% mutate(dset = factor(dset))

train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))

train %>%
  mutate(check = abs(int_length(interval(dropoff_datetime,pickup_datetime)) + trip_duration) > 0) %>%
  select(check, pickup_datetime, dropoff_datetime, trip_duration) %>%
  group_by(check) %>%
  count()

jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

pick_coord <- train %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- train %>%
  select(dropoff_longitude, dropoff_latitude)
train$dist <- distCosine(pick_coord, drop_coord)
train$bearing = bearing(pick_coord, drop_coord)

train$jfk_dist_pick <- distCosine(pick_coord, jfk_coord)
train$jfk_dist_drop <- distCosine(drop_coord, jfk_coord)
train$lg_dist_pick <- distCosine(pick_coord, la_guardia_coord)
train$lg_dist_drop <- distCosine(drop_coord, la_guardia_coord)

train <- train %>%
  mutate(speed = dist/trip_duration*3.6,
         date = date(pickup_datetime),
         month = month(pickup_datetime, label = TRUE),
         wday = wday(pickup_datetime, label = TRUE),
         wday = fct_relevel(wday, c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")),
         hour = hour(pickup_datetime),
         work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
         jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
         lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
         blizzard = !((date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
  )





day_plus_trips <- train  %>%
  filter(trip_duration > 24*3600)

day_plus_trips %>% select(pickup_datetime, dropoff_datetime, speed)

day_trips <- train %>%
  filter(trip_duration < 24*3600 & trip_duration > 10*3600)

day_trips %>% 
  arrange(desc(dist)) %>%
  select(dist, pickup_datetime, dropoff_datetime, speed) %>%
  head(5)

min_trips <- train %>%
  filter(trip_duration < 5*60)

min_trips %>% 
  arrange(dist) %>%
  select(dist, pickup_datetime, dropoff_datetime, speed) %>%
  head(5)

zero_dist <- train %>%
  filter(near(dist,0))
nrow(zero_dist)

zero_dist %>%
  arrange(desc(trip_duration)) %>%
  select(trip_duration, pickup_datetime, dropoff_datetime, vendor_id) %>%
  head(5)

zero_dist %>%
  filter(trip_duration < 6000) %>%
  ggplot(aes(trip_duration, fill = vendor_id)) +
  geom_histogram(bins = 50) +
  scale_x_log10()

long_dist <- train %>%
  filter( (jfk_dist_pick > 3e5) | (jfk_dist_drop > 3e5) )
long_dist_coord <- long_dist %>%
  select(lon = pickup_longitude, lat = pickup_latitude)

long_dist %>%
  select(id, jfk_dist_pick, jfk_dist_drop, dist, trip_duration, speed) %>%
  arrange(desc(jfk_dist_pick))

leaflet(long_dist_coord) %>%
  addTiles() %>%
  setView(-92.00, 41.0, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(popup = ~as.character(long_dist$dist), label = ~as.character(long_dist$id))

train <- train %>%
  filter(trip_duration < 22*3600,
         dist > 0 | (near(dist, 0) & trip_duration < 60),
         jfk_dist_pick < 3e5 & jfk_dist_drop < 3e5,
         trip_duration > 10,
         speed < 100)

# Adding weather reports
weather <- as.tibble(fread("weather_data_nyc_centralpark_2016.csv"))

glimpse(weather)

weather <- weather %>%
  mutate(date = dmy(date),
         rain = as.numeric(ifelse(precipitation == "T", "0.01", precipitation)),
         s_fall = as.numeric(ifelse(`snow fall` == "T", "0.01", `snow fall`)),
         s_depth = as.numeric(ifelse(`snow depth` == "T", "0.01", `snow depth`)),
         all_precip = s_fall + rain,
         has_snow = (s_fall > 0) | (s_depth > 0),
         has_rain = rain > 0,
         max_temp = `maximum temperature`,
         min_temp = `minimum temperature`)

foo <- weather %>%
  select(date, rain, s_fall, all_precip, has_snow, has_rain, s_depth, max_temp, min_temp)

train <- left_join(train, foo, by = "date")