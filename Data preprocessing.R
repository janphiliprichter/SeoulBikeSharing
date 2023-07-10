library(tidyverse)

# Loading the data set
bike <- read.csv("/Users/philip/Documents/Milano University/2. Semester/Statistical Learning/Statistical Learning Project/SeoulBikeSharing/SeoulBikeData.csv",
                 check.names = FALSE)

# Removing troublesome characters
iconv(names(bike), to = "ASCII", sub = "")

# Column names
colnames(bike)

# Renaming column names
bike <- bike %>% 
  rename("date" = "Date",
         "count" = "Rented Bike Count",
         "hour" = "Hour",
         "temperature" = "Temperature(\xb0C)",
         "humidity" = "Humidity(%)",
         "wind_speed" = "Wind speed (m/s)",
         "visibility" = "Visibility (10m)",
         "dew_point_temperature" = "Dew point temperature(\xb0C)",
         "solar_radiation" = "Solar Radiation (MJ/m2)",
         "rainfall" = "Rainfall(mm)",
         "snowfall" = "Snowfall (cm)",
         "seasons" = "Seasons",
         "holiday" = "Holiday",
         "functioning_day" = "Functioning Day"
         )

# Checking the data classes
glimpse(bike)

# Changing data classes, where necessary
bike$date <- as.Date(bike$date, format = "%d/%m/%Y")
bike$holiday <- as.factor(bike$holiday)
bike$functioning_day <- as.factor(bike$functioning_day)
bike$holiday <- as.factor(bike$holiday)
bike$seasons <- as.factor(bike$seasons)

# Checking for missing values
sum(is.na(bike))

# Checking for duplicates
sum(duplicated(bike))

# Remove non-functioning days
bike <- bike[bike$functioning_day != "No", ]

## Checking for implausible values

# Count
max(bike$count) 
min(bike$count)
hist(bike$count)

# Temperature
max(bike$temperature) 
min(bike$temperature)
hist(bike$temperature)

# Humidity
max(bike$humidity) 
min(bike$humidity)

# Wind speed
max(bike$wind_speed) 
min(bike$wind_speed)
hist(bike$wind_speed)

# Visibility
max(bike$visibility) 
min(bike$visibility)
hist(bike$visibility)

# Dew point temperature
max(bike$dew_point_temperature) 
min(bike$dew_point_temperature)
hist(bike$dew_point_temperature)

# Solar radiation
max(bike$solar_radiation) 
min(bike$solar_radiation)
hist(bike$solar_radiation)

# Rainfall
max(bike$rainfall) 
min(bike$rainfall)
hist(bike$rainfall)

# Snowfall
max(bike$snowfall) 
min(bike$snowfall)
hist(bike$snowfall)

# Seasons
levels(bike$seasons)

# Holiday
levels(bike$holiday)
