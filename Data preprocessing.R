library(tidyverse)

# Loading the data set
bike <- read.csv(paste0("/Users/philip/Documents/Milano University/",
                        "2. Semester/Statistical Learning/",
                        "Statistical Learning Project/",
                        "SeoulBikeSharing/SeoulBikeData.csv"),
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
         "dp_temperature" = "Dew point temperature(\xb0C)",
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

# Checking for infinite values
apply(bike, 2, function(x) any(is.infinite(x)))

# Checking for duplicates
sum(duplicated(bike))

# Remove non-functioning days
bike <- bike[bike$functioning_day != "No",]

