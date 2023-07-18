library(ggplot2)
library(ggcorrplot)

### Count ###

# Summary statistics
summary(bike$count)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$count)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8, 
                 bins = 25) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "Number of Rented Bikes",
       y = "Density",
       title = "Histogram and KDE for Count") +
  theme(plot.title = element_text(hjust = 0.5)) 


### Hour ###

# Unique values of hour
unique(bike$hour)



### Temperature ###

# Summary statistics
summary(bike$temperature)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$temperature)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "Temperature (C)",
       y = "Density",
       title = "Histogram and KDE for Temperature") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Dew-point Temperature ###

# Summary statistics
summary(bike$dew_point_temperature)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$dp_temperature)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "Dew-Point Temperature (C)",
       y = "Density",
       title = "Histogram and KDE for Dew-Point Temperature") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Humidity ###

# Summary statistics
summary(bike$humidity)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$humidity)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "Humidity (%)",
       y = "Density",
       title = "Histogram and KDE for Humidity") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Rainfall ###

# Summary statistics
summary(bike$rainfall)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$rainfall)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  scale_x_continuous(limits = c(-1, 5), oob = scales::oob_keep) + 
  theme_minimal() + 
  labs(x = "Rainfall (mm)",
       y = "Density",
       title = "Histogram and KDE for Rainfall") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Snowfall ###

# Summary statistics
summary(bike$snowfall)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$snowfall)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  scale_x_continuous(limits = c(-1, 5), oob = scales::oob_keep) + 
  theme_minimal() + 
  labs(x = "Snowfall (cm)",
       y = "Density",
       title = "Histogram and KDE for Snowfall") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Wind Speed ###

# Summary statistics
summary(bike$wind_speed)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$wind_speed)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "Wind Speed (m/s)",
       y = "Density",
       title = "Histogram and KDE for Wind Speed") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Visibility ###

# Summary statistics
summary(bike$visibility)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$visibility)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "Visibility (10m)",
       y = "Density",
       title = "Histogram and KDE for Visibility") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Solar Radiation ###

# Summary statistics
summary(bike$solar_radiation)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$solar_radiation)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "Solar Radiation (Mj/m2)",
       y = "Density",
       title = "Histogram and KDE for Solar Radiation") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Holiday ###

# Unique values
levels(bike$holiday)



### Seasons ###

# Unique values
levels(bike$holiday)



### Correlation ###

corr <- cor(bike[, c("count", "hour", "temperature", "humidity", "wind_speed", 
                     "visibility", "dp_temperature", "solar_radiation", "rainfall",
                     "snowfall")])

ggcorrplot(corr, outline.col = "white", 
           type = "lower",
           colors = c("#fcd303",  "#21908CFF", "#440154FF")) +
  labs(title = "Correlation Matrix") +
  theme(plot.title = element_text(hjust = 0.5))

