# Loading libraries
library(ggplot2)
library(ggcorrplot)
library(patchwork)
library(moments)


##### Exploratory Data Analysis #####


### Count ###

# Summary statistics
summary(bike$count)

# Skewness
skewness(bike$count)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$count)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8, 
                 bins = 25) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  theme_minimal() + 
  labs(x = "Number of Rented Bikes",
       y = "Density",
       title = "Histogram and KDE for Count") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = count)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 



### Hour ###

# Unique values of hour
unique(bike$hour)



### Temperature ###

# Summary statistics
summary(bike$temperature)

# Skewness
skewness(bike$temperature)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$temperature)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  theme_minimal() + 
  labs(x = "Temperature (C)",
       y = "Density",
       title = "Histogram and KDE for Temperature") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = temperature)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Temperature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 



### Dew-point Temperature ###

# Summary statistics
summary(bike$dp_temperature)

# Skewness
skewness(bike$dp_temperature)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$dp_temperature)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  theme_minimal() + 
  labs(x = "Dew-Point Temperature (C)",
       y = "Density",
       title = "Histogram and KDE for Dew-Point Temperature") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = dp_temperature)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Dew-Point Temperature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 



### Humidity ###

# Summary statistics
summary(bike$humidity)

# Skewness
skewness(bike$humidity)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = bike$humidity)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 25) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  theme_minimal() + 
  labs(x = "Humidity (%)",
       y = "Density",
       title = "Histogram and KDE for Humidity") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = humidity)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Humidity") +
  theme_minimal() +
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
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  scale_x_continuous(limits = c(-1, 5), 
                     oob = scales::oob_keep) + 
  theme_minimal() + 
  labs(x = "Rainfall (mm)",
       y = "Density",
       title = "Histogram and KDE for Rainfall") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = rainfall)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Rainfall") +
  theme_minimal() +
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
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  scale_x_continuous(limits = c(-1, 5), 
                     oob = scales::oob_keep) + 
  theme_minimal() + 
  labs(x = "Snowfall (cm)",
       y = "Density",
       title = "Histogram and KDE for Snowfall") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = snowfall)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Snowfall") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 



### Wind Speed ###

# Summary statistics
summary(bike$wind_speed)

# Skewness
skewness(bike$wind_speed)

# Histogram and Kernel Density Estimation
ggplot(bike, mapping = aes(x = wind_speed)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  theme_minimal() + 
  labs(x = "Wind Speed (m/s)",
       y = "Density",
       title = "Histogram and KDE for Wind Speed") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = wind_speed)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Wind Speed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 



### Visibility ###

# Summary statistics
summary(bike$visibility)

# Skewness
skewness(bike$visibility)

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

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = wind_speed)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Visibility") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 



### Solar Radiation ###

# Summary statistics
summary(bike$solar_radiation)

# Skewness
skewness(bike$solar_radiation)

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

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = wind_speed)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for Solar Radiation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


### Holiday ###

# Unique values
levels(bike$holiday)



### Seasons ###

# Unique values
levels(bike$holiday)



### Correlation ###

corr <- cor(bike[, c("count", "hour", "temperature", "humidity", "wind_speed", 
                     "visibility", "dp_temperature", "solar_radiation", 
                     "rainfall", "snowfall")])

ggcorrplot(corr, outline.col = "white", 
           type = "lower",
           colors = c("#fcd303",  "#21908CFF", "#440154FF")) +
  labs(title = "Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))


