# Loading libraries
library(ggplot2)

# Setting random seed
set.seed(42)


##### Feature Engineering #####


### Log-transformation Count ###

bike$log_count <- log(bike$count)

# New distribution
ggplot(bike, mapping = aes(x = bike$log_count)) + 
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
       title = "Histogramm and KDE for log(Count)") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = log_count)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for log(Count)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# New Skewness 
skewness(bike$log_count)


### Sine-Cosine-Encoding of Hour ###

bike$sin_hour = sin(2 * pi * bike$hour / 24)
bike$cos_hour = cos(2 * pi * bike$hour / 24)

# Visualisation
plot(bike$sin_hour, bike$cos_hour, 
     xlab = "sine value", 
     ylab = "cosine value", 
     pch = 16,
     main = "Cyclical Encoding for Hour")



### Day of the week ###

bike$week_day <- wday(bike$date)

bike$sin_dow = sin(2 * pi * bike$week_day / 7)
bike$cos_dow = cos(2 * pi * bike$week_day / 7)



### Log-transformation Rainfall ###

bike$log_rainfall <- log(bike$rainfall + 1)

# New distribution
ggplot(bike, mapping = aes(x = bike$log_rainfall)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  scale_x_continuous(limits = c(-1, 5), oob = scales::oob_keep) + 
  theme_minimal() + 
  labs(x = "log(Rainfall)",
       y = "Density",
       title = "Histogram and KDE for log(Rainfall)") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = log_rainfall)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for log(Rainfall)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# New Skewness 
skewness(bike$log_count)



### Log-transformation Snowfall ###

bike$log_snowfall <- log(bike$snowfall + 1)

# New distribution
ggplot(bike, mapping = aes(x = bike$log_snowfall)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  scale_x_continuous(limits = c(-1, 5), oob = scales::oob_keep) + 
  theme_minimal() + 
  labs(x = "log(Snowfall)",
       y = "Density",
       title = "Histogram and KDE for log(Snowfall)") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = log_snowfall)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for log(Snowfall)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# New Skewness 
skewness(bike$log_snowfall)



### Log-transformation Wind Speed ###

bike$log_wind_speed <- log(bike$wind_speed + 1)

# New distribution
ggplot(bike, mapping = aes(x = log_wind_speed)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 12) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  theme_minimal() + 
  labs(x = "log(Wind Speed + 1)",
       y = "Density",
       title = "Histogram and KDE for log(Wind Speed)") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = log_wind_speed)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for log(Wind Speed)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# New Skewness 
skewness(bike$wind_speed)


### Square-root-transformation Visibility ###

bike$sqrt_visibility <- sqrt(bike$visibility)

# New distribution
ggplot(bike, mapping = aes(x = bike$sqrt_visibility)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               color = "#01393d") +
  theme_minimal() + 
  labs(x = "sqrt(Visibility)",
       y = "Density",
       title = "Histogram and KDE for sqrt(Visibility)") +
  theme(plot.title = element_text(hjust = 0.5)) 

# QQ-Plot
ggplot(data = bike, mapping = aes(sample = sqrt_visibility)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles",
       title = "QQ-Plot for sqrt(Visibility)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# New Skewness 
skewness(bike$sqrt_visibility)



### One-hot-Encoding Holiday ###

bike$holiday <- ifelse(bike$holiday == "Holiday", 1, 0)



# One-hot-Encoding seasons
bike$winter <- ifelse(bike$seasons == "Winter", 1, 0)
bike$spring <- ifelse(bike$seasons == "Spring", 1, 0)
bike$summer <- ifelse(bike$seasons == "Summer", 1, 0)
bike$autumn <- ifelse(bike$seasons == "Autumn", 1, 0)


