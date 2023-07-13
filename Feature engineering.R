library(ggplot2)

### Log-transformation Count ###

bike$log_count <- log(bike$count)

# New distribution
ggplot(bike, mapping = aes(x = bike$log_count)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8, 
                 bins = 25) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "Number of Rented Bikes",
       y = "Density",
       title = "Histogramm and KDE for Count") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Sine-Cosine-Encoding of Hour ###

bike$sin_hour = sin(2 * pi * bike$hour / 24)
bike$cos_hour = cos(2 * pi * bike$hour / 24)

# Visualisation
plot(bike$sin_hour, bike$cos_hour, 
     xlab = "sine value", 
     ylab = "cosine value", 
     pch = 16)



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
  geom_density(kernel = "gaussian", bw = "nrd0") +
  scale_x_continuous(limits = c(-1, 5), oob = scales::oob_keep) + 
  theme_minimal() + 
  labs(x = "log(Rainfall)",
       y = "Density",
       title = "Histogram and KDE for log(Rainfall)") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Log-transformation Snowfall ###

bike$log_snow <- log(bike$snowfall + 1)

# New distribution
ggplot(bike, mapping = aes(x = bike$log_snow)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  scale_x_continuous(limits = c(-1, 5), oob = scales::oob_keep) + 
  theme_minimal() + 
  labs(x = "log(Snowfall)",
       y = "Density",
       title = "Histogram and KDE for log(Snowfall)") +
  theme(plot.title = element_text(hjust = 0.5)) 




### Log-transformation Wind Speed ###

bike$log_wind_speed <- log(bike$wind_speed + 1)

# New distribution
ggplot(bike, mapping = aes(x = bike$log_wind_speed)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "log(Wind Speed)",
       y = "Density",
       title = "Histogram and KDE for log(Wind Speed)") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Square-root-transformation Visibility ###

bike$sqrt_visibility <- sqrt(bike$visibility)

# New distribution
ggplot(bike, mapping = aes(x = bike$sqrt_visibility)) + 
  geom_histogram(mapping = aes(y = after_stat(density)), 
                 fill = "#00AFBB", 
                 color = "#00868f", 
                 alpha = 0.8,
                 bins = 20) +
  geom_density(kernel = "gaussian", bw = "nrd0") +
  theme_minimal() + 
  labs(x = "sqrt(Visibility)",
       y = "Density",
       title = "Histogram and KDE for sqrt(Visibility)") +
  theme(plot.title = element_text(hjust = 0.5)) 



### One-hot-Encoding Holiday ###

bike$holiday <- ifelse(bike$holiday == "Holiday", 1, 0)



# One-hot-Encoding seasons
bike$winter <- ifelse(bike$seasons == "Winter", 1, 0)
bike$spring <- ifelse(bike$seasons == "Spring", 1, 0)
bike$summer <- ifelse(bike$seasons == "Summer", 1, 0)
bike$autumn <- ifelse(bike$seasons == "Autumn", 1, 0)


