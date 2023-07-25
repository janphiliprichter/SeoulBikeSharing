# Loading libraries
library(npreg)
library(mgcv)
library(gplm)

# Setting random seed
set.seed(42)


##### Non-parametric Regression #####

### GAM

# Fitting the model
mod_gam <- gam(log_count ~ s(temperature) + s(dp_temperature) + 
      s(humidity) + s(log_wind_speed) + sin_hour + cos_hour + 
        s(log_rainfall) + s(solar_radiation) + s(sqrt_visibility) + 
        sin_dow + cos_dow + spring + summer + winter, 
      data = train)

# Summary output
summary(mod_gam)

# Plot of the smoothing effects
plot(mod_gam)

# BIC
BIC(mod_gam)


