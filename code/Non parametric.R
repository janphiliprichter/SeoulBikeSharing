library(npreg)
library(mgcv)
library(gplm)
set.seed(42)


### GAM

mod_gam <- gam(log_count ~ s(temperature) + s(dp_temperature) + 
      s(humidity) + s(log_wind_speed) + s(sin_hour) + s(cos_hour) + 
        s(log_rainfall) + s(solar_radiation) + s(sqrt_visibility) + 
        sin_dow + cos_dow + spring + summer + winter, 
      data = train)

plot(mod_gam)

BIC(mod_gam)

pred_gam <- predict(mod_gam, newdata = test)
MSE(pred_gam, test$log_count)

