attach(bike)



mod <- lm(log(count) ~ temperature + I(temperature**2) + I(temperature**3) +
            humidity + log_rainfall + sqrt(visibility) +
            winter + summer + spring + holiday + wind_speed + sin_hour + cos_hour + 
            sin_dow + cos_dow + sin_dom + cos_dom)

summary(mod)

mod2 <- lm(log(count) ~ pc1 + pc2)
summary(mod2)

library("car")
library(moments)
attach(bike)

skewness(visibility ** 2)
skewness(sqrt_visibility)


