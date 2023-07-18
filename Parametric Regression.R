library(leaps)
library(car)
library(glmnet)
library(MASS)


### Train-Test-Split

sample <- sample(c(TRUE, FALSE), 
                 nrow(bike), 
                 replace = TRUE, 
                 prob = c(0.7,0.3))
train  <- bike[sample, ]
test   <- bike[!sample, ]


### Matrices for Regulisation Methods
X_train <- scale(as.matrix(train[,c("temperature", "dp_temperature", "humidity", 
                                    "log_wind_speed", "solar_radiation", 
                                    "sin_hour", "cos_hour", "sin_dow", 
                                    "cos_dow", "log_rainfall", "log_snowfall", 
                                    "sqrt_visibility")]))

X_test <- scale(as.matrix(test[,c("temperature", "dp_temperature", "humidity", 
                                  "log_wind_speed", "solar_radiation", 
                                  "sin_hour", "cos_hour", "sin_dow", "cos_dow", 
                                  "log_rainfall", "log_snowfall", 
                                  "sqrt_visibility")]))



### Linear Regression ###

## Linear Regression using the first 2 Principal Components

# Training the model
mod_pc <- lm(log_count ~ pc1 + pc2,
             data = train)

# Summary of the model
summary(mod_pc)

# BIC of the model
BIC(mod_pc)

# R2
summary(mod_pc)$r.squared

# Adjusted R2
summary(mod_pc)$adj.r.squared




## Linear Regression using all variables

# Training the model
mod_all <- lm(log_count ~ temperature + dp_temperature + 
                humidity + log_wind_speed + solar_radiation + sin_hour + 
                cos_hour + sin_dow + cos_dow + log_rainfall + log_snowfall + 
                sqrt_visibility + spring + summer + winter + holiday, 
              data = train)

# Summary of the model
summary(mod_all)

# VIF
vif(mod_all)

# Outlier visualisations
plot(mod_all, pch = 16, cex = 0.5)

# BIC of the model
BIC(mod_all)

# R2
summary(mod_all)$r.squared

# Adjusted R2
summary(mod_all)$adj.r.squared



### Robust linear regression

mod_robust <- rlm(log_count ~ temperature + dp_temperature + 
                    humidity + log_wind_speed + solar_radiation + sin_hour + 
                    cos_hour + sin_dow + cos_dow + log_rainfall + log_snowfall + 
                    sqrt_visibility + spring + summer + winter + holiday, 
                  data = train, 
                  psi = psi.hampel, 
                  init = "lts")

BIC(mod_robust)



## Best subset selection

# Training the models
mod_best <- regsubsets(log_count ~ temperature + dp_temperature + 
                         humidity + wind_speed + solar_radiation + sin_hour + 
                         cos_hour + sin_dow + cos_dow + log_rainfall + 
                         log_snowfall + sqrt_visibility + winter + spring +  
                         summer + holiday, 
                       data = train, method = "exhaustive", nvmax = 18)

# Plotting with BIC
plot(mod_best, 
     scale = "bic",
     main = "Best Subset Selection",
     ylab = "BIC")

# Best models
data.frame(
  R2 = which.max(summary(mod_best)$rsq),
  AdjR2 = which.max(summary(mod_best)$adjr2),
  BIC = which.min(summary(mod_best)$bic)
)

# Variables of best model with BIC
summary(mod_best)$which[13,]

# Train the model
mod_sub <- lm(log_count ~ dp_temperature + 
                humidity + log_wind_speed + solar_radiation + sin_hour + 
                cos_hour + sin_dow + cos_dow + log_rainfall + spring + summer + 
                winter + holiday, 
              data = train)

# Summary of the model
summary(mod_sub)



### Regulisation methods

## Ridge regression

# Visualisation of coefficients for multiple lambdas
lambda_array <- 10 ** seq(2, -2, by = -0.1)

ridge <- glmnet(X_train,
                train$log_count, 
                alpha = 0,
                lambda = lambda_array)

plot(ridge, 
     xvar = "lambda",
     label = TRUE,
     main = "Ridge Regression Coefficients")

# Cross validation for lambda
cv_ridge <- cv.glmnet(X_train, 
                      train$log_count, 
                      alpha = 0)
plot(cv_ridge) 

#find optimal lambda value that minimizes test MSE
lambda_ridge <- cv_ridge$lambda.min
lambda_ridge

mod_ridge <- glmnet(X_train, 
                    train$log_count, 
                    alpha = 0, 
                    lambda = lambda_ridge)
coef(mod_ridge)


## Lasso regression

lambda_array <- 10 ** seq(0.1, -2, by = -0.1)

lasso <- glmnet(X_train,
                train$log_count,
                alpha = 1,
                lambda = lambda_array)

plot(lasso, 
     xvar = "lambda", 
     label = TRUE,
     main = "Lasso Regression Coefficients")

cv_lasso <- cv.glmnet(X_train, 
                      train$log_count, 
                      alpha = 1)
plot(cv_lasso) 


#find optimal lambda value that minimizes test MSE
lambda_lasso <- cv_lasso$lambda.min
lambda_lasso

mod_lasso <- glmnet(X_train, 
                    train$log_count, 
                    alpha = 1, 
                    lambda = lambda_lasso)
coef(mod_lasso)


### Comparison with lm with only numeric

mod_num <- lm(train$log_count ~ X_train)

mod_num$coefficients

coef(mod_ridge)

coef(mod_lasso)

