# Loading libraries
library(leaps)
library(car)
library(glmnet)
library(MASS)

# Setting random seed
set.seed(42)


##### Parametric Regression #####


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

# BIC of the model
BIC(mod_all)

# R2
summary(mod_all)$r.squared

# Adjusted R2
summary(mod_all)$adj.r.squared




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
     ylab = "BIC",
     main = "Best Subset Selection with BIC")

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

# VIF
vif(mod_sub)

# BIC
BIC(mod_sub)

# R2
summary(mod_sub)$r.squared

# Adjusted R2
summary(mod_sub)$adj.r.squared



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
     label = 5,
     lwd = 2,
     main = "Ridge Regression Coefficients")


# Cross validation for shrinkage penalty
cv_ridge <- cv.glmnet(X_train, 
                      train$log_count, 
                      alpha = 0)
plot(cv_ridge,
     main = "CV - Shrinkage Penalty Ridge Regression") 

# Finding optimal lambda value that minimizes test MSE
lambda_ridge <- cv_ridge$lambda.1se
lambda_ridge

# Fitting the model
mod_ridge <- glmnet(X_train, 
                    train$log_count, 
                    alpha = 0, 
                    lambda = lambda_ridge,
                    standardize = FALSE,
                    family = "gaussian",
                    thres = 1E-12, 
                    maxit = 10^7)

coef(mod_ridge)



## Lasso regression

# Visualisation of coefficients for multiple lambdas
lambda_array <- 10 ** seq(0.1, -2, by = -0.1)

lasso <- glmnet(X_train,
                train$log_count,
                alpha = 1,
                lambda = lambda_array)

plot(lasso, 
     xvar = "lambda", 
     label = TRUE, 
     lwd = 2,
     main = "Coefficients Lasso Regression")

# Cross validation for shrinkage penalty
cv_lasso <- cv.glmnet(X_train, 
                      train$log_count, 
                      alpha = 1)
plot(cv_lasso,
     main = "CV - Shrinkage Penalty Lasso Regression") 


# Finding the optimal lambda value that minimizes test MSE
lambda_lasso <- cv_lasso$lambda.1se
lambda_lasso

# Fitting the model
mod_lasso <- glmnet(X_train, 
                    train$log_count, 
                    alpha = 1, 
                    lambda = lambda_lasso,
                    thres = 1E-12, 
                    maxit = 10^7)

coef(mod_lasso)


