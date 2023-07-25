library(MLmetrics)
### Predictions

# PCA
pred_pc <- predict(mod_pc, newdata = test)
mse_pc <- MSE(pred_pc, test$log_count)
mae_pc <- MAE(pred_pc, test$log_count)

# All vairables
pred_all <- predict(mod_all, newdata = test)
mse_all <- MSE(pred_all, test$log_count)
mae_all <- MAE(pred_all, test$log_count)


# Subset model
pred_sub <- predict(mod_sub, newdata = test)
mse_sub <- MSE(pred_sub, test$log_count)
mae_sub <- MAE(pred_sub, test$log_count)


# Ridge
pred_ridge <- predict(mod_ridge, newx = X_test)
mse_ridge <- MSE(pred_ridge, test$log_count)
mae_ridge <- MAE(pred_ridge, test$log_count)


# Lasso
pred_lasso <- predict(mod_lasso, newx = X_test)
mse_lasso <- MSE(pred_lasso, test$log_count)
mae_lasso <- MAE(pred_lasso, test$log_count)


# GAM
pred_gam <- predict(mod_gam, newdata = test)
mse_gam <- MSE(pred_gam, test$log_count)
mae_gam <- MAE(pred_gam, test$log_count)


# MSEs

evals <- data.frame(method = c("PC", "All Variables", "Best Subset", 
                               "Ridge", "Lasso", "GAM", 
                               "PC", "All Variables", "Best Subset", 
                               "Ridge", "Lasso", "GAM"),
                   eval = c(mse_pc, mse_all, mse_sub, 
                                     mse_ridge, mse_lasso, mse_gam,
                           mae_pc, mae_all, mae_sub, 
                           mae_ridge, mae_lasso, mae_gam),
                   metric = c(rep("MSE", 6),
                              rep("MAE", 6)))

ggplot(data = evals,
       mapping = aes(x = factor(method, 
                                levels = c("PC", "All Variables", "Robust", 
                                           "Best Subset", "Ridge", "Lasso", 
                                           "GAM")),
                     y = eval,
                     fill = metric)) + 
  geom_bar(stat = "identity",
           position = position_dodge(),
           alpha = 0.9) +
  scale_fill_manual(values = c("#023740", "#0e9799")) +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = guide_legend(title = "Metric")) + 
  theme(axis.text.x = element_text(angle = 315))

