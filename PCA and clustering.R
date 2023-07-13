library(ggplot2)
library(factoextra)
library(scales)

### PCA and Clustering ###

# Setting the variables used for the PCS
X <- bike[c("temperature", "dew_point_temperature", "humidity", 
            "sin_hour", "cos_hour", "sin_dow", "cos_dow", "log_rainfall",
            "log_snowfall", "log_wind_speed", "sqrt_visibility")]

# Performing the PCA on the data matrix
pca <- prcomp(X, center = TRUE, scale. = TRUE)

# Summary
summary(pca)

## Variance

# Variance of the principal components
pca_var <- pca$sdev ** 2

# Percentage of variance explained by the principal components
var_exp <- pca_var / sum(pca_var) * 100
var_exp

# PCA Scree Plot 
scree <- 
  ggplot(data.frame(var_exp)) + 
  geom_bar(mapping = aes(x = 1:length(var_exp),
                         y = var_exp),
           fill = "#00AFBB",
           stat = "identity") +
  geom_line(mapping = aes(x = 1:length(var_exp), 
                          y = cumsum(var_exp)),
            stat = "identity") +
  geom_point(mapping = aes(x = 1:length(var_exp), 
                           y = cumsum(var_exp)),
             stat = "identity") +
  scale_x_continuous(n.breaks=10) +
  ylim(0,101) +
  theme_minimal() +
  labs(x = "Principal Components",
       y = "% Variance Explained",
       title = "PCA Variance Explained") +
  theme(plot.title = element_text(hjust = 0.5)) 

scree

## Save the first 2 PC's for further analysis
bike$pc1 <- pca$x[,"PC1"]
bike$pc2 <- pca$x[,"PC2"]

# Graph of variables
vars <- 
  fviz_pca_var(pca,
             legend.title = "Contribution",
             col.var = "contrib",
             gradient.cols = viridis_pal()(30),
             repel = TRUE) + 
  labs(x = paste("PC1 ", round(var_exp[1], 2), "%", sep = ""),
       y = paste("PC2 ", round(var_exp[2], 2), "%", sep = ""),
       title = "PCA Variables") +
  theme(plot.title = element_text(hjust = 0.5))

vars




pcs <- bike[,c("pc1", "pc2")]

fviz_nbclust(pcs, kmeans, 
             method = "wss",
             barfill = "#00AFBB",
             k.max = 8)

km <- kmeans(X, centers = 3, nstart = 25)

fviz_cluster(km, data = X)





head(X)

X_scaled <- scale(X)
head(X_scaled)

km1 <- kmeans(X, centers = 3, nstart = 25)
fviz_cluster(km1, data = X, geom = "point", pointsize = 0.5, ellipse = FALSE)

km2 <- kmeans(X_scaled, centers = 3, nstart = 25)
fviz_cluster(km2, data = X_scaled, geom = "point", pointsize = 0.5, ellipse = FALSE)

km3 <- kmeans(pcs, centers = 3, nstart = 25)
fviz_cluster(km3, data = pcs, geom = "point", pointsize = 0.5, ellipse = FALSE) 

pcs2 <- bike[c("pc1", "pc2", "pc3")]
km4 <- kmeans(pcs2, centers = 3, nstart = 25)
fviz_cluster(km4, data = pcs2, geom = "point", pointsize = 0.5) 


