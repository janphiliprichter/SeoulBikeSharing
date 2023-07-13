library(ggplot2)
library(factoextra)
library(scales)
set.seed(42)

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
             legend.title = "Contribution (%)",
             col.var = "contrib",
             gradient.cols = viridis_pal()(30),
             repel = TRUE) + 
  labs(x = paste("PC1 (", round(var_exp[1], 2), "%)", sep = ""),
       y = paste("PC2 (", round(var_exp[2], 2), "%)", sep = ""),
       title = "PCA Variables") +
  theme(plot.title = element_text(hjust = 0.5))

vars

# Elbow plot for k-means
fviz_nbclust(bike[,c("pc1", "pc2")], kmeans, 
             method = "wss",
             linecolor = "#00868f") + 
  labs(x = "Number of Clusters",
       y = "Total Within Sum of Squares",
       title = "Optimal number of clusters for K-Means") +
  geom_vline(xintercept = 3, linetype = 2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# k-means algorithm
km <- kmeans(x = bike[,c("pc1", "pc2")], 
             centers = 3, 
             nstart = 1000)

# Adding cluster variable to the dataset
bike$cluster <- as.factor(km$cluster)

# Scatterplot of the clusters
ggplot(data = bike, 
       mapping = aes(x = pc1,
                     y = pc2,
                     color = cluster)) +
  geom_point(size = 0.8, alpha = 0.8) +
  scale_colour_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  theme_minimal() +
  labs(x = paste("PC1 (", round(var_exp[1], 2), "%)", sep = ""),
       y = paste("PC2 (", round(var_exp[2], 2), "%)", sep = ""),
       title = "3-Means-Clustering") +
  theme(plot.title = element_text(hjust = 0.5))


# Relevelling clusters for histogram 
bike$cluster <- relevel(bike$cluster, ref = 3)
#bike$cluster <- relevel(bike$cluster, ref = 1)
#bike$cluster <- relevel(bike$cluster, ref = 2)
levels(bike$cluster)



# Histogram for the different clusters
ggplot(data = bike,
       mapping = aes(x = count, 
                     fill = cluster,
                     colour = cluster)) +
  geom_histogram(mapping = aes(y = after_stat(density)),
                 position="identity",
                 bins = 20,
                 alpha = 0.5) +
  geom_density(alpha = 0,
               linewidth = 0.8) +
  scale_fill_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  scale_colour_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  theme_minimal() +
  labs(x = "Number of rented Bikes",
       y = "Density",
       title = "Histogram and KDE for each Cluster") +
  theme(plot.title = element_text(hjust = 0.5))
  


