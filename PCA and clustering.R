library(ggplot2)
library(factoextra)
library(scales)
set.seed(42)

### Principal Component Analysis ###

# Using only the numeric variables used for the PCS
bike_num <- bike[,c("temperature", "dp_temperature", "humidity", 
            "sin_hour", "cos_hour", "sin_dow", "cos_dow", "log_rainfall",
            "log_snowfall", "log_wind_speed", "sqrt_visibility")]

# Singular value decomposition
svd <- prcomp(bike_num, 
              center = TRUE, 
              scale = TRUE)

# SVD Summary
summary(svd)


## Variance Analysis

# Variance of the principal components
pc_var <- svd$sdev ** 2

# Percentage of variance explained by the principal components
var_exp <- pc_var / sum(pc_var) * 100

# PC Scree Plot 
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
       y = "Variance Explained (%)",
       title = "PCA Variance Explained") +
  theme(plot.title = element_text(hjust = 0.5)) 


## Save the first 2 PC's for further analysis
bike$pc1 <- svd$x[,"PC1"]
bike$pc2 <- svd$x[,"PC2"]

# Contribution of each variable
fviz_pca_var(svd,
             legend.title = "Contribution (%)",
             col.var = "contrib",
             gradient.cols = viridis_pal()(30),
             repel = TRUE) + 
  labs(x = paste("PC1 (", round(var_exp[1], 2), "%)", sep = ""),
       y = paste("PC2 (", round(var_exp[2], 2), "%)", sep = ""),
       title = "PCA Variables") +
  theme(plot.title = element_text(hjust = 0.5))



### Clustering Analysis ###

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


# Adding cluster variable to the data set
bike$cluster <- as.factor(km$cluster)

# Relevelling to plot the histograms in the desired order
levels(bike$cluster) <- c("C2", "C3", "C1")
bike$cluster <- relevel(bike$cluster, ref = 3)
levels(bike$cluster)


# Scatter-plot of the clusters
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
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 4)))


# Histogram for the different clusters
ggplot(data = bike,
       mapping = aes(x = count, 
                     fill = cluster,
                     colour = cluster)) +
  geom_histogram(mapping = aes(y = after_stat(density)),
                 position="identity",
                 bins = 20,
                 alpha = 0.5) +
  geom_density(kernel = "gaussian", 
               bw = "nrd0", 
               alpha = 0,
               linewidth = 0.8) +
  scale_fill_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  scale_colour_manual(values = c("#440154FF", "#21908CFF", "#d1af06")) +
  theme_minimal() +
  labs(x = "Number of rented Bikes",
       y = "Density",
       title = "Histogram and KDE for each Cluster") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.title = element_blank())




