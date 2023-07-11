library(ggplot2)
library(factoextra)
library(scales)

### PCA and Clustering ###

# Setting the variables used for the PCS
X <- bike[c("hour", "temperature", "humidity", "wind_speed", "visibility",
            "dew_point_temperature", "solar_radiation", "rainfall", "snowfall")]

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

## PCA Plot
pc1 <- pca$x[,"PC1"]
pc2 <- pca$x[,"PC2"]

plot(pc1, pc2, pch = 16, cex = 0.2)

# Graph of variables
vars <- 
  fviz_pca_var(pca,
             legend.title = "Contribution",
             col.var = "contrib",
             gradient.cols = viridis_pal()(30),
             repel = TRUE) + 
  labs(x = "PC1",
       y = "PC2",
       title = "PCA Variables") +
  theme(plot.title = element_text(hjust = 0.5))

vars



