library('writexl')
library('umap')
library('ggplot2')
library('openxlsx')
library('plotly') 
library('ggfortify')
library("dplyr")
library("Rtsne")
  
setwd("~/Desktop/ScilifeLab_Work/Phd/Project 1_Comparing CP and NN features for MoA prediction//UMAPS_DMSO/Unnormalized data/")

norm_cp_all <- read.xlsx("unnorm_efficientnetb0_all_filteredbbox.xlsx")
#norm_nn_all <- read.xlsx("unnorm_cp_all.xlsx") 

#add a column called CLUSTER with new labels 
norm_cp_all$Cluster <- norm_cp_all$Category
df <- norm_cp_all 
df <- df[, c(1:5, grep("Mean_", names(df)), (ncol(df)-6):ncol(df))]

#replace values in cluster column with DMSO and BzCl 
df$Cluster[df$Compound == "DMSO"] <- "DMSO"
df$Cluster[df$Compound == "BzCl"] <- "BzCl"
df$Cluster[df$Compound == "cells"] <- "cells"
df <- df[df$Cluster != "Miscl.", ]
df <- df[df$Cluster != "Miscl. ", ]

df_umap <- df

df_umap <- df_umap[df_umap$Compound %in% c('DMSO'), ]
df_umap[is.na(df_umap) | df_umap == " " | df_umap == "NA" | df_umap == "-inf" | df_umap == "inf"] <- 0
df_umap <- na.omit(df_umap)
df_umap <- df_umap[, colSums(is.null(df_umap) | is.na(df_umap) | df_umap == "") == 0]

# Get column names ending with "_X" or "_Y" this is only applicable for CP features
columns_to_drop <- grep("_X$|_Y$", names(df_umap), value = TRUE)
# Remove the columns
df_umap <- df_umap[, !(names(df_umap) %in% columns_to_drop)]
df_umapp <- df_umap
df_umapp[, 6:1285] <- lapply(df_umapp[, 6:1285], as.numeric) #6 TO 229 FOR CP and 7:518 for nn
data.umap <- umap(df_umapp[6:1285],  learning_rate = 0.001, metric = "cosine", min_dist = 0.99)  

#Plot UMAP
set.seed(123)
combination_colors <- c("orange", "darkgreen", "blue","deeppink", "red" )
umap_plot <- ggplot(df_umap, aes(x = data.umap$layout[, 1], y = data.umap$layout[, 2], colour = as.factor(Combination))) +
  geom_point(size = 3) + # Adjust the alpha parameter for transparency
  xlim(c(-15, 20)) +
  ylim(c(-15, 15)) +
  labs(x = 'UMAP 1', y = 'UMAP 2') +  # Add axis labels here
  theme_minimal() +
  theme(axis.text = element_text(size = 32, color = "black"),
        axis.title = element_blank(), legend.position = "bottom", legend.title = element_text(size = 16),  legend.text = element_text(size = 14)) + 
  scale_color_manual(name = "Combination", values = combination_colors) +  # Assign specific colors
  guides(color = guide_legend(title.position = "left", nrow = 1, override.aes = list(size = 10)))  # Adjust legend appearance
umap_plot

#Plot PCA
set.seed(123)
combination_colors <- c("orange", "darkgreen", "blue","deeppink", "red" )
data.pca <- prcomp(df_umapp[6:1285], center = TRUE, scale. = TRUE, rank =30) 

# Get eigenvalues
eigenvalues <- data.pca$sdev^2

# Calculate variance explained by PC1 and PC2
variance_explained_pc1 <- eigenvalues[1] / sum(eigenvalues)
variance_explained_pc2 <- eigenvalues[2] / sum(eigenvalues)

# Convert to percentage
variance_explained_percentage_pc1 <- variance_explained_pc1 * 100
variance_explained_percentage_pc2 <- variance_explained_pc2 * 100


pcaData <- as.data.frame(data.pca$x[, 1:2])
pcaData <- cbind(pcaData, df_umap$Combination) # add species to df
colnames(pcaData) <- c("PC1", "PC2", "Combination")
pca_plot <- ggplot(pcaData, aes(PC1, PC2, color = as.factor(Combination))) +
  geom_point(size = 3) + # Adjust the alpha parameter for transparency
  xlim(c(-50, 50)) +
  ylim(c(-50, 50)) +
  labs(x = 'PC 1', y = 'PC 2') +  # Add axis labels here
  theme_minimal() +
  theme(axis.text = element_text(size = 32, color = "black"),
        axis.title = element_blank(), legend.position = "bottom", legend.title = element_text(size = 16),  legend.text = element_text(size = 14)) + 
  scale_color_manual(name = "Combination", values = combination_colors) +  # Assign specific colors
  guides(color = guide_legend(title.position = "left", nrow = 1, override.aes = list(size = 10)))  # Adjust legend appearance
pca_plot