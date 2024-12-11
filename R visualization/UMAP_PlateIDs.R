library('writexl')
library('umap')
library('ggplot2')
library('openxlsx')
library('plotly') 
library('ggfortify')
library("dplyr")
library("Rtsne")
library("ggExtra")
library("harmony")
library("ggcorrplot")
library("RColorBrewer")
library("tidyverse")
library("corrplot")
library("writexl")
library("clusterSim")
library('ggfortify')
library("dplyr")
library("Rtsne")
library("irlba")
library("pracma")
setwd("~/Desktop/ScilifeLab_Work/Phd/Project 1_Comparing CP and NN features for MoA prediction/UMAPS_DMSO/Unnormalized data/")

#PLot UMAP with PlateIDs
norm_cp_all <- read.xlsx("unnorm_efficientnetb0_all.xlsx")

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
df$Umap_Label[df$Label == "DMSO"] <- "Negative controls"
df$Umap_Label[df$Label == "BzCl"] <- "Positive controls"
df$Umap_Label[df$Label == "Drug"] <- "Treatments"
df <- df[df$Compound != "cells", ]

df_umap <- df
df_umap[is.na(df_umap) | df_umap == " " | df_umap == "NA" | df_umap == "-inf" | df_umap == "inf"] <- 0
df_umap <- na.omit(df_umap)
df_umap <- df_umap[, colSums(is.null(df_umap) | is.na(df_umap) | df_umap == "") == 0]

# Get column names ending with "_X" or "_Y" this is only applicable for CP features
columns_to_drop <- grep("_X$|_Y$", names(df_umap), value = TRUE)
# Remove the columns
df_umap <- df_umap[, !(names(df_umap) %in% columns_to_drop)]

df_umapp <- df_umap
df_umapp <- df_umapp[df_umapp$Combination %in% c('MHB'), ]

df_umapp[, 6:1286] <- lapply(df_umapp[, 6:1286], as.numeric) #6:229 FOR CP and 7:518 for nn

df_umapp[6:1286] <- scale(df_umapp[6:1286]) #FOR kb
# Add a small constant to avoid zero vectors
df_umapp[6:1286] <- df_umapp[6:1286] + 1e-10 #FOR MHB


data.umap <- umap(df_umapp[6:1286],  learning_rate = 0.001, metric = "cosine", min_dist = 0.99, n_neighbors=30)  

# Define a color palette for Plate numbers
plate_colors <- c("red", "green", "blue4", "deeppink", "darkgreen", "cyan4", "purple", "brown")
set.seed(123)
piris <- ggplot(df_umapp, aes(x = data.umap$layout[, 1], y = data.umap$layout[, 2], colour = as.factor(Plate.number))) +
  geom_point(size = 3, alpha = 0.5) + # Adjust the alpha parameter for transparency
  xlim(c(-10, 10)) +
  ylim(c(-10, 10)) +
  labs(x = 'UMAP 1', y = 'UMAP 2') +  # Add axis labels here
  theme_minimal() +
  theme(axis.text = element_text(size = 32, color = "black"),
        axis.title = element_blank(), legend.position = "bottom", legend.title = element_text(size = 16),  legend.text = element_text(size = 14)) + 
  scale_color_manual(name = "Plate IDs", values = plate_colors) +  # Assign specific colors
  guides(color = guide_legend(title.position = "left", nrow = 1, override.aes = list(size = 10)))  # Adjust legend appearance
ggMarginal(piris, groupColour = TRUE, groupFill = TRUE, alpha = 0.2)


# Define a color palette for Plate numbers
plate_colors <- c("red", "blue", "darkgrey")
set.seed(123)
piris <- ggplot(df_umapp, aes(x = data.umap$layout[, 1], y = data.umap$layout[, 2], colour = as.factor(Label))) +
  geom_point(size = 3, alpha = 0.5) + # Adjust the alpha parameter for transparency
  xlim(c(-10, 10)) +
  ylim(c(-10, 10)) +
  labs(x = 'UMAP 1', y = 'UMAP 2') +  # Add axis labels here
  theme_minimal() +
  theme(axis.text = element_text(size = 32, color = "black"),
        axis.title = element_blank(), legend.position = "bottom", legend.title = element_text(size = 16),  legend.text = element_text(size = 14)) + 
  scale_color_manual(name = "Condition", values = plate_colors) +  # Assign specific colors
  guides(color = guide_legend(title.position = "left", nrow = 1, override.aes = list(size = 10)))  # Adjust legend appearance
ggMarginal(piris, groupColour = TRUE, groupFill = TRUE, alpha = 0.2)







#after batch correction
metadata <- df_umapp[, c(2, 3, 230:237)]
features <- df_umapp[, 6:229]

harmonized_features <- HarmonyMatrix(data = features, meta_data = metadata, vars_use = c("Plate.number"))

#Combine crrected data with metadata
harmonized_data <- cbind(metadata, harmonized_features)
data.umap <- umap(harmonized_data[11:233],  learning_rate = 0.001, metric = "euclidean", min_dist = 0.99)  

# Define a color palette for Plate numbers
plate_colors <- c("red", "green", "blue4", "deeppink", "darkgreen", "cyan4", "purple", "brown")
set.seed(123)
piris <- ggplot(harmonized_data, aes(x = data.umap$layout[, 1], y = data.umap$layout[, 2], colour = as.factor(Plate.number))) +
  geom_point(size = 3, alpha = 0.5) + # Adjust the alpha parameter for transparency
  labs(x = 'UMAP 1', y = 'UMAP 2') +  # Add axis labels here
  theme_minimal() +
  theme(axis.text = element_text(size = 32, color = "black"),
        axis.title = element_blank(), legend.position = "bottom", legend.title = element_text(size = 16),  legend.text = element_text(size = 14)) + 
  scale_color_manual(name = "Plate Number", values = plate_colors) +  # Assign specific colors
  guides(color = guide_legend(title.position = "left", nrow = 1, override.aes = list(size = 10)))  # Adjust legend appearance
ggMarginal(piris, groupColour = TRUE, groupFill = TRUE, alpha = 0.2)


# Define a color palette for Plate numbers
plate_colors <- c("red", "lightgreen")
set.seed(123)
piris <- ggplot(harmonized_data, aes(x = data.umap$layout[, 1], y = data.umap$layout[, 2], colour = as.factor(Umap_Label))) +
  geom_point(size = 3, alpha = 0.5) + # Adjust the alpha parameter for transparency
  labs(x = 'UMAP 1', y = 'UMAP 2') +  # Add axis labels here
  theme_minimal() +
  theme(axis.text = element_text(size = 32, color = "black"),
        axis.title = element_blank(), legend.position = "bottom", legend.title = element_text(size = 16),  legend.text = element_text(size = 14)) + 
  scale_color_manual(name = "Condition", values = plate_colors) +  # Assign specific colors
  guides(color = guide_legend(title.position = "left", nrow = 1, override.aes = list(size = 10)))  # Adjust legend appearance
ggMarginal(piris, groupColour = TRUE, groupFill = TRUE, alpha = 0.2)



# Specify the file path where you want to save the Excel file
file_path <- "/Users/osheen.sharma/Desktop/ScilifeLab_Work/Phd/Project 1/Batch corrected cp features/O3B.xlsx"
# Save the dataframe as an Excel file
write.xlsx(harmonized_data, file_path, rowNames = FALSE)
