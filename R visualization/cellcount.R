library(ggplot2)
library(openxlsx)
library("writexl")
library(dplyr)
library(ggridges)

#set the working directory
setwd("~/Desktop/ScilifeLab_Work/Phd/Project 1_Comparing CP and NN features for MoA prediction/Cancer cell count")

#Read all the data frames 
data <- read.xlsx("Cancer_Count.xlsx")
# Filter the data
filtered_data <- subset(data, !(MoA %in% c("Negative Controls", "Miscl.") & Label != "DMSO"))
# Filter data for MoA excluding Negative Controls
filtered_data_moa <- subset(filtered_data, MoA != "Negative Controls")
# Filter data for DMSO labels
filtered_data_dmso <- subset(filtered_data, Label == "DMSO")
# Combine filtered data frames
combined_data <- rbind(filtered_data_moa, filtered_data_dmso)
# Create a new dataframe for DMSO data
dmso_data <- subset(filtered_data, Label == "DMSO")
# Merge the DMSO data with the filtered data based on Combination
filtered_data_with_dmso <- merge(filtered_data, dmso_data[,c("Combination", "Cancer_Cell_Count")], by="Combination", suffixes=c("", "_DMSO"), all.x=TRUE)

# Filter out "Negative Controls" MoA category
filtered_data_no_negative_controls <- subset(filtered_data_with_dmso, MoA != "Negative Controls")

combination_colors <- c("darkorange", "darkgreen", "blue", "deeppink", "red")
com_col <- c("#F7B98F", "#82A57B", "#8CA2D9", "#E2A4E3", "#D58E95")
grey_color <- "gray22"

# Plot density curves
ggplot(filtered_data_no_negative_controls, aes(x = Cancer_Cell_Count, y = Combination)) +
  geom_density_ridges(aes(fill = Combination, color = Combination), scale = 1) +
  geom_density_ridges(aes(x = ifelse(is.na(Cancer_Cell_Count_DMSO), Cancer_Cell_Count, Cancer_Cell_Count_DMSO), y = Combination), 
                      fill = grey_color, color = grey_color, alpha = 0.5, scale = 1, show.legend = FALSE) +  # Add grey density plot
  scale_fill_manual(name = "Combination", values = com_col) +
  scale_color_manual(name = "Combination", values = combination_colors, guide = guide_legend(override.aes = list(fill = NA))) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold", size = 25),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 20, color = "black"),  # X-axis title style
    axis.text.x = element_text(size = 22, color = "black",  angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),  # Remove Y-axis title
  ) +
  facet_wrap(~MoA, ncol = 5) +
  xlim(0, 3000) +
  xlab("Cancer Cell Count")  # Set X-axis label title