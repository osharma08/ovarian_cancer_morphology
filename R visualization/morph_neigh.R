library('ggplot2')
library('writexl')
library('openxlsx')

setwd("~/Desktop/ScilifeLab_Work/Phd/Project 1/Morphology_Neighbour_DMSO")

# Read the data (replace with the appropriate file path)
df <- read.xlsx("morph_neigh_data_dmso.xlsx")
df <- df[, -c(2,3,4,8)]

colnames(df) <- gsub("\\.", " ", colnames(df))


# Reshape the data to long format
df_long <- tidyr::pivot_longer(df, cols = colnames(df[, -5]), names_to = "variable")

# Custom colors for each Combination
combination_colors <- c("orange", "darkgreen", "blue", "deeppink", "red")

ggplot(df_long, aes(x = Combination, y = value, color = Combination)) +
  geom_boxplot(width=0.75, lwd=1.3) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.3), alpha = 0.5, size=3) +
  scale_color_manual(name = "Coculture Combinations:", values = combination_colors) +
  facet_wrap(~ variable, ncol = 4, nrow = 5, scales = "free_y") +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 22, color='black'),
        axis.text.x = element_blank(),
        strip.background = element_rect(fill = "lightgrey", color = "black"),
        strip.text = element_text(size = 30, face = "bold"), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1.5, "lines"),
        legend.position = "none")




# Filter the data for the "eccentricity" variable
df_long_eccentricity <- df_long[df_long$variable == "Eccentricity" ]

ggplot(df_long_eccentricity, aes(x = Combination, y = value, color = Combination)) +
  geom_boxplot(width=0.5, size=1) +
  geom_jitter(position = position_jitterdodge(dodge.width = 1)) +
  scale_color_manual(name = "Coculture Combinations:", values = combination_colors) +
  facet_wrap(~ variable, ncol = 3, nrow = 3, scales = "free_y") +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 20),
        axis.text.x = element_blank(),
        #strip.background = element_rect(fill = "lightgrey", color = "black"),
        #strip.text = element_text(size = 25, face = "bold"), 
        #legend.title = element_text(size = 14),
        #legend.text = element_text(size = 12),
        legend.key.size = unit(1.5, "lines"),
        legend.position = "bottom")
