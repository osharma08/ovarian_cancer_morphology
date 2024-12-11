library(ggplot2)
library(openxlsx)
library("writexl")
library(dplyr)
library(ggridges)


#set the working directory
setwd("~/Desktop/ScilifeLab_Work/Phd/Project 1/Drug sensitivity score_BREEZE/")

#Read all the data frames 
df <- read.xlsx("DSS_Selected MoA_allcombinations.xlsx")

# Reshape data for ggplot2
df_long <- reshape2::melt(df, id.vars = 'Mechanism_of_action')


# Ridge plot for cell count for each cell line combination individually

combination_colors <- c("darkorange", "darkgreen", "blue", "deeppink", "red")
com_col <- c("#F7B98F","#82A57B","#8CA2D9","#E2A4E3","#D58E95")


# Create boxplot with facets and jittered points
ggplot(df_long, aes(x = Mechanism_of_action, y = value,  color = variable)) +
  geom_boxplot( width = 0.6, lwd=1.3) +
  geom_jitter(position = position_jitter(width = 0.3), alpha = 0.5, size=3) +
  scale_fill_manual(name = "Combination", values = com_col) +
  scale_color_manual(name = "Combination", values = combination_colors) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold", size = 30),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 20, color = "black"),
    axis.text.x = element_text(size = 22, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.y = element_blank(), plot.margin = margin(t = 10, r = 10, b = 20, l = 10, unit = "pt")
  ) +
  labs(x = "Mechanism of Action") +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  ylab("DSS Variable Value")



