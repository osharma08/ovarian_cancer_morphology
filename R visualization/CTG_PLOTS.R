library(ggplot2)
library(tidyr)
library(openxlsx)
# Set the working directory
setwd("~/Desktop/ScilifeLab_Work/Phd/Project 1_Comparing CP and NN features for MoA prediction/CTG monoculture data/")

# read ctg data
df <- read.xlsx("ctg_osheen.xlsx")
df <- df[,-3]
df$Group[df$Group == 'TOPO'] <- 'Topoisomerase'
df$Group[df$Group == 'MITOTIC'] <- 'Mitotic'
# Convert the dataset to long format
df_long <- tidyr::pivot_longer(df, cols = c(OvCar8, OvCar3, MH, Kuramochi), names_to = "Cell_Line", values_to = "Viability")

# Custom colors for each Combination
combination_colors <- c("black", "black", "black", "black", "black")


box_jitter_ind_cells <- ggplot(df_long, aes(x = Group, y = Viability, color = Cell_Line)) +
  geom_boxplot(width = 0.6, lwd=1.3) +  
  geom_jitter(position = position_jitter(width = 0.3), alpha = 0.5, size=3) +  # Add jittered points
  scale_color_manual(name = "Cell lines", values = combination_colors) +
  labs(x = "Mechanism of Action", y = "Drug Sensitivity Score", fill = "Group", color = "Group") +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold", size = 30),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 20, color = "black"),
    axis.text.x = element_text(size = 22, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.y = element_blank(), plot.margin = margin(t = 10, r = 10, b = 20, l = 10, unit = "pt")
  )   +

  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 20),
    labels = seq(0, 60, 20)
  ) +
  facet_wrap(~ Cell_Line,nrow=1, scales = "free_y")
box_jitter_ind_cells










box_jitter_alldata <- ggplot(df_long, aes(x = Group, y = Viability, fill = Group)) +
  geom_boxplot(width = 0.5) +  
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +  # Add jittered points
  labs(x = "Mechanism of Action", y = "DSS", fill = "Group", color = "Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),                        # Increase the font size of the axis titles
    strip.background = element_rect(fill = "lightgrey", color = "black"),  # Set background and border color for group titles
    strip.text = element_text(face = "bold", size = 14) , legend.position = "none"                        # Set font style and size for group titles
  ) +
  scale_fill_manual(values = replicate(10, 'lightblue3')) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 5),
    labels = seq(0, 40, 5)
  ) 
box_jitter_alldata








