# Load necessary libraries
library(fmsb)
library(readxl)
library(dplyr)

# Define the directory and list of files
base_path <- "/Users/osheen.sharma/Desktop/ScilifeLab_Work/Phd/Project 1_Comparing CP and NN features for MoA prediction/Enrichment polygons/Enrichment Scores After Revision/ResNet50_90//"
files <- c("pval_norm_KB.xlsx",  "pval_norm_MHB.xlsx", "pval_norm_O3B.xlsx" , "pval_norm_O8W.xlsx", "pval_norm_KW.xlsx") #
#files <- c("pval_norm_KB.xlsx","pval_norm_O3B.xlsx") #

# Initialize list to store data frames
data_list <- list()

# Loop over files to read and process data individually
for (file_name in files) {
  # Construct the full file path
  file_path <- file.path(base_path, file_name)
  
  # Extract the combination name (e.g., KB, MHB, O3B)
  combination <- strsplit(file_name, "_|\\.")[[1]][3]
  
  # Read the file
  df <- read_excel(file_path)
  
  # Extract the 'MoA' column (assuming it's the second column) and the last column (% Enriched)
  MoA_column <- df[[2]]  # Adjust the index if 'MoA' is in a different position
  Enriched_column <- df[[ncol(df)]]
  
  # Create a data frame with 'MoA' and 'Enriched' values
  df_combination <- data.frame(MoA = MoA_column, Enriched = Enriched_column)
  
  # Store in the list with the combination name
  data_list[[combination]] <- df_combination
}

# Use 'MoA' names from any one file since they are the same across files
MoA_names <- data_list[[1]]$MoA

# Initialize the radar chart data frame
radar_data <- data.frame(MoA = MoA_names)

# Add upper limit (100) and lower limit (0) rows
upper_limit <- rep(100, length(MoA_names))
lower_limit <- rep(0, length(MoA_names))

# Start building the radar data with Max and Min rows
radar_data_values <- data.frame(
  row.names = c("Max", "Min"),
  matrix(ncol = length(MoA_names), nrow = 2)
)
colnames(radar_data_values) <- MoA_names
radar_data_values[1, ] <- upper_limit
radar_data_values[2, ] <- lower_limit

# Append each combination's 'Enriched' values as a new row
for (combination in names(data_list)) {
  enriched_values <- data_list[[combination]]$Enriched
  radar_data_values <- rbind(radar_data_values, setNames(as.numeric(enriched_values), MoA_names))
  rownames(radar_data_values)[nrow(radar_data_values)] <- combination
}

# Ensure all data is numeric
radar_data_values[] <- lapply(radar_data_values, as.numeric)

# Update custom colors for each combination
colors_border <- c(
  "darkorange",   # KB
  "blue",          # MHB
  "deeppink",     # O3B
  "red",          # O8W
  "darkgreen"     # KW
)
colors_in <- c(
  rgb(1, 0.55, 0, 0.2),  # Semi-transparent deep orange
  rgb(0, 0, 1, 0.2) ,    # Semi-transparent blue
 rgb(1, 0.08, 0.58, 0.2), # Semi-transparent deep pink
  rgb(1, 0, 0, 0.2),     # Semi-transparent red
  rgb(0, 0.5, 0, 0.2)    # Semi-transparent dark green
)

# Plot radar chart with specified y-limit and colors
radarchart(
  radar_data_values,
  axistype = 1,
  # Custom polygon
  pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
  # Custom grid
  cglcol = "grey", cglty = 1, axislabcol = "grey",
  caxislabels = c("0%", "25%", "50%", "75%", "100%"), cglwd = 0.8,
  # Custom labels
  vlcex = 2.5
)

# Add legend with new colors
legend(
  x = 1, y = 5,
  legend = rownames(radar_data_values)[-c(1, 2)],
  bty = "n", pch = 20, col = colors_border,
  text.col = "black", cex = 1.2, pt.cex = 3
)
