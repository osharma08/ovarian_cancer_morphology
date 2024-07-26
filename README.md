# Evaluating Feature Extraction in Ovarian Cancer Cell Line Co-Cultures Using Deep Neural Networks
# 📁 Project Overview

Welcome to the project repository! This project consists of several components to handle image processing, feature extraction, and visualization from 2D co-culture assays. Here’s a brief overview of the folders and their contents:

## 1. CellProfiler Feature Extraction and Illumination Correction 🧪
This folder contains two main pipelines:

- **Illumination Correction**: Corrects for uneven illumination in raw images to ensure accurate feature extraction.
- **CellProfiler Feature Extraction**: Extracts single-cell features from cancer cells using CellPose masks. The features are then aggregated by well and exported as Excel files.


## 2. Data Preprocessing and Feature Extraction 🔄
Scripts in this folder are designed to ensure precise data extraction and correction, improving the quality of downstream analyses.

- **Data Preprocessing Scripts**: Includes merging fields of view, CellPose segmentation, and bounding box creation to prepare raw data for analysis.
- **Feature Extraction**: Python scripts for extracting single-cell features using the ResNet50 model.
- **Enrichment Score Calculation**: Code for calculating enrichment scores to assess the significance of features.

These scripts are used for preparing and processing data before the feature extraction and analysis stages.

## 3. R Visualization 📊
This folder contains R scripts for visualizing extracted features:

- **UMAP and PCA**: Dimensionality reduction techniques for visualizing complex data.
- **Box Plots and Density Plots**: Statistical plots to analyze and compare feature distributions.

These visualizations help in understanding the patterns and relationships within the data, providing insights into the results of your analyses.

---

Feel free to explore each folder to dive deeper into the components of this project.🚀

