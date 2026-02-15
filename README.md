# 🏙 KL Property Market Analysis (R)
## 📌 Overview

This project analyzes residential property data in Kuala Lumpur using R.
The objective is to explore pricing patterns based on:

Property size

Furnishing status

Location (KLCC vs Other areas)

Price distribution across size ranges

The project is structured into modular analysis scripts and can be executed sequentially through a single entry point.

# Features
## Data Cleaning

Duplicate removal

Missing value handling

Column standardization

Price formatting cleanup

Size conversion to square feet

Location normalization

## Furnishing & Pricing Analysis

Average price by furnishing status

KLCC vs non-KLCC furnishing comparison

Regression relationship between size and price

## Location & Size Range Analysis

Property distribution by size range

Price comparison: KLCC vs Other locations

Visualization of pricing trends

## Visualizations

The project generates:

Bar charts (price comparison)

Scatter plots (size vs price)

Distribution charts by size range

Comparative KLCC analysis

### All visualizations are created using:

ggplot2

dplyr

scales

# How to Run

Ensure the dataset is placed in:

data/kl_property_data.csv

Open R or RStudio in the project directory.

Run:

source("main.R")

All analysis scripts will execute in order.

# Dependencies

Make sure the following R packages are installed:

install.packages(c("dplyr", "ggplot2", "stringr", "scales"))

# Dataset

The dataset contains property listings with fields such as:

Property location

Size (square feet)

Price (RM)

Furnishing status

## Notes

The project uses modular scripts for separation of concerns.

Dataset path is currently local; adjust if necessary for your environment.

Designed for exploratory data analysis and academic demonstration purposes.
