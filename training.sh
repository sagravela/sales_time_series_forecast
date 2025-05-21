#!/bin/bash

# Run data cleaning processes
./src/data_cleaning.R

# Run data analysis
quarto render data_analysis.qmd --to all --output-dir output/reports/

# Run data processing for modeling
./src/model_data_prep.R

# Run model training
./src/train.R

# Run model forecasting
./src/forecast.R
