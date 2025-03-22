#!/usr/bin/env Rscript

# Run data cleaning processes
source("src/data_cleaning.R")
clean_data()

# Run data analysis
source("src/data_analysis.R")
analysis()

# Run data processing for modeling
source("src/model_data_prep.R")
model_data_processing()

# Run model validation
source("src/validation.R")
model_validation()

# Run model training
source("src/train.R")
model_training()

# Run model forecasting
source("src/forecast.R")
model_forecasting()
