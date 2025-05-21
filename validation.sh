#!/bin/bash

# Run data cleaning processes
./src/data_cleaning.R

# Create data analysis report
quarto render data_analysis.qmd --to all --output-dir output/reports/

# Run data processing for modeling
./src/model_data_prep.R

# Run model validation
./src/validate.R

# Create validation report
quarto render validation.qmd --to all --output-dir output/reports/
