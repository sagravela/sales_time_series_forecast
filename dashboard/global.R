## GLOBAL VARIABLES AND DATA

## DEPENDENCIES
# General
library(tidyverse)
library(tsibble)

# TS analysis
library(fabletools)
library(feasts)

# Visualization
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(DT)

# Load data
library(arrow)

## Load data
transactions_final <- read_parquet("data/processed/transactions_model.parquet")
store <- read_parquet("data/processed/store.parquet")
products <- read_parquet("data/processed/products.parquet")
# Forecasting parquets
fc_train <- read_parquet("data/fc_train.parquet") |> mutate(.model = toupper(.model))
fc_arima <- read_parquet("data/fc_arima.parquet") |> mutate(.model = toupper(.model))
fc_stl <- read_parquet("data/fc_stl.parquet") |> mutate(.model = toupper(.model))
# Residuals
res <- read_parquet("data/residuals.parquet") |> mutate(.model = toupper(.model))
