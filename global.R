# GLOBAL VARIABLES AND DATA

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

# Miscellaneous
library(here)

## Load data
transactions_final <- read_parquet(here("data", "processed", "transactions_final.parquet"))
store <- read_parquet(here("data", "processed", "store.parquet"))
products <- read_parquet(here("data", "processed", "products.parquet"))
# Forecasting parquets
fc_train <- read_parquet(here("output", "model", "fc_train.parquet")) |>
  mutate(.model = toupper(.model))
fc_arima <- read_parquet(here("output", "model", "fc_arima.parquet")) |>
  mutate(.model = toupper(.model))
fc_stl <- read_parquet(here("output", "model", "fc_stl.parquet")) |>
  mutate(.model = toupper(.model))
# Residuals
res <- read_parquet(here("output", "model", "residuals.parquet")) |>
  mutate(.model = toupper(.model))
