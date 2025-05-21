#!/usr/bin/env Rscript

### MODEL VALIDATION

## DEPENDENCIES
# General
library(tidyverse)
library(tsibble)
library(purrr)

# Load and save data
library(arrow)

# Visualization
library(ggplot2)

# Miscelaneous
library(glue)
library(progressr)
library(here)

# Load local functions
source(here("src", "models.R"))
source(here("src", "forecast.R"))

# Set up progress bars
handlers(global = TRUE)
handlers(list(
  handler_progress(
    format   = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta",
    width    = 80,
    clear    = FALSE,
    enable   = TRUE
  )
))

#' Data loading
#'
#' @returns A `list` of `tsibble`.
load_sample_data <- function() {
  return(list(
    sample_train = read_parquet(here("data", "processed", "sample_train.parquet")),
    sample_val = read_parquet(here("data", "processed", "sample_val.parquet"))
  ))
}

#' Training function.
#'
#' @description
#' Models to train:
#' - Dinamyc Regression Models with ARIMA errors and their variations
#' with predictors `feature`, `display` and `tpr_only`.
#' - STL decomposition model with ETS errors
#' - Neural Network (NNetAR)
#' - Prophet model
#'
#' I transform units to square root to avoid negative values.
#' @returns A `mable` with fitted models.
train_model <- function(train) {
  message("Training model...")
  trained_mb <- with_progress(model(train, !!!models), enable = TRUE)
  return(trained_mb)
}

#' Get AICc among trained models
#'
#' Get AICc values for each model and count for nulls models for further analysis.
#' @param fit A `mable` with the fitted models.
#' @returns NULL
calculate_aicc <- function(fit) {
  # I will select one of the ARIMA models which minimizes AICc in the training set.
  val_aicc <- fit |>
    select(contains("arima")) |>
    glance() |>
    select(upc_id, store_id, .model, AICc) |>
    pivot_wider(
      names_from = .model,
      values_from = AICc
    )
  dir.create(here("output", "validation"), recursive = TRUE, showWarnings = FALSE)
  val_aicc |> write_csv2(here("output", "validation", "aicc.csv"))
  message("AICc values written to: output/validation/aicc.parquet")
}

#' Calculate RMSE
#'
#' Calculate RMSE for each model.
#' @param fc A `fable` with forecasts.
#' @param sample_val A `tsibble` with validation data.
#' @returns A `fable` with forecasts for the validation data.
calculate_rmse <- function(fc, sample_val) {
  fc |>
    as_tibble() |>
    left_join(
      sample_val,
      by = c("week", "store_id", "upc_id"),
      suffix = c("", "")
    ) |>
    group_by(store_id, upc_id, .model) |>
    summarise(
      RMSE = sqrt(mean((units - .mean)^2)),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = .model,
      values_from = RMSE
    ) |>
    write_csv2(here("output", "validation", "rmse.csv"))
  message("RMSE values written to: output/validation/rmse.csv")
}

#' Model validation function
#'
#' Run validation processes.
model_validation <- function() {
  message("\n\n========================= VALIDATION =========================\n\n")
  data <- load_sample_data()

  message("Data from 'data/processed' loaded successfully.")
  fit <- train_model(data$sample_train)
  fit |> saveRDS(here("output", "validation", "fit.rds"))
  # fit <- readRDS(here("output", "validation", "fit.rds"))
  calculate_aicc(fit)
  fit <- select(fit, -arima_base, -arima_seasonal, -arima_seasonal_lagged, -prophet, -nnetar)
  message("Forecasting...")
  batch_size <- 8
  get_forecast(fit, data$sample_val, batch_size) |>
    calculate_rmse(data$sample_val)
}

## TRIGGER VALIDATION
if (sys.nframe() == 0) {
  model_validation()
}

# Based on RMSE, ARIMA lagged model with predictors performs better
# than STL decomposition model with ETS errors in the majority of the validation set.
# Anyway, both perform well in the validation set, so I will consider apply both models.
