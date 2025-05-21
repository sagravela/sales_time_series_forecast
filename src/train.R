#!/usr/bin/env Rscript

### MODEL TRAINING

## DEPENDENCIES
# General
library(tidyverse)
library(tsibble)
library(purrr)

# TS model
library(fable)
library(fabletools)
library(feasts)

# Load and save data
library(arrow)

# Miscelaneous
library(progressr)
library(here)

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

#' Load data
#'
#' @returns A `list` of 'tsibbles'.
load_train_data <- function() {
  return(list(
    train = read_parquet(here("data", "processed", "train.parquet")),
    test = read_parquet(here("data", "processed", "test.parquet"))
  ))
}

#' Train ARIMA model
#'
#' Fits an ARIMA default model and a model with lagged predictors.
#' @param ds A `tsibble` containing the training dataset.
#' @returns A `mable` containing the fitted ARIMA model.
train_arima <- function(ds) {
  # Train a default model because some TS arise errors with regressors.
  message("ARIMA training...")
  arima_models <- list(
    arima_def = ARIMA(sqrt(units)),
    arima_lagged = ARIMA(
      sqrt(units) ~ feature + display + tpr_only +
        lag(feature) + lag(display) + lag(tpr_only)
    )
  )
  fit <- with_progress(model(ds, !!!arima_models), enable = TRUE)
  return(
    fit |>
      mutate(arima = if_else(
        is_null_model(arima_lagged), arima_def, arima_lagged
      )) |>
      select(-arima_def, -arima_lagged) |>
      filter(!is_null_model(arima))
  )
}

#' Train STL+ETS
#'
#' @param ds A `tsibble` containing the dataset to train.
#' @returns A `mable` containing the fitted STL+ETS model.
train_stl <- function(ds) {
  # Add STL decomposition model with ETS errors
  message("STL training...")
  with_progress(
    model(ds, stl = decomposition_model(STL(sqrt(units)), ETS(season_adjust ~ season("N")))),
    enable = TRUE
  )
}

#' Train model
#'
#' Train ARIMA and STL models.
#' @param train A `tsibble` containing the training dataset.
#' @returns A `mable` containing the fitted model.
train_model <- function(train) {
  # STL doesn't accept nulls values, so I'll impute them with ARIMA (which accepts nulls)
  fit_arima <- train_arima(train)
  message("Impute nulls with ARIMA...")
  fit_stl <- interpolate(
    fit_arima,
    train |> semi_join(fit_arima, by = c("store_id", "upc_id"))
  ) |>
    train_stl()

  # Join both models in one mable
  fit <- bind_cols(fit_arima, fit_stl["stl"])
  return(fit)
}

#' Model training
#'
#' Train the model in the train set and in the whole dataset.
#' Save the model and residuals.
model_training <- function() {
  message("\n\n========================= TRAINING =========================\n\n")
  data <- load_train_data()
  message("Training data from 'data/processed' loaded successfully.")

  # Train in train set
  message("Training in train set...")
  fit <- train_model(data$train)
  # Add CI and save the forecast as parquet
  dir.create(here("output", "model"), recursive = TRUE, showWarnings = FALSE)
  train_model_path <- here("output", "model", "fit_train.rds")
  saveRDS(fit, train_model_path)
  message("Fitted model written to: ", train_model_path)

  # Remove objects to free memory
  rm(fit)

  ## Train in the whole dataset
  message("Training in whole dataset...")
  full_data <- bind_rows(data$train, data$test)
  fit <- train_model(full_data)

  # Save mable
  fit_path <- here("output", "model", "fit.rds")
  saveRDS(fit, fit_path)
  message("Fitted model written to: ", fit_path)

  # RESIDUALS
  residuals_path <- here("output", "model", "residuals.parquet")
  fit |>
    augment() |>
    write_parquet(residuals_path)
  message("Residuals written to: ", residuals_path)
}

## TRIGGER TRAINING
if (sys.nframe() == 0) {
  model_training()
}
