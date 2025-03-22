#!/usr/bin/env Rscript

### MODEL VALIDATION

## DEPENDENCIES
# General
library(tidyverse)
library(tsibble)
library(purrr)

# TS model
library(fable)
# Install fable.prophet from CRAN with install.packages("fable.prophet"),
# it doesn't available in Conda Environment at the moment
library(fable.prophet)
library(fabletools)
library(feasts)

# Load and save data
library(arrow)

# Visualization
library(ggplot2)

# Miscelaneous
library(glue)
library(progressr)
library(here)

# Utils
source(here("src", "utils.R"))
source(here("src", "forecast.R"))

#' Data loading
#'
#' @returns A `list` of `tsibble`.
load_sample_data <- function() {
  return(list(
    sample_train = read_parquet(here("data", "processed", "sample_train.parquet")),
    sample_val = read_parquet(here("data", "processed", "sample_val.parquet"))
  ))
}

# Define models
models <- list(
  # ARIMA default
  arima_base = ARIMA(sqrt(units) ~ feature + display + tpr_only),
  # ARIMA with lagged predictors
  arima_lagged = ARIMA(
    sqrt(units) ~ feature + display + tpr_only +
      lag(feature) + lag(display) + lag(tpr_only)
  ),
  # ARIMA with seasonal predictors
  arima_seasonal = ARIMA(
    sqrt(units) ~ PDQ(0, 0, 0) + fourier(K = 6) +
      feature + display + tpr_only
  ),
  # ARIMA with seasonal and lagged predictors
  arima_seasonal_lagged = ARIMA(
    sqrt(units) ~ PDQ(0, 0, 0) + pdq(d = 0) +
      fourier(K = 6) + feature + display + tpr_only +
      lag(feature) + lag(display) + lag(tpr_only)
  ),
  # Seasonal decomposition model with ETS errors.
  stl = decomposition_model(
    STL(sqrt(units)),
    ETS(season_adjust ~ season("N"))
  ),
  # Default Neural Network Model with predictors.
  nnetar = NNETAR(sqrt(units) ~ feature + display + tpr_only),
  # Default prophet model with predictors.
  prophet = fable.prophet::prophet(
    sqrt(units) ~ feature + display + tpr_only
  )
)

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

#' Compare AICc among trained models
#'
#' Plot AICc values for each model and count for nulls models for further analysis.
#' @param fit A `mable` with the fitted models.
#' @returns A `mable` with the best models by AICc score.
compare_aicc <- function(fit) {
  # I will select one of the ARIMA models which minimizes AICc in the training set.
  val_aicc <- fit |>
    select(contains("arima")) |>
    glance() |>
    select(upc_id, store_id, .model, AICc) |>
    pivot_wider(
      names_from = .model,
      values_from = AICc
    )

  # Create a column called `best_model` with the arima model which minimizes AICc, then count them.
  # Also, add the mean AICc among all models.
  arima_model_names <- c("arima_base", "arima_lagged", "arima_seasonal", "arima_seasonal_lagged")
  fig <- val_aicc %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      mean_aicc = mean(c_across(all_of(arima_model_names)), na.rm = TRUE),
      best_model = names(.)[2 + which.min(c_across(all_of(arima_model_names)))]
    ) |>
    group_by(best_model) |>
    summarise(
      mean_aicc = mean(mean_aicc),
      n_best_model = n()
    ) |>
    mutate(mean_aicc_text = paste("Mean AICc: ", round(mean_aicc, 2))) |>
    ggplot(aes(best_model, n_best_model, fill = mean_aicc)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = mean_aicc_text), vjust = -0.5) +
    labs(
      title = "Best ARIMA models by AICc",
      y = "Number of best models by AICc",
      caption = glue("Models: {paste(arima_model_names, collapse = ', ')}"),
    )

  # Create the neccessary folders
  dir.create(here("output", "plots"), recursive = TRUE, showWarnings = FALSE)
  ggsave("arima_aicc.png", path = here("output", "plots"))
  message("Models AICc plot written to: output/plots/arima_aicc.png")

  # The best model among arima variations is the lagged version of ARIMA.
  # Prophet model has more than 60% of null models so I will remove it from the mable.
  # Because NNETAR is very slow at predicting, I will remove it from the mable.
  # Also, NNETAR is not performing well in the validation set due to lack of historic data.
  return(select(fit, -arima_base, -arima_seasonal, -arima_seasonal_lagged, -prophet, -nnetar))
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
    )
}

#' RMSE Comparision
#'
#' Plot best models count by minimum RMSE among models with their median.
#' @param rmse_fc A `tibble` with RMSE among models.
compare_rmse <- function(rmse_fc) {
  rmse_fc %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      best_model = names(.)[2 + which.min(c_across(c("arima_lagged", "stl")))],
      mean_rmse = mean(c_across(c("arima_lagged", "stl")), na.rm = TRUE),
    ) |>
    group_by(best_model) |>
    summarise(
      # The median is used instead of the mean because the median is more robust to outliers
      median_rmse = median(mean_rmse),
      n_best_model = n()
    ) |>
    mutate(median_rmse_text = paste("Median RMSE: ", round(median_rmse, 2))) |>
    ggplot(
      aes(reorder(best_model, n_best_model, decreasing = TRUE), n_best_model, fill = median_rmse)
    ) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = median_rmse_text), vjust = -0.5) +
    labs(x = NULL, y = "Number of best models by RMSE", title = "Models performance in validation set")
  ggsave("models_rmse.png", path = here("output", "plots"))
  message("Models performance plot written to: output/plots/models_rmse.png")
}

#' Model validation function
#'
#' Run validation processes.
model_validation <- function() {
  message("\n\n========================= VALIDATION =========================\n\n")
  data <- load_sample_data()

  message("Data from 'data/processed' loaded successfully.")
  fit <- train_model(data$sample_train) |>
    compare_aicc()

  message("Forecasting...")
  batch_size <- 8
  get_forecast(fit, data$sample_val, batch_size) |>
    calculate_rmse(data$sample_val) |>
    compare_rmse()
}

## TRIGGER VALIDATION
if (sys.nframe() == 0) {
  model_validation()
}

# Based on RMSE, ARIMA lagged model with predictors performs better
# than STL decomposition model with ETS errors in the majority of the validation set.
# Anyway, both perform well in the validation set, so I will consider apply both models.
