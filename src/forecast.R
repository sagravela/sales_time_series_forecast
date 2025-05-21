#!/usr/bin/env Rscript

### MODEL FORECASTING

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
library(glue)
library(here)

# Utils
source(here("src", "train.R"))

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

#' Forecasting function
#'
#' @param fit A `mable` with fitted models.
#' @param test A `tsibble` with test data.
#' @param bs An `integer` with batch size.
#' @returns A `fable` with forecasts for the test data.
get_forecast <- function(fit, test, bs) {
  batchs <- seq(1, nrow(fit), by = bs)
  with_progress(
    {
      p <- progressor(along = batchs)
      map_dfr(
        batchs,
        function(x) {
          end <- min(x + bs - 1, nrow(fit))
          # p(sprintf("%s/%s", end, nrow(fit)))
          fabletools::forecast(fit[x:end, ], test)
        }
      )
    },
    enable = TRUE
  )
}

#' Helper function to add CI to fables
#'
#' @param fc A `fable` object with forecasts.
#' @returns A `tsibble` with lower and upper bounds.
add_ci <- function(fc) {
  #' Helper function to calculate lower and upper bounds
  #'
  #' @param level A `numeric` with the confidence level.
  #' @param pos A `character` with the position of the bound (lower or upper).
  #' @returns A `numeric` with the bound or NA if there is an error.
  hilo_bound <- function(level, pos) {
    map_dbl(
      fc$units,
      ~ tryCatch(
        {
          if (pos == "upper") hilo(.x, level)$upper else hilo(.x, level)$lower
        },
        error = function(e) {
          NA
        }
      )
    )
  }

  # Add lower and upper bounds
  fc$low80 <- hilo_bound(80, "lower")
  fc$up80 <- hilo_bound(80, "upper")
  fc$low95 <- hilo_bound(95, "lower")
  fc$up95 <- hilo_bound(95, "upper")

  return(
    fc |>
      as_tsibble() |>
      select(-units) |>
      rename(p_units = ".mean")
  )
}

#' Forecasting function for ARIMA
#'
#' @description
#' Forecast for Dinamyc Regression Model with ARIMA errors.
#' Because this model needs regressors, we will forecast in all possible combination of
#' regressors (feature, display, tpr_only).
#' @param fit A `mable` with fitted models.
#' @param data A `tsibble` with time series data.
#' @param bs An `integer` with batch size.
#' @returns A `fable` with forecasts for the test data.
forecast_arima <- function(fit, data, bs) {
  combinations <- list(
    list(feature = 0, display = 0, tpr_only = 0),
    list(feature = 1, display = 0, tpr_only = 0),
    list(feature = 0, display = 1, tpr_only = 0),
    list(feature = 0, display = 0, tpr_only = 1),
    list(feature = 1, display = 1, tpr_only = 0)
  )

  fc_arima <- map_dfr(
    seq_along(combinations), function(i) {
      message(
        "\n\nFeature = ", combinations[[i]]$feature,
        "| Display = ", combinations[[i]]$display,
        "| TPR = ", combinations[[i]]$tpr_only,
        "\n"
      )
      ds <- new_data(data, n = 2) |>
        mutate(!!!combinations[[i]])
      fc <- get_forecast(select(fit, -stl), ds, bs) |>
        mutate(fc_ind = i)
      # Save checkpoint just in case
      # saveRDS(fc, glue("output/model/fc_arima_{i}.rds"))

      # Return as tibble to avoid problems binding later
      return(as_tibble(fc))
    }
  )
  return(
    fc_arima |>
      as_tsibble(index = week, key = c(store_id, upc_id, .model, fc_ind)) |>
      add_ci()
  )
}

#' Model forecasting
#'
#' Load models and forecast in train/test and whole data.
#' Forecasts are saved in output/model/.
model_forecasting <- function() {
  message("\n\n========================= FORECASTING =========================\n")
  data <- load_train_data()
  batch_size <- 16

  # TRAIN/TEST FORECAST
  # Load models
  fit <- readRDS(here("output", "model", "fit_train.rds"))

  # Forecast
  message("\nForecasting in Train/Test Datasets...")
  fc_train_path <- here("output", "model", "fc_train.parquet")
  get_forecast(fit, data$test, batch_size) |>
    add_ci() |>
    write_parquet(fc_train_path)
  message("\nSuccessful. Forecasts written to: ", fc_train_path)

  # WHOLE DATA FORECAST
  # Load model
  fit <- readRDS(here("output", "model", "fit.rds"))

  message("\n\nForecasting in whole data...")
  message("\nForecasting STL+ETS model...")
  # Forecast STL decomposition model with ETS errors
  full_data <- bind_rows(data$train, data$test)
  fc_stl_path <- here("output", "model", "fc_stl.parquet")
  get_forecast(
    fit |> select(-arima),
    new_data(full_data, n = 2),
    batch_size
  ) |>
    add_ci() |>
    write_parquet(fc_stl_path)
  message("\nSuccessful. Forecasts written to: ", fc_stl_path)

  message("\n\nForecasting ARIMA model...")
  fc_arima_path <- here("output", "model", "fc_arima.parquet")
  forecast_arima(fit, full_data, batch_size) |> write_parquet(fc_arima_path)
  message("\nSuccessful. Forecasts written to: ", fc_arima_path)
}

# TRIGGER FORECASTING
if (sys.nframe() == 0) {
  model_forecasting()
}
