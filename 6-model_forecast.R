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

## LOAD DATA
train <- read_parquet("data/train.parquet")
test <- read_parquet("data/test.parquet")
transactions_final <- read_parquet("data/transactions_final.parquet")

# Set up progress bars
handlers(global = TRUE)
handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsedfull ETA: :eta",
    width    = 80,
    complete = "#",
    clear    = FALSE
  )
))

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# Get forecast
get_forecast <- function(batchs, ds) {
  p <- progressor(along = batchs)

  fc_train <- map_dfr(
    batchs,
    function(x) {
      end <- min(x + batch_size - 1, nrow(fit))
      p(sprintf("%s/%s", end, nrow(fit)))
      forecast(fit[x:end, ], ds)
    }
  )
}

# Helper function to add CI to fables
add_ci <- function(fc) {
  # Add lower and upper bounds
  fc$low80 <- map_dbl(
    fc$units,
    ~ tryCatch(
      {
        hilo(.x, 80)$lower
      },
      error = function(e) {
        NA
      }
    )
  )
  fc$up80 <- map_dbl(
    fc$units,
    ~ tryCatch(
      {
        hilo(.x, 80)$upper
      },
      error = function(e) {
        NA
      }
    )
  )
  fc$low95 <- map_dbl(
    fc$units,
    ~ tryCatch(
      {
        hilo(.x, 95)$lower
      },
      error = function(e) {
        NA
      }
    )
  )
  fc$up95 <- map_dbl(
    fc$units,
    ~ tryCatch(
      {
        hilo(.x, 95)$upper
      },
      error = function(e) {
        NA
      }
    )
  )

  return(fc |> as_tsibble() |> select(-units) |> rename(p_units = ".mean"))
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

batch_size <- 16

# TRAIN/TEST FORECAST
# Load models
fit <- readRDS("model/fit_train.rds")

# Forecast
cat("++ TRAIN/TEST DATASET ++\n")
cat("-- FORECASTING --\n")
batchs <- seq(1, nrow(fit), by = batch_size)

get_forecast(batchs, test) |>
  add_ci() |>
  write_parquet("shiny_app/fc_train.parquet")

# Remove objects to free memory
rm(fit)

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# WHOLE DATA FORECAST
# Load model
fit_model <- readRDS("model/fit.rds")

# Forecast
cat("++ WHOLE DATA ++\n")
cat("-- FORECASTING --\n")
cat("- STL -\n")
# Forecast STL decomposition model with ETS errors
fit <- fit_model |> select(-arima)
batchs <- seq(1, nrow(fit), by = batch_size)
fc_stl <- get_forecast(batchs, new_data(transactions_final, n = 2))

fc_stl |>
  add_ci() |>
  write_parquet("shiny_app/fc_stl.parquet")

# Remove objects to free memory
rm(fit, fc_stl)

# Forecast for Dinamyc Regression Model with ARIMA errors.
# Because this model needs regressors, we will forecast in all possible combination of
# regressors (feature, display, tpr_only).
combinations <- list(
  list(
    feature = 0,
    display = 0,
    tpr_only = 0
  ),
  list(
    feature = 1,
    display = 0,
    tpr_only = 0
  ),
  list(
    feature = 0,
    display = 1,
    tpr_only = 0
  ),
  list(
    feature = 1,
    display = 1,
    tpr_only = 0
  ),
  list(
    feature = 0,
    display = 0,
    tpr_only = 1
  )
)

cat("- ARIMA -\n")
fit <- fit_model |> select(-stl)
batchs <- seq(1, nrow(fit), by = batch_size)
fc_arima <- map_dfr(
  seq_along(combinations), function(i) {
    cat(
      "Feature = ", combinations[[i]]$feature,
      "| Display = ", combinations[[i]]$display,
      "| TPR = ", combinations[[i]]$tpr_only,
      "\n"
    )
    ds <- new_data(transactions_final, n = 2) |>
      mutate(!!!combinations[[i]])
    fc <- get_forecast(batchs, ds) |>
      mutate(fc = i)

    # Save checkpoint just in case
    # saveRDS(fc, glue("shiny_app/fc_arima_{i}.rds"))

    # Return as tibble to avoid problems binding later
    return(as_tibble(fc))
  }
)

# Save forecasts
fc_arima |>
  as_tsibble(index = week, key = c(upc_id, store_id, .model, fc)) |>
  add_ci() |>
  write_parquet("shiny_app/fc_arima.parquet")
