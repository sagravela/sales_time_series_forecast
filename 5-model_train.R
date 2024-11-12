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

# Multiprocess
library(future)
library(furrr)

# Miscelaneous
library(progressr)

## LOAD DATA
train <- read_parquet("data/train.parquet")
test <- read_parquet("data/test.parquet")
transactions_final <- read_parquet("data/transactions_final.parquet")

# Set up future plan
# plan(multisession(workers = 8))

# Set up progress bars
handlers(global = TRUE)
handlers(list(
  handler_progress(
    format   = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta",
    width    = 80,
    clear    = FALSE
  )
))

## MODEL
model_set <- list(
  arima_def = ARIMA(
    sqrt(units)
  ),
  arima_lagged = ARIMA(
    sqrt(units) ~ feature + display + tpr_only +
      lag(feature) + lag(display) + lag(tpr_only)
  )
)

model_imp_set <- list(
  stl = decomposition_model(
    STL(sqrt(units)),
    ETS(season_adjust ~ season("N"))
  )
)

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## PIPELINES
# TRAIN
train_arima <- function(ds) {
  cat("- ARIMA training -\n")

  # Fit ARIMA model. I will train a default model because some TS arise errors with regressors
  fit <- ds |>
    model(
      !!!model_set
    )
  cat("\n", paste(rep("-", 70), collapse = ""), "\n")

  fit |>
    mutate(arima = if_else(
      is_null_model(arima_lagged), arima_def, arima_lagged
    )) |>
    select(-arima_def, -arima_lagged) |>
    filter(!is_null_model(arima))
}

# Imputation with ARIMA
impute <- function(fit, ds) {
  cat("- Imputation -\n")
  cat(" This takes a while... \n")
  tic("Elapsed time")
  ds_imp <- fit |>
    interpolate(ds |> semi_join(fit, by = c("store_id", "upc_id")))
  toc()

  return(ds_imp)
}

train_stl <- function(ds) {
  # Add STL decomposition model with ETS errors
  cat("- STL training -\n")
  ds |>
    model(
      !!!model_imp_set
    )
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## TRAIN DATASET
# Train in train set
cat("++ TRAIN/TEST DATASET ++\n")
cat("-- TRAINING --\n")
fit_arima <- train_arima(train)
train_imp <- impute(fit_arima, train)
fit_stl <- train_stl(train_imp) # ARIMA for imputation

# Join both models in one mable
fit <- bind_cols(fit_arima, fit_stl["stl"])

# Add CI and save the forecast as parquet
saveRDS(fit, "model/fit_train.rds")


# Remove objects to free memory
rm(fit_arima, train_imp, fit_stl, fit)
cat("\n", paste(rep("-", 70), collapse = ""), "\n")

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## TRAIN IN WHOLE DATA
cat("++ WHOLE DATA ++\n")
cat("-- TRAINING --\n")
fit_arima <- train_arima(transactions_final)
train_imp <- impute(fit_arima, transactions_final)
fit_stl <- train_stl(train_imp)

# Join both models in one mable
fit <- bind_cols(fit_arima, fit_stl["stl"])

# Save mable
saveRDS(fit, "model/fit.rds")
cat("\n", paste(rep("-", 70), collapse = ""), "\n")

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

# RESIDUALS
fit |>
  augment() |>
  write_parquet("shiny_app/residuals.parquet")

# Close multisession
plan("sequential")
