#!/usr/bin/env Rscript

### MODEL PREPROCESSING

## DEPENDENCIES
# General
library(tidyverse)
library(tsibble)

# TS model
library(fable)
library(fabletools)

# Load and save data
library(arrow)

# Visualization
library(ggplot2)

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

#' Data loading
#'
#' Load raw data from Excel and clean their column names
#' @returns A `list` of dataframes.
load_data <- function() {
  return(list(
    products = read_parquet(here("data", "processed", "products.parquet")),
    transactions = read_parquet(here("data", "processed", "transactions.parquet"))
  ))
}

## VALIDATION SET
# In order to validate the model, I will select at least one TS sample randomly for each product category and store.
# Also, because many TS are incomplete, I will select those with less 50 nulls (almost one year of data).
# If any TS has nulls, I will imput them with an ARIMA model. That's because some models require non-nulls.

#' Missing transactions
#'
#' Aggregate missing values accross all stores and products.
#' @param transactions A `tsibble` containing the transactions data.
#' @returns A `tsibble` with the number of missing transactions by store and product.
missing_transactions <- function(transactions) {
  # Add a column with the number of missing values.
  transactions_na <- transactions |>
    fill_gaps() |>
    as_tibble() |>
    group_by(upc_id, store_id) |>
    mutate(n_na = sum(is.na(units))) |>
    ungroup() |>
    as_tsibble(index = week, key = c(store_id, upc_id))
  message("Missing values aggregated successfully.")
  return(transactions_na)
}

#' Sample TS
#'
#' Sample TS with less than 50 nulls.
#' @param transactions_na A `tsibble` with the number of missing transactions by store and product.
#' @param products A `tibble` containing the products data.
#' @returns A `tsibble` with the sampled TS.
sample_ts <- function(transactions_na, products) {
  # Set seed for reproducibility
  set.seed(42)
  ts_samples <- transactions_na |>
    as_tibble() |>
    left_join(products[c("upc_id", "category")], by = "upc_id") |>
    group_by(store_id, upc_id) |>
    mutate(missingness = n_na / n()) |>
    ungroup() |>
    group_by(store_id, category) |>
    filter(missingness < 0.25) |>
    distinct(upc_id, store_id, category, missingness) |>
    slice_sample(n = 1) |>
    ungroup()

  samples_transactions <- ts_samples |>
    inner_join(
      transactions_na,
      by = c("store_id", "upc_id"),
      suffix = c("", "")
    ) |>
    as_tsibble(index = week, key = c(store_id, upc_id))
  message("TS sampled successfully.")
  return(samples_transactions)
}

#' Imputation
#'
#' @description
#' Nulls imputation with ARIMA because some models require non-nulls.
#' Also, I impute feature, display and tpr_only with 0.
#' @param samples_transactions A `tsibble` with sampled time series.
#' @returns A `tsibble` with the imputed time series.
impute_ts <- function(samples_transactions) {
  nulls_train <- samples_transactions |>
    filter(is.na(units)) |>
    as_tibble() |>
    distinct(upc_id, store_id, n_na) |>
    left_join(samples_transactions, by = c("store_id", "upc_id")) |>
    as_tsibble(index = week, key = c(store_id, upc_id))

  # Trasform units to square root to avoid negative values.
  # I don't use regressors because in time gaps they aren't known.
  message("Imputing with ARIMA...")
  model_imp <- with_progress(
    model(nulls_train, imp = ARIMA(sqrt(units))),
    enable = TRUE
  )

  # I will impute with the model in those TS if the model is not null.
  # Otherwise, I will impute them with the last value.
  sample_transactions_imp <- model_imp |>
    filter(!is_null_model(imp)) |>
    interpolate(nulls_train)

  samples_transactions <- samples_transactions |>
    left_join(sample_transactions_imp, by = c("week", "store_id", "upc_id")) |>
    mutate(units = if_else(is.na(units.x), units.y, units.x)) |>
    select(-units.x, -units.y, -n_na) |>
    group_by_key() |>
    tidyr::fill(units, .direction = "down") |>
    relocate(week, .before = 1) |>
    relocate(units, , .after = store_id)
  message("Nulls imputed successfully.")
  return(samples_transactions)
}

#' Split sampled dataset
#'
#' @description
#' Because crossvalidation is time consuming, split them into train
#' and validation sets. Last month is used for validation.
#' @param samples_transactions A `tsibble` with sampled time series.
#' @returns A `list` with sampled train and validation sets.
split_sample_dataset <- function(samples_transactions) {
  sample_train <- samples_transactions |>
    group_by(store_id, upc_id) |>
    filter(week <= yearweek(max(as.Date(week)) - months(1))) |>
    ungroup()

  sample_val <- samples_transactions |>
    group_by(store_id, upc_id) |>
    filter(week > yearweek(max(as.Date(week)) - months(1))) |>
    ungroup()
  message("Sampled TS splitted successfully.")
  return(list(sample_train = sample_train, sample_val = sample_val))
}

#' Filter TS
#'
#' @description
#' Conserve those TS with more than 12 observations (3 months at least). Also,
#' fill implicit gaps with explicit ones.
#' @param transactions A `tsibble` containing the transactions data.
#' @returns A `tsibble` with the filtered time series.
filter_ts <- function(transactions) {
  message("Filtering TS...")
  return(
    transactions |>
      group_by_key() |>
      filter(n() > 12) |>
      ungroup() |>
      fill_gaps() # convert implicit gaps as explicit
  )
}

#' Split full dataset
#'
#' Split the dataset into train and test sets.
#' @param transactions A `tsibble` containing the transactions data.
#' @returns A `list` with train and test sets.
split_dataset <- function(transactions) {
  # To test the dataset, I will use the last month of available data.
  train <- transactions |>
    group_by_key() |>
    slice_head(n = -4) |> # retrive all steps except the last 4
    ungroup()

  test <- new_data(train, n = 4) |>
    left_join(transactions, by = join_by(week, store_id, upc_id))
  message("Full TS splitted successfully.")
  return(list(train = train, test = test))
}

#' Model data processing
#'
#' @returns NULL
model_data_processing <- function() {
  message("\n\n========================= MODEL DATA PROCESSING =========================\n\n")
  data <- load_data()
  message("Data from 'data/processed' loaded successfully.")
  sampled_data <- missing_transactions(data$transactions) |>
    sample_ts(data$products) |>
    impute_ts() |>
    split_sample_dataset()

  train_data <- filter_ts(data$transactions) |>
    split_dataset()
  full_data <- bind_rows(train_data$train, train_data$test)
  ## Save the data
  write_parquet(sampled_data$sample_train, here("data", "processed", "sample_train.parquet"))
  write_parquet(sampled_data$sample_val, here("data", "processed", "sample_val.parquet"))
  write_parquet(train_data$train, here("data", "processed", "train.parquet"))
  write_parquet(train_data$test, here("data", "processed", "test.parquet"))
  write_parquet(full_data, here("data", "processed", "transactions_final.parquet"))
  message("Data written successfully to: data/processed")
}

## TRIGGER VALIDATION
if (sys.nframe() == 0) {
  model_data_processing()
}
