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


## LOAD DATA
transactions <- read_parquet("data/transactions.parquet")
products <- read_parquet("data/products.parquet")

# Set up progress bars
handlers(global = TRUE)
handlers(list(
  handler_progress(
    format   = ":spin :current/:total [:bar] :percent in :elapsedfull ETA: :eta",
    width    = 80,
    clear    = FALSE
  )
))

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## VALIDATION SET
# In order to validate the model, I will select at least one TS sample randomly for each product category and store.
# Also, because many TS are incomplete, I will select those with less 50 nulls (almost one year of data).
# If any TS has nulls, I will imput them with an ARIMA model. That's because some models require non-nulls.

# Add a column with the number of missing values.
transactions_na <- transactions |>
  fill_gaps() |>
  as_tibble() |>
  group_by(upc_id, store_id) |>
  mutate(n_na = sum(is.na(units))) |>
  ungroup() |>
  as_tsibble(index = week, key = c(store_id, upc_id))

# Visualization of missing values distribuiton in the whole dataset.
transactions_na |>
  distinct(upc_id, store_id, n_na) |>
  ggplot(aes(n_na)) +
  geom_histogram()

# Sample TS with less than 50 nulls.
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

# Nulls imputation with ARIMA because some models require non-nulls.
# Also, I impute feature, display and tpr_only with 0.
nulls_train <- samples_transactions |>
  filter(is.na(units)) |>
  as_tibble() |>
  distinct(upc_id, store_id, n_na) |>
  left_join(samples_transactions) |>
  as_tsibble(index = week, key = c("upc_id", "store_id"))

# Trasform units to square root to avoid negative values.
# I don't use regressors because in time gaps they aren't known.
model_imp <- nulls_train |>
  model(imp = ARIMA(sqrt(units)))


# I will impute with the model in those TS if the model is not null.
# Otherwise, I will impute them with the last value.
sample_transactions_imp <- model_imp |>
  filter(!is_null_model(imp)) |>
  interpolate(nulls_train)

samples_transactions <- samples_transactions |>
  left_join(sample_transactions_imp, by = c("upc_id", "store_id", "week")) |>
  mutate(units = if_else(is.na(units.x), units.y, units.x)) |>
  select(-units.x, -units.y, -n_na) |>
  group_by_key() |>
  tidyr::fill(units, .direction = "down") |>
  relocate(week, .before = 1) |>
  relocate(units, , .after = store_id)

# Because crossvalidation is time consuming, I will split them into train
# and validation sets. Last month is used for validation.
sample_train <- samples_transactions |>
  group_by(upc_id, store_id) |>
  filter(week <= yearweek(max(as.Date(week)) - months(1))) |>
  ungroup()

sample_val <- samples_transactions |>
  group_by(upc_id, store_id) |>
  filter(week > yearweek(max(as.Date(week)) - months(1))) |>
  ungroup()

## Save the data
write_parquet(sample_train, "data/sample_train.parquet")
write_parquet(sample_val, "data/sample_val.parquet")

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## TRANSACTIONS PREPROCESSING
transactions_final <- transactions |>
  group_by_key() |>
  filter(n() > 12) |> # conserve those TS with more than 12 observations (3 months at least)
  ungroup() |>
  fill_gaps() # convert implicit gaps as explicit

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## TEST SET
# To test the dataset, I will use the last month of available data.
train <- transactions_final |>
  group_by_key() |>
  slice_head(n = -4) |> # retrive all steps except the last 4
  ungroup()

test <- new_data(train, n = 4) |>
  left_join(transactions_final, by = join_by(week, upc_id, store_id))

# Save the data
write_parquet(transactions_final, "data/transactions_final.parquet")
write_parquet(train, "data/train.parquet")
write_parquet(test, "data/test.parquet")
