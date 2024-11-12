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

## LOAD DATA
sample_train <- read_parquet("data/sample_train.parquet")
sample_val <- read_parquet("data/sample_val.parquet")

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

## MODEL DEFINITION
# I will focus on Dinamyc Regression Models with ARIMA errors and their variations
# with predictors `feature`, `display` and `tpr_only`.
# Also, I will try with STL decomposition model with ETS errors, Neural Network (NNetAR)
# and Prophet model.
# I transform units to square root to avoid negative values.
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

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## TRAIN MODEL
cat("++ TRAINING ++")
fit <- sample_train |>
  model(
    !!!models
  )

# I will select one of the ARIMA models which minimizes AICc in the training set.
val_aicc <- fit |>
  select(contains("arima")) |>
  glance() |>
  select(upc_id, store_id, .model, AICc) |>
  pivot_wider(
    names_from = .model,
    values_from = AICc
  )

# Create a column called `best_model` with the arima model which minimizes AICc, then count them
val_aicc %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    best_model = names(.)[2 + which.min(
      c_across(
        c(
          "arima_base",
          "arima_lagged",
          "arima_seasonal",
          "arima_seasonal_lagged"
        )
      )
    )]
  ) |>
  count(best_model) |>
  ggplot() +
  geom_bar(
    aes(best_model, n, fill = best_model),
    stat = "identity",
    show.legend = FALSE
  )

# Mean AICc across models
val_aicc |>
  summarise(
    across(
      c(
        "arima_base",
        "arima_lagged",
        "arima_seasonal",
        "arima_seasonal_lagged"
      ),
      \(x) mean(x, na.rm = TRUE)
    )
  )

# The best model among arima variations is the lagged version of ARIMA
fit <- fit |>
  select(-arima_base, -arima_seasonal, -arima_seasonal_lagged)

# Check for null models
fit %>%
  pivot_longer(
    cols = c("arima_lagged", "stl", "nnetar", "prophet"),
    names_to = "name_model",
    values_to = "model"
  ) |>
  filter(is_null_model(model)) |>
  group_by(name_model) |>
  summarize(nulls_percent = n() / nrow(fit) * 100)

# Prophet model has more than 60% of null models so I will remove it from the mable.
fit <- fit %>% select(-prophet)

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## FORECAST VALIDATION SET
# Forecast in validation set to compare the performance among models.
# Because NNETAR is very slow at predicting, I will remove it from the mable.
# Anyways, NNETAR is not performing well in the validation set due to lack of historic data.
cat("++ FORECASTING ++")

fit <- fit %>% select(-nnetar)

# Set up batchs
batch_size <- 8
batchs <- seq(1, nrow(fit), by = batch_size)

# Forecast
get_forecast <- function(batchs, ds) {
  p <- progressor(along = batchs)

  map_dfr(
    batchs,
    function(x) {
      end <- min(x + batch_size - 1, nrow(fit))
      p(sprintf("%s/%s", end, nrow(fit)))
      forecast(fit[x:end, ], ds)
    }
  )
}

fc <- get_forecast(batchs, sample_val) |>
  as_tsibble()

# Calculate RMSE
rmse_fc <- fc |>
  left_join(
    sample_val,
    by = c("upc_id", "store_id", "week"),
    suffix = c("", "")
  ) |>
  as_tibble() |>
  group_by(upc_id, store_id, .model) |>
  summarise(
    RMSE = sqrt(mean((units - .mean)^2))
  ) |>
  pivot_wider(
    names_from = .model,
    values_from = RMSE
  )

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## MODEL COMPARISON
rmse_fc %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    best_model = names(.)[2 + which.min(
      c_across(c("arima_lagged", "stl"))
    )]
  ) |>
  count(best_model) |>
  ggplot(
    aes(reorder(best_model, n, decreasing = TRUE), n)
  ) +
  geom_bar(stat = "identity") +
  labs(x = NULL, title = "Models performance in validation set")

# In order to select a model, I will get the median of RMSE to compare models.
# The median is used instead of the mean because the median is more robust
# to outliers.
rmse_fc |>
  ungroup() |>
  summarise(
    across(
      c("arima_lagged", "stl"),
      \(x) median(x, na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    cols = c("arima_lagged", "stl"),
    names_to = ".model",
    values_to = "RMSE"
  ) |>
  ggplot(aes(.model, RMSE)) +
  geom_bar(stat = "identity") +
  labs(
    x = NULL,
    y = "meadian(RMSE)",
    title = "Average RMSE across TS in validation set"
  )

# Based on RMSE, ARIMA lagged model with predictors performs better
# than STL decomposition model with ETS errors in the majority of the validation set.
# Anyway, both perform well in the validation set, so I will consider apply both models.
