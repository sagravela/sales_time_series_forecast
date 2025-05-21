## MODELS DEFINITION
# TS model
library(fable)
# Install fable.prophet from CRAN with install.packages("fable.prophet"),
# it doesn't available in Conda Environment at the moment
library(fable.prophet)
library(fabletools)
library(feasts)

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
