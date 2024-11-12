### ANALYSIS

## DEPENDENCIES
# General
library(tidyverse)
library(tsibble)
library(skimr)

# Load and save data
library(arrow)

# Visualization
library(ggplot2)
library(plotly)
library(GGally)

# Miscelaneous
library(glue)


## LOAD DATA
transactions <- read_parquet("data/transactions.parquet")
store <- read_parquet("data/store.parquet")
products <- read_parquet("data/products.parquet")

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## SKIM SUMMARY
# Create skim dataframes
transactions_skim <- skim(transactions[-1])
store_skim <- skim(store)
products_skim <- skim(products)

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## OUTLIERS
is_outlier <- function(x) {
  return(
    (x < quantile(x, 0.25, na.rm = TRUE) - 3 * IQR(x, na.rm = TRUE)) |
      (x > quantile(x, 0.75, na.rm = TRUE) + 3 * IQR(x, na.rm = TRUE))
  )
}

outliers <- transactions |>
  as_tibble() |>
  summarise_at(
    c(
      outlier_units = "units",
      outlier_visits = "visits",
      outlier_hhs = "hhs",
      outlier_spend = "spend",
      outlier_price = "price",
      outlier_base_price = "base_price"
    ),
    is_outlier
  ) |>
  mutate(
    week = transactions$week,
    store_id = transactions$store_id,
    upc_id = transactions$upc_id,
    .before = 1
  )

# Number of outliers per feature
outliers |>
  select(contains("outlier")) |>
  summarise(across(everything(), sum))

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## There are 3 products not sampled in transactions
products |>
  filter(!upc_id %in% transactions$upc_id)

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## What is the range of prices offered on products?
# Skim summary custom function
price_summary <- skim_with(
  numeric = sfl(min, max, mean, sd),
  append = FALSE
)

prices_range <- transactions |>
  as_tibble() |>
  select(upc_id, price) |>
  group_by(upc_id) |>
  filter(upc_id == sample(upc_id, 1)) |>
  price_summary()

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## What is the impact on sales of promotions, displays, or being
## featured in the circular?

# I will aggregate the data by product category, in order to
# compare promotions, displays and being featured against sales.
# Aggregation will lost information but the idea is to obtain an overview.
ts <- transactions |>
  left_join(products, by = join_by(upc_id)) |>
  as_tibble() |>
  group_by(week, category) |>
  summarise(
    units = sum(units, na.rm = TRUE),
    visits = sum(visits, na.rm = TRUE),
    base_price = mean(base_price, na.rm = TRUE),
    feature = ceiling(mean(feature, na.rm = TRUE)),
    display = ceiling(mean(display, na.rm = TRUE)),
    tpr_only = ceiling(mean(tpr_only, na.rm = TRUE))
  ) |>
  ungroup()

product_overview <- function(selected_cat) {
  plt <- ts |>
    filter(category == selected_cat) |>
    pivot_longer(
      c(units, base_price, feature, display, tpr_only),
      names_to = "col",
      values_to = "value"
    ) |>
    ggplot(aes(x = week, y = value, color = col)) +
    geom_line() +
    facet_wrap(~col, scales = "free_y", ncol = 1) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = glue("Product: {category}")) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggplotly(plt)
}

# Also, I will plot a pairplot to see if there is any correlation among the features.
product_corr <- function(selected_cat) {
  corr <- ts |>
    filter(category == selected_cat) |>
    mutate(
      feature = ifelse(feature == 1, "True", "False"),
      display = ifelse(display == 1, "True", "False"),
      tpr_only = ifelse(tpr_only == 1, "True", "False")
    ) |>
    select(
      -category,
      -week
    ) |>
    ggpairs() +
    labs(title = glue("Product: {category}"))
}

for (category in unique(products$category)) {
  print(product_overview(category))
  print(product_corr(category))
}

# Observing the plots, we can see that exist correlation between units and promotions.
# This is expected, since promotions are done to increase sales. But, some promoted products
# don't show increase in sales. Further analysis is recommended for those products.
# Also, is visible that units is slightly correlated with base_price, since
# base_price represents the baseline price of the final price of the product.

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## What is the impact on units/visits of promotions?
units_visits_prom <- function(selected_cat) {
  plt <- ts |>
    filter(category == selected_cat) |>
    mutate(units_visits = units / visits) |>
    pivot_longer(
      c(units_visits, feature, display, tpr_only),
      names_to = "col",
      values_to = "value"
    ) |>
    ggplot(aes(x = week, y = value, color = col)) +
    geom_line() +
    facet_wrap(~col, scales = "free_y", ncol = 1) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = glue("Product: {category}")) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggplotly(plt)
}

for (category in unique(products$category)) {
  print(units_visits_prom(category))
}

# In aggregated TS there isn't visible too much correlation among the
# units/visits and promotions because aggregated products are very noisy
# in promotional features.
# Further analysis along each TS individually (disaggregated) is recommended.
