#!/usr/bin/env Rscript

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
library(here)

#' Data loading
#' Load raw data from Excel and clean their column names
#' @returns A `list` of dataframes.
load_data <- function() {
  return(list(
    store = read_parquet(here("data", "processed", "store.parquet")),
    products = read_parquet(here("data", "processed", "products.parquet")),
    transactions = read_parquet(here("data", "processed", "transactions.parquet"))
  ))
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Summaries
#'
#' Prints a summary of the data
#' @param data A `list` containing the data.
#' @returns NULL
summaries <- function(data) {
  message("\n\nData Summaries\n")
  # Create skim dataframes
  print(transactions_skim <- skim(data$transactions[-1]))
  print(store_skim <- skim(data$store))
  print(products_skim <- skim(data$products))
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Outliers
#'
#' Print total number of outliers in transactions.
#' @param transactions A `tsibble` containing the transactions dataset.
#' @returns A `tibble` containing the outliers.
outliers <- function(transactions) {
  is_outlier <- function(x) {
    return(
      (x < quantile(x, 0.25, na.rm = TRUE) - 3 * IQR(x, na.rm = TRUE)) |
        (x > quantile(x, 0.75, na.rm = TRUE) + 3 * IQR(x, na.rm = TRUE))
    )
  }

  message("\n\nOutliers\n")
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
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Price range
#'
#' Print the range of prices offered on products.
#' @param transactions A `tsibble` containing the transactions data.
#' @returns A `skim` summary.
prices_range <- function(transactions) {
  message("\n\nWhat is the range of prices offered on products?\n\n")
  # Skim summary custom function
  price_summary <- skim_with(
    numeric = sfl(
      min = ~ min(., na.rm = TRUE),
      max = ~ max(., na.rm = TRUE),
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE)
    ),
    append = FALSE
  )

  prices_range <- transactions |>
    as_tibble() |>
    select(upc_id, price) |>
    group_by(upc_id) |>
    price_summary()
  return(prices_range)
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Product overview
#'
#' Helper functions to make visualizations
#' @param ts A `tibble` containing the transactions data.
#' @param selected_cat A `character` containing the product category to be selected.
#' @returns A `ggplotly` object.
product_overview <- function(ts, selected_cat) {
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
    labs(title = glue("Product: {selected_cat}")) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  print(ggplotly(plt))
}

#' Product correlation
#'
#' Plot a pairplot to see if there is any correlation among the features.
#' @param ts A `tibble` containing the transactions data.
#' @param selected_cat A `character` containing the product category to be selected.
#' @returns A `ggpairs` object.
product_corr <- function(ts, selected_cat) {
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
    labs(title = glue("Product: {selected_cat}"))
  print(corr)
}

#' Sales impact
#'
#' Helper functions to make visualizations
#' @param data A `list` containing the data.
#' @returns A `tibble` of summarized transactions data.
sales_impact <- function(data) {
  message("\n\nWhat is the impact on sales of promotions, displays, or being featured in the circular?\n\n")

  # I will aggregate the data by product category, in order to
  # compare promotions, displays and being featured against sales.
  # Aggregation will lost information but the idea is to obtain an overview.
  ts <- data$transactions |>
    left_join(data$products, by = join_by(upc_id)) |>
    as_tibble() |>
    group_by(week, category) |>
    summarise(
      .groups = "drop",
      units = sum(units, na.rm = TRUE),
      visits = sum(visits, na.rm = TRUE),
      base_price = mean(base_price, na.rm = TRUE),
      feature = ceiling(mean(feature, na.rm = TRUE)),
      display = ceiling(mean(display, na.rm = TRUE)),
      tpr_only = ceiling(mean(tpr_only, na.rm = TRUE))
    )

  prod_categories <- data$products$category
  for (prod_cat in unique(prod_categories)) {
    product_overview(ts, prod_cat)
    product_corr(ts, prod_cat)
  }
  return(ts)
}

# Observing the plots, we can see that exist correlation between units and promotions.
# This is expected, since promotions are done to increase sales. But, some promoted products
# don't show increase in sales. Further analysis is recommended for those products.
# Also, is visible that units is slightly correlated with base_price, since
# base_price represents the baseline price of the final price of the product.

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Units/Visits promotion
#'
#' Helper functions to make visualizations.
#' @param ts A `tibble` of the transactions data.
#' @param selected_cat A `character` containing the product category to be selected.
#' @returns A `ggplotly` object.
units_visits_prom <- function(ts, selected_cat) {
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
    labs(title = glue("Product: {selected_cat}")) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  print(ggplotly(plt))
}

#' Promotions impact
#'
#' @param ts A `tibble` of the transactions data.
#' @param products A `tibble` of the products data.
#' @returns NULL
promotions_impact <- function(ts, products) {
  message("\n\nWhat is the impact on units/visits of promotions?\n\n")
  for (category in unique(products$category)) {
    units_visits_prom(ts, category)
  }
}

# In aggregated TS there isn't visible too much correlation among the
# units/visits and promotions because aggregated products are very noisy
# in promotional features.
# Further analysis along each TS individually (disaggregated) is recommended.

#' Analysis
#'
#' Main analysis function
#' @returns NULL
analysis <- function() {
  message("\n\n========================= ANALYSIS =========================\n\n")
  data <- load_data()
  message("Data from 'data/processed' loaded successfully.")
  summaries(data)
  print(outliers(data$transactions))
  print(prices_range(data$transactions))
  ts <- sales_impact(data)
  promotions_impact(ts, data$products)
}

## TRIGGER ANALYSIS
if (sys.nframe() == 0) {
  analysis()
}
