#!/usr/bin/env Rscript

### DATA LOAD AND PREPARATION

## DEPENDENCIES
# General
library(tidyverse)
library(tsibble)

# Load and save data
library(arrow)
library(readxl)

# Miscelaneous
library(janitor)
library(measurements)
library(here)


#' Data loading
#' Load raw data from Excel and clean their column names
#' @returns A `list` of dataframes.
load_raw_data <- function() {
  raw_data_path <- here("data", "raw", "dunnhumby - Breakfast at the Frat.xlsx")
  store_lookup <- read_excel(
    raw_data_path,
    sheet = "dh Store Lookup", skip = 1
  ) |>
    clean_names()
  products_lookup <- read_excel(
    raw_data_path,
    sheet = "dh Products Lookup", skip = 1
  ) |>
    clean_names()
  transactions <- read_excel(
    raw_data_path,
    sheet = "dh Transaction Data", skip = 1
  ) |>
    clean_names()

  products_lookup <- products_lookup |>
    rename(upc_id = "upc")

  transactions <- transactions |>
    rename(upc_id = "upc", store_id = "store_num")
  message("Data from 'data/raw' loaded successfully.")
  return(list(store = store_lookup, products = products_lookup, transactions = transactions))
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Remove duplicated stores
#' @param store A `tibble` containing the store data.
#' @returns A `tibble` containing the cleaned store data.
store_duplicates <- function(store) {
  # Handle repeated stores
  repeated_stores <- store |>
    group_by(store_id) |>
    summarize(n = n()) |>
    filter(n > 1) |>
    left_join(store, by = join_by(store_id))

  # Create new category for `seg_value_name` to remove duplicated rows
  return(
    store |>
      mutate(
        seg_value_name = ifelse(
          store_id %in% repeated_stores$store_id,
          "MAINSTREAM_UPSCALE",
          seg_value_name
        )
      ) |>
      distinct() |>
      mutate(store_id = as.character(store_id))
  )
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Format transactions
#'
#' @param transactions A `tibble` containing the transactions.
#' @return A `tsibble` dataset with the formatted transactions.
format_transactions <- function(transactions) {
  transactions_formatted <- transactions |>
    mutate(
      week = yearweek(week_end_date),
      upc_id = as.character(upc_id),
      store_id = as.character(store_id)
    ) |>
    as_tsibble(index = week, key = c(store_id, upc_id)) |>
    relocate(week)
  return(transactions_formatted)
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Convert product size to volume (ml) and mass (oz)
#' @param x A `character` containing the product size.
#' @returns A `number` of the converted product size.
converter <- function(x) {
  split_values <- unlist(strsplit(x, " "))
  unit <- tolower(split_values[2])
  result <- conv_unit(
    as.numeric(split_values[1]),
    ifelse(unit == "lt", "l",
      ifelse(unit == "ct", "carat", unit)
    ),
    to = ifelse(unit %in% c("lt", "ml"), "ml", "oz")
  )
  return(result)
}

#' Convert product size to volume (ml) and mass (oz)
#' @param products A `tibble` containing the products data.
#' @returns A `tibble` containing the cleaned products data.
products_conversion <- function(products) {
  return(
    products |>
      mutate(
        upc_id = as.character(upc_id),
        volume_ml = ifelse(
          grepl("LT|ML", product_size),
          sapply(product_size, converter), NA
        ),
        mass_oz = ifelse(
          grepl("OZ|CT", product_size),
          sapply(product_size, converter), NA
        )
      )
  )
}

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

#' Clean and save data
#' @returns Cleaned data saved.
clean_data <- function() {
  message("\n\n========================= DATA CLEANING =========================\n\n")
  data <- load_raw_data()
  data$store <- store_duplicates(data$store)
  data$transactions <- format_transactions(data$transactions)
  data$products <- products_conversion(data$products)
  message("Data cleaned successfully.")
  # Create the processed folder to save outputs
  dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

  # Save it to Parquet
  write_parquet(data$store, here("data", "processed", "store.parquet"))
  write_parquet(data$products, here("data", "processed", "products.parquet"))
  write_parquet(data$transactions, here("data", "processed", "transactions.parquet"))
  message("Data written successfully to: data/processed")
}

## TRIGGER CLEANING PROCESS
if (sys.nframe() == 0) {
  clean_data()
}
