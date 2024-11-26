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


## LOAD DATA
# Load raw data from Excel and clean their column names
store_lookup <- read_excel(
  "data/dunnhumby - Breakfast at the Frat.xlsx",
  sheet = "dh Store Lookup", skip = 1
) |>
  clean_names()
products_lookup <- read_excel(
  "data/dunnhumby - Breakfast at the Frat.xlsx",
  sheet = "dh Products Lookup", skip = 1
) |>
  clean_names()
transactions <- read_excel(
  "data/dunnhumby - Breakfast at the Frat.xlsx",
  sheet = "dh Transaction Data", skip = 1
) |>
  clean_names()

products_lookup <- products_lookup |>
  rename(upc_id = "upc")

transactions <- transactions |>
  rename(upc_id = "upc", store_id = "store_num")

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## DUPLICATES
# Handle repeated stores
repeated_stores <- store_lookup |>
  group_by(store_id) |>
  summarize(n = n()) |>
  filter(n > 1) |>
  left_join(store_lookup, by = join_by(store_id))

# Create new category for `seg_value_name` to remove duplicated rows
store_lookup <- store_lookup |>
  mutate(
    seg_value_name = ifelse(
      store_id %in% repeated_stores$store_id,
      "MAINSTREAM_UPSCALE",
      seg_value_name
    )
  ) |>
  distinct()

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## FORMAT DATA
# Format trnasactions to tsibble
transactions <- transactions |>
  mutate(week = yearweek(week_end_date)) |>
  as_tsibble(index = week, key = c(store_id, upc_id)) |>
  relocate(week)

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## UNITS CONVERSION
# Convert units in `product_szie` to volume (ml) and mass (oz)
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

products_lookup <- products_lookup |>
  mutate(
    volume_ml = ifelse(
      grepl("LT|ML", product_size),
      sapply(product_size, converter), NA
    ),
    mass_oz = ifelse(
      grepl("OZ|CT", product_size),
      sapply(product_size, converter), NA
    )
  )

# --------------------------------------------------------------------------- #
# --------------------------------------------------------------------------- #

## SAVE DATA
# Save it to Parquet
write_parquet(store_lookup, "data/store.parquet")
write_parquet(products_lookup, "data/products.parquet")
write_parquet(transactions, "data/transactions.parquet")
