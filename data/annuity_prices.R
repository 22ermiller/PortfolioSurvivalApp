# Script to generate simulated draws from ESG

library(MSGARCH)
library(rugarch)
library(tidyverse)
library(forecast)
library(tsDyn)
library(progressr)

# Read in models

cpi_mod <- readRDS("data/models/cpi_mod.rds")
eci_mod <- readRDS("data/models/eci_mod.rds")
med_mod <- readRDS("data/models/med_mod.rds")
ir3mo_mod <- readRDS("data/models/interest_garch.rds")
yield_var_mod <- readRDS("data/models/yield_curve_var_mod.rds")
yield_lms <- readRDS("data/models/yield_curve_lm_mods.rds")
equity_mean_mod <- readRDS("data/models/equity_mean_mod.rds")
equity_mean_mod_resids <- readRDS("data/models/mean_mod_resids.rds")


# Read in functions
source("data/functions.R")

n_years <- 50
n_sims <- 10

# Load in Necessary Data --------------------------------------------------

cpi_df <- read_csv("data/data/cpi.csv") |>
  filter(date >= "2010-01-01") |> # only dates from 2010 onwards
  mutate(lagged_cpi = lag(cpi, n = 1)) |>
  mutate(log_dif_cpi = log(cpi) - log(lagged_cpi)) |>
  filter(!is.na(log_dif_cpi))

ir3mo_df <- read_csv("data/data/ir3mo.csv") |>
  mutate(
    lagged_rate = lag(rate, n = 1),
    rate_rmmean = rate - mean(rate, na.rm = TRUE)
  ) |>
  mutate(
    dif_rate = rate - lagged_rate,
    log_dif_rate = log(rate) - log(lagged_rate)
  )

full_ir_df <- read_csv("data/data/full_ir.csv") |>
  mutate(across(three_month:thirty_year, ~ . / 100)) |>
  mutate(
    slope = thirty_year - three_month,
    curve = three_month + thirty_year - (2 * ten_year)
  )

mortality_tbl <- read_csv("data/data/mortality.csv") |>
  filter(!is.na(death_pdf))

# Simulations -------------------------------------------------------------

cpi_sims <- cpi_multiple_sims(cpi_mod, n_years * 12, n_sims)

final_cpi_val <- get_last_quarterly_cpi_val(cpi_df)

mean_3mo_ir <- get_average_3mo_rate(ir3mo_df)
ir3mo_sims <- ir3mo_multiple_sims(
  ir3mo_mod,
  n_years * 12,
  n_sims,
  cpi_sims,
  mean_3mo_ir
)

slope_curve_vals <- get_final_slope_curve_vals(full_ir_df)
yield_curve_sims <- yield_multiple_sims(
  yield_var_mod,
  yield_lms,
  n_years * 12,
  n_sims,
  ir3mo_sims,
  slope_curve_vals
)

prices_test <- map_dbl(
  seq(1:960),
  ~ price_annuity(start_age = 60, yield_curve_sim, mortality_tbl, .x, 1, .2)
)

prices <- price_annuities(
  start_age = 60,
  yield_curve_sims,
  mortality_tbl,
  1,
  1,
  .1,
  10
)

with_progress({
  p <- progressor(along = 1:(n_years * 12))

  df <- data.frame(time = seq(1:(n_years * 12))) %>%
    mutate(
      prices = map(
        time,
        ~ {
          p()
          price_annuities(
            start_age = 60,
            yield_curve_sims,
            mortality_tbl,
            .x,
            1,
            .1,
            10
          )
        }
      )
    )
})

df[1, ]
df[2, ]
