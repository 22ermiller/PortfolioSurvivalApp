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
n_sims <- 1000

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
eci_sims <- eci_multiple_sims(
  eci_mod,
  n_years * 4,
  n_sims,
  cpi_sims,
  final_cpi_val
)

med_sims <- med_multiple_sims(med_mod, n_years * 12, n_sims)

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

equity_rs_mod <- fit_equity_rs_model(equity_mean_mod_resids)
equity_sims <- equity_multiple_sims(
  equity_mean_mod,
  equity_rs_mod,
  n_years * 12,
  n_sims,
  cpi_sims,
  ir3mo_sims
)

annuity_prices <- price_annuities_fast(
  start_age = 60,
  yield_curve_sims,
  mortality_tbl,
  1,
  .1,
  n_sims
)


# Run Simulation ---------------------------------------------------------

portfolio_sim_vec <- function(
  annuity_prop,
  withdrawal_rate,
  annuity_prices,
  equity_sims,
  cpi_sims,
  med_sims,
  death_ages
) {
  n_sims <- nrow(equity_sims)
  T <- ncol(equity_sims)

  # Convert log returns to gross returns
  equity_gross <- exp(equity_sims)

  # Price level from CPI simulations
  matrix_cpi_sims <- do.call(rbind, cpi_sims)
  price_level <- t(apply(matrix_cpi_sims, 1, function(x) cumprod(exp(x))))
  matrix_med_sims <- do.call(rbind, med_sims)
  med_price_level <- t(apply(matrix_med_sims, 1, function(x) cumprod(exp(x))))

  # Starting portfolio
  starting_value <- 100
  annuity_amt <- starting_value * annuity_prop
  stock_market_amt <- starting_value - annuity_amt
  monthly_annuity_payout <- rep(annuity_amt, n_sims) / annuity_prices
  withdrawal_amt <- rep(starting_value, n_sims) * (withdrawal_rate / 12)

  # Initialize portfolio matrix
  portfolio_value <- matrix(NA, nrow = n_sims, ncol = T)

  # Period 1
  portfolio_value[, 1] <- (stock_market_amt -
    ((price_level[, 1] * withdrawal_amt * .82) +
      (med_price_level[, 1] * withdrawal_amt * .18)) +
    monthly_annuity_payout) *
    equity_gross[, 1]

  # Remaining periods
  for (t in 2:T) {
    portfolio_value[, t] <- (portfolio_value[, t - 1] -
      ((price_level[, t] * withdrawal_amt * .82) +
        (med_price_level[, t] * withdrawal_amt * .18)) +
      monthly_annuity_payout) *
      equity_gross[, t]
  }

  # Never allow negative values
  portfolio_value <- pmax(portfolio_value, 0)

  # First month portfolio hits 0
  failure_month_vec <- apply(portfolio_value, 1, function(x) {
    m <- which(x == 0)[1]
    if (is.na(m)) NA else m
  })

  # Death month (relative to retirement at 60)
  death_month_vec <- ifelse(death_ages > 60 * 12, death_ages - 60 * 12, 0)

  # Clamp death months to max simulation length
  death_month_vec <- pmin(death_month_vec, ncol(portfolio_value))
  death_month_vec[death_month_vec == 0] <- 1 # pre-60 deaths → use first month

  # Value at death (vectorized)
  value_at_death <- portfolio_value[cbind(1:n_sims, death_month_vec)]

  # Success indicators
  success <- (death_month_vec <= failure_month_vec) | is.na(failure_month_vec)
  success_no_death <- portfolio_value[, ncol(portfolio_value)] > 0

  # End-of-life expected value ignoring death
  expected_value_no_death <- mean(portfolio_value[, -1])

  # Success rates
  success_rate <- mean(success)
  success_rate_no_death <- mean(success_no_death)
  expected_value_at_death <- mean(value_at_death)
  value_at_death_ci <- quantile(value_at_death, probs = c(.05, .5, .95))
  value_no_death_ci <- quantile(portfolio_value[, -1], probs = c(.05, .5, .95))

  return(list(
    success = success_rate,
    success_no_death = success_rate_no_death,
    expected_value_at_death = expected_value_at_death,
    expected_value_no_death = expected_value_no_death,
    value_at_death_ci = value_at_death_ci,
    value_no_death_ci = value_no_death_ci
  ))
}


death_ages <- sample(
  mortality_tbl$month,
  size = n_sims,
  replace = TRUE,
  prob = mortality_tbl$death_pdf
)

df <- expand_grid(
  withdrawal_rate = seq(.01, .2, by = .001),
  annuity_prop = seq(0, 1, by = .1) # Adjust range and step as needed
)

with_progress({
  p <- progressor(along = 1:nrow(df))

  df <- df %>%
    mutate(
      success_rates = pmap(
        list(annuity_prop, withdrawal_rate),
        ~ {
          p()
          portfolio_sim_vec(
            annuity_prop = ..1,
            withdrawal_rate = ..2,
            prices,
            equity_sims,
            cpi_sims,
            med_sims,
            death_ages
          )
        }
      )
    )
})

final_df <- df %>%
  mutate(
    success_rate = map_dbl(success_rates, "success"),
    success_no_death = map_dbl(success_rates, "success_no_death"),
    mean_value_at_death = map_dbl(success_rates, "expected_value_at_death"),
    mean_value_no_death = map_dbl(success_rates, "expected_value_no_death"),
    value_at_death_p05 = map_dbl(success_rates, ~ .x$value_at_death_ci[["5%"]]),
    value_at_death_p50 = map_dbl(
      success_rates,
      ~ .x$value_at_death_ci[["50%"]]
    ),
    value_at_death_p95 = map_dbl(
      success_rates,
      ~ .x$value_at_death_ci[["95%"]]
    ),
    value_no_death_p05 = map_dbl(success_rates, ~ .x$value_no_death_ci[["5%"]]),
    value_no_death_p50 = map_dbl(
      success_rates,
      ~ .x$value_no_death_ci[["50%"]]
    ),
    value_no_death_p95 = map_dbl(success_rates, ~ .x$value_no_death_ci[["95%"]])
  )

saveRDS(final_df, "imports/portfolio_sims.rds")
