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

n_years <- 80
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


# Set up duckdb database -------------------------------------------------

library(duckdb)

con <- dbConnect(duckdb(), dbdir = "esg-db.duckdb", read_only = FALSE)

# Simulations -------------------------------------------------------------

## CPI ##
cpi_sims <- cpi_multiple_sims(cpi_mod, n_years * 12, n_sims)
cpi_sims_df <- as_tibble(do.call(rbind, cpi_sims))
cpi_long <- cpi_sims_df %>%
  mutate(sim_id = row_number()) %>%
  pivot_longer(
    cols = -sim_id,
    names_to = "month",
    values_to = "log_cpi"
  ) %>%
  mutate(
    month = as.integer(gsub("[^0-9]", "", month))
  ) %>%
  group_by(sim_id) %>%
  arrange(month) %>%
  mutate(price_level = cumprod(exp(log_cpi))) %>%
  ungroup()

# create cpi table
dbWriteTable(con, "cpi_sims", cpi_long, overwrite = TRUE)

## ECI ##
final_cpi_val <- get_last_quarterly_cpi_val(cpi_df)
eci_sims <- eci_multiple_sims(
  eci_mod,
  n_years,
  n_sims,
  cpi_sims,
  final_cpi_val
)
matrix_eci_sims <- do.call(rbind, eci_sims)
matrix_eci_sims <- t(apply(matrix_eci_sims, 1, function(x) cumprod(exp(x))))
matrix_eci_sims <- t(apply(matrix_eci_sims, 1, function(x) {
  n_years <- length(x)
  monthly <- numeric(n_years * 12)

  for (i in 1:(n_years - 1)) {
    # endpoints for this year
    y0 <- x[i]
    y1 <- x[i + 1]

    # 12 monthly points (including first month = y0)
    monthly_segment <- seq(y0, y1, length.out = 12)

    # fill into output
    monthly[((i - 1) * 12 + 1):(i * 12)] <- monthly_segment
  }

  # last year gets flat because no next point
  monthly[(n_years - 1) * 12 + 1] <- x[n_years]
  monthly[(n_years - 1) * 12 + 2:(12)] <- x[n_years]

  monthly
}))

eci_long <- as_tibble(matrix_eci_sims) %>%
  mutate(sim_id = row_number()) %>%
  pivot_longer(
    cols = -sim_id,
    names_to = "month",
    values_to = "wage_level"
  ) %>%
  mutate(
    month = as.integer(gsub("[^0-9]", "", month))
  )

# create eci table
dbWriteTable(con, "eci_sims", eci_long, overwrite = TRUE)

## MED CPI ##

med_sims <- med_multiple_sims(med_mod, n_years * 12, n_sims)
med_sims_df <- as_tibble(do.call(rbind, med_sims))
med_cpi_long <- med_sims_df %>%
  mutate(sim_id = row_number()) %>%
  pivot_longer(
    cols = -sim_id,
    names_to = "month",
    values_to = "log_mcpi"
  ) %>%
  mutate(
    month = as.integer(gsub("[^0-9]", "", month))
  ) %>%
  group_by(sim_id) %>%
  arrange(month) %>%
  mutate(med_price_level = cumprod(exp(log_mcpi))) %>%
  ungroup()

# create med_cpi table
dbWriteTable(con, "med_cpi_sims", med_cpi_long, overwrite = TRUE)

## 3MO INTEREST RATE ##

mean_3mo_ir <- get_average_3mo_rate(ir3mo_df)
ir3mo_sims <- ir3mo_multiple_sims(
  ir3mo_mod,
  n_years * 12,
  n_sims,
  cpi_sims,
  mean_3mo_ir
)

saveRDS(ir3mo_sims, "imports/ir3mo_sims.rds")

slope_curve_vals <- get_final_slope_curve_vals(full_ir_df)
yield_curve_sims <- yield_multiple_sims(
  yield_var_mod,
  yield_lms,
  n_years * 12,
  n_sims,
  ir3mo_sims,
  slope_curve_vals
)

# add simulation and month indeces
for (i in 1:length(yield_curve_sims)) {
  yield_curve_sims[[i]]$sim_id <- i
  yield_curve_sims[[i]]$month <- seq(1:nrow(yield_curve_sims[[i]]))
}

ir_df <- as_tibble(do.call(rbind, yield_curve_sims)) %>%
  select(sim_id, month, everything())


# create interest rate table
dbWriteTable(con, "interest_rate_sims", ir_df, overwrite = TRUE)


equity_rs_mod <- fit_equity_rs_model(equity_mean_mod_resids)
equity_sims <- equity_multiple_sims(
  equity_mean_mod,
  equity_rs_mod,
  n_years * 12,
  n_sims,
  cpi_sims,
  ir3mo_sims
)

equity_long <- as_tibble(equity_sims) %>%
  mutate(sim_id = row_number()) %>%
  pivot_longer(
    cols = -sim_id,
    names_to = "month",
    values_to = "log_returns"
  ) %>%
  mutate(
    month = as.integer(gsub("[^0-9]", "", month))
  ) %>%
  mutate(equity_gross = exp(log_returns))


# create equity_returns table
dbWriteTable(con, "equity_sims", equity_long, overwrite = TRUE)

annuity_df <- crossing(
  purchase_age = seq(60, 100),
  months_into_future = seq(1, 80 * 12, by = 12)
)

handlers(global = TRUE)
handlers("txtprogressbar")


with_progress({
  p <- progressor(along = seq_len(nrow(annuity_df)))

  annuity_df <- annuity_df %>%
    mutate(
      annuity_prices = pmap(
        list(purchase_age, months_into_future),
        function(purchase_age, months_into_future) {
          p()
          price_annuities(
            start_age = purchase_age,
            yield_curve_sims,
            mortality_tbl,
            purchase_date = months_into_future,
            payout = 1,
            load = .1,
            n_sims
          )
        }
      )
    )
})

annuity_long <- annuity_df %>%
  unnest_longer(annuity_prices, indices_to = "sim_id") %>%
  select(
    sim_id,
    purchase_age,
    months_into_future,
    annuity_price = annuity_prices
  )

# create annuity_prices table
dbWriteTable(con, "annuity_prices", annuity_long, overwrite = TRUE)

dbDisconnect(con)

annuity_prices <- price_annuities(
  start_age = 90,
  yield_curve_sims,
  mortality_tbl,
  1,
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
