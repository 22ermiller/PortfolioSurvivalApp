## All simulation functions for the ESG ##

# CPI ---------------------------------------------------------------------

cpi_single_sim <- function(model, sim_length) {
  simulate(model, nsim = sim_length)
}

cpi_multiple_sims <- function(model, sim_length, n_sims) {
  sims <- vector("list", n_sims)

  for (i in 1:n_sims) {
    sims[[i]] <- cpi_single_sim(model, sim_length)
  }

  return(sims)
}

# ECI ---------------------------------------------------------------------

# get last quarterly cpi value helper function

get_last_quarterly_cpi_val <- function(cpi_df) {
  quarterly_cpi <- cpi_df |>
    filter(month(date) %in% c(3, 6, 9, 12)) |>
    mutate(lagged_cpi = lag(cpi, n = 1)) |>
    mutate(
      log_dif_cpi = log(cpi) - log(lagged_cpi),
      lagged_cpi = lag(log_dif_cpi, n = 1)
    ) |>
    filter(!is.na(log_dif_cpi) & !is.na(lagged_cpi)) # remove first observation without lag

  last_val <- tail(quarterly_cpi$log_dif_cpi, n = 1)
  return(last_val)
}

eci_single_sim <- function(model, sim_length, cpi_sim, final_cpi_value) {
  # make cpi simulation quarterly
  quarterly_cpi_sim_df <- data.frame(
    date = zoo::as.Date(time(cpi_sim)),
    value = cpi_sim
  ) |>
    mutate(year = year(date), quarter = quarter(date)) |>
    group_by(year, quarter) |>
    summarize(date = max(date), log_dif_cpi = sum(value))

  lagged_cpi_sim <- c(
    final_cpi_value,
    head(quarterly_cpi_sim_df$log_dif_cpi, n = -1)
  )
  sim <- simulate(model, nsim = sim_length, xreg = lagged_cpi_sim)

  return(sim)
}

eci_multiple_sims <- function(
  model,
  sim_length,
  nsims,
  cpi_sims,
  final_cpi_value
) {
  sims <- vector("list", n_sims)

  for (i in 1:n_sims) {
    sims[[i]] <- eci_single_sim(
      model,
      sim_length,
      cpi_sims[[i]],
      final_cpi_value
    )
  }

  return(sims)
}

# Medical Inflation -------------------------------------------------------

med_single_sim <- function(model, sim_length) {
  stats::simulate(model, nsim = sim_length)
}

med_multiple_sims <- function(model, sim_length, n_sims) {
  sims <- vector("list", n_sims)

  for (i in 1:n_sims) {
    sims[[i]] <- med_single_sim(model, sim_length)
  }

  return(sims)
}


# Short Term Interest Rates -----------------------------------------------

get_average_3mo_rate <- function(interest_df) {
  r_bar <- .005

  model_df <- interest_df |>
    dplyr::select(date, dif_rate, rate) |>
    left_join(cpi_df, by = c("date" = "date")) |>
    mutate(rate = rate / 100) |>
    mutate(rate = ifelse(rate == 0, .0001, rate)) |> # can't have 0 for transformation
    mutate(
      transformed_rate = ifelse(
        rate > r_bar,
        rate,
        r_bar - r_bar * log(r_bar) + r_bar * log(rate)
      )
    ) |>
    mutate(
      transformed_dif = transformed_rate - lag(transformed_rate, n = 1, ),
      transformed_rmmean = transformed_rate -
        mean(transformed_rate, na.rm = TRUE)
    ) |>
    filter(!is.na(transformed_dif))

  return(mean(model_df$transformed_rate))
}

back_transform <- function(x) {
  r_bar <- 0.005
  ifelse(x > r_bar, x, exp((x - r_bar + r_bar * log(r_bar)) / r_bar))
}

ir3mo_single_sim <- function(model, sim_length, cpi_sim, mean_rate) {
  # cpi simulation
  cpi <- matrix(cpi_sim, ncol = 1)
  mexsim_list <- list(cpi)

  sim <- ugarchsim(
    model,
    n.sim = sim_length,
    startMethod = "sample",
    mexsimdata = mexsim_list
  )@simulation$seriesSim +
    mean_rate

  final_sim <- apply(as.matrix(sim), 1, back_transform)

  return(final_sim)
}

ir3mo_multiple_sims <- function(model, sim_length, nsims, cpi_sims, mean_rate) {
  sims <- matrix(NA, nrow = n_sims, ncol = sim_length)

  for (i in 1:n_sims) {
    sims[i, ] <- ir3mo_single_sim(model, sim_length, cpi_sims[[i]], mean_rate)
  }

  return(sims)
}


# Yield Curve -------------------------------------------------------------

get_final_slope_curve_vals <- function(ir_df) {
  list(slope = tail(ir_df$slope, 1), curve = tail(ir_df$curve, 1))
}

yield_single_sim <- function(
  var_mod,
  lm_mods,
  sim_length,
  ir3mo_sim,
  final_vals
) {
  r_bar <- 0.005

  transformed_3mo <- ifelse(
    ir3mo_sim > r_bar,
    ir3mo_sim,
    r_bar - r_bar * log(r_bar) + r_bar * log(ir3mo_sim)
  )

  sample_cov <- cov(var_mod$residuals)
  var_sim <- VAR.sim(
    var_mod$coefficients,
    n = sim_length,
    starting = matrix(c(final_vals$slope, final_vals$curve), nrow = 1),
    varcov = sample_cov,
    exogen = transformed_3mo
  )
  three_month <- transformed_3mo
  slope <- var_sim[, 1]
  curve <- var_sim[, 2]

  new_df <- data.frame(
    three_month = transformed_3mo,
    slope = slope,
    curve = curve
  )

  preds <- sapply(lm_mods, function(model) predict(model, newdata = new_df))

  # Add 10yr and 30yr yield (derived from slope and curve)
  ten_year <- transformed_3mo + (slope - curve) / 2
  thirty_year <- transformed_3mo + slope

  # Create full yield curve paths (time series format)
  yield_curve <- data.frame(
    three_month = transformed_3mo,
    one_year = preds[, 1],
    two_year = preds[, 2],
    three_year = preds[, 3],
    five_year = preds[, 4],
    seven_year = preds[, 5],
    ten_year = ten_year,
    twenty_year = preds[, 6],
    thirty_year = thirty_year
  )

  final_yield_curve <- yield_curve |>
    mutate(across(three_month:thirty_year, back_transform))

  return(final_yield_curve)
}

yield_multiple_sims <- function(
  var_mod,
  lm_mods,
  sim_length,
  n_sims,
  ir3mo_sims,
  final_vals
) {
  sims <- vector("list", n_sims)

  for (i in 1:n_sims) {
    sims[[i]] <- yield_single_sim(
      var_mod,
      lm_mods,
      sim_length,
      ir3mo_sims[i, ],
      final_vals
    )
  }

  return(sims)
}

# Equity Returns -----------------------------------------------------------

# Must refit MSGARCH model, fitted model can't be saved out of memory due to stored C++ pointers that can't be loaded across R sessions
fit_equity_rs_model <- function(data) {
  spec <- CreateSpec(
    variance.spec = list(model = "sGARCH"),
    distribution.spec = list("norm"),
    switch.spec = list(K = 2)
  )
  MSGARCH::FitML(spec, data = data)
}

equity_single_sim <- function(
  mean_mod,
  rs_mod,
  sim_length,
  cpi_sim,
  ir3mo_sim
) {
  X <- data.frame(log_dif_cpi = cpi_sim, rate = ir3mo_sim)
  mean_vals <- predict(mean_mod, X)
  rs_sim <- simulate(rs_mod, nsim = sim_length)
  sim_df <- data.frame(resid_sim = rs_sim$draw[1, ], state = rs_sim$state[1, ])

  final_sim <- sim_df$resid_sim + mean_vals
  return(final_sim)
}

equity_multiple_sims <- function(
  mean_mod,
  rs_mod,
  sim_length,
  nsims,
  cpi_sims,
  ir3mo_sims
) {
  sims <- matrix(NA, nrow = n_sims, ncol = sim_length)

  for (i in 1:n_sims) {
    sims[i, ] <- equity_single_sim(
      mean_mod,
      rs_mod,
      sim_length,
      cpi_sims[[i]],
      ir3mo_sims[i, ]
    )
  }

  return(sims)
}


# Annuity Pricing Functions -----------------------------------------------

get_yield_df <- function(yield_curve) {
  yield_curve_df <- yield_curve %>%
    pivot_longer(
      three_month:thirty_year,
      names_to = "time",
      values_to = "rate"
    ) %>%
    mutate(
      time = case_when(
        time == "three_month" ~ 3 / 12,
        time == "one_year" ~ 1,
        time == "two_year" ~ 2,
        time == "three_year" ~ 3,
        time == "five_year" ~ 5,
        time == "seven_year" ~ 7,
        time == "ten_year" ~ 10,
        time == "twenty_year" ~ 20,
        time == "thirty_year" ~ 30
      )
    )
  return(yield_curve_df)
}


# Vectorized interpolation using base R
interpolate_rates_vec <- function(curve_df, target_months) {
  # Convert to years
  target_time <- target_months / 12

  # Ensure sorted once
  curve_df <- curve_df[order(curve_df$time), ]

  # Use approx for linear interpolation with constant extrapolation
  approx(x = curve_df$time, y = curve_df$rate, xout = target_time, rule = 2)$y
}

price_annuity <- function(start_age, yield_curve, mortality_tbl, payout, load) {
  # Prepare yield curve once
  yield_curve_df <- get_yield_df(yield_curve[1, ])
  yield_curve_df <- yield_curve_df[order(yield_curve_df$time), ]

  # Mortality clipping
  if (start_age * 12 < min(mortality_tbl$month) - 1) {
    stop("start_age must be at least 50")
  }
  mortality_tbl_clipped <- mortality_tbl[
    mortality_tbl$month >= start_age * 12,
  ]

  months_seq <- seq_len(nrow(mortality_tbl_clipped))

  # Vectorized interpolation
  rates <- interpolate_rates_vec(yield_curve_df, months_seq)

  # Compute discount and EPV in base R
  discounts <- (1 + rates)^(-months_seq / 12)
  epv <- discounts * mortality_tbl_clipped$S

  price <- sum(epv) * payout * (1 + load)
  return(price)
}

price_annuities <- function(
  start_age,
  yield_curves,
  mortality_tbl,
  payout,
  load,
  n_sims
) {
  prices <- numeric(n_sims)
  for (i in seq_len(n_sims)) {
    prices[i] <- price_annuity(
      start_age,
      yield_curves[[i]],
      mortality_tbl,
      payout,
      load
    )
  }
  return(prices)
}
