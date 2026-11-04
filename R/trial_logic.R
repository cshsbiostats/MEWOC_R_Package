## Stopping rule: stopping for trial efficiency (shared by the simulation engines, Trial Conduct and the tree).
## Stop when at least max_patients patients have already been treated at the recommended next dose.
## Discrete doses (width = 0): the same dose level. Continuous doses: doses within +/- width / 2 of the
## recommended dose, where width is the dose-region width on the standardized scale. The window is
## centred on the recommended dose, so the count does not depend on where fixed bin edges fall.
efficiency_stop_check <- function(assigned_doses, next_dose, max_patients, width = 0) {
  assigned_doses <- suppressWarnings(as.numeric(assigned_doses))
  next_dose <- suppressWarnings(as.numeric(next_dose))
  max_patients <- suppressWarnings(as.numeric(max_patients))
  if (length(assigned_doses) == 0L || length(next_dose) != 1L || !is.finite(next_dose) ||
      length(max_patients) != 1L || !is.finite(max_patients)) {
    return(FALSE)
  }
  width <- suppressWarnings(as.numeric(width))
  half_width <- if (length(width) == 1L && is.finite(width) && width > 0) width / 2 else 0
  tolerance <- half_width + sqrt(.Machine$double.eps)
  sum(abs(assigned_doses - next_dose) <= tolerance, na.rm = TRUE) >= max_patients
}

## Stopping rule: whether the efficiency stop is in use with a usable patient limit.
efficiency_stop_enabled <- function(stopping_rule_selected, num_patients_restrict) {
  isTRUE(stopping_rule_selected) && length(num_patients_restrict) == 1L &&
    is.finite(suppressWarnings(as.numeric(num_patients_restrict)))
}

## Trial logic: Cap the first-ever escalation step to one dose level above the current dose (dose recommendation, one-drug discrete)
apply_first_time_escalation_rule <- function(recommended_dose, current_dose, assigned_doses, incr) {
  if (!is.finite(recommended_dose) || !is.finite(current_dose) || !is.finite(incr)) {
    return(recommended_dose)
  }
  
  if (recommended_dose <= current_dose + incr) {
    return(recommended_dose)
  }
  
  dose_seen_before <- any(abs(assigned_doses - recommended_dose) < sqrt(.Machine$double.eps))
  if (dose_seen_before) {
    return(recommended_dose)
  }
  
  current_dose + incr
}

## Trial logic: Model DLT probability under the one-drug logistic toxicity model (true/prior DLT model, one-drug shared)
compute_pdlt <- function(rho0, rho1, X) {
  beta0 <- log(rho0 / (1 - rho0))
  beta1 <- log(rho1 / (1 - rho1)) - log(rho0 / (1 - rho0))
  exp(beta0 + beta1 * X) / (1 + exp(beta0 + beta1 * X))
}

## Trial logic: Convert an actual dose value to the standardized [0,1] scale (dose scaling, one-drug shared)
standardize_actual_dose <- function(actual_dose, min_dose, max_dose) {
  (actual_dose - min_dose) / (max_dose - min_dose)
}

## Trial logic: Convert a standardized [0,1] dose back to an actual dose value (dose scaling, one-drug shared)
actualize_standardized_dose <- function(standardized_dose, min_dose, max_dose) {
  min_dose + standardized_dose * (max_dose - min_dose)
}

format_dose_value <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return(NA_character_)
  }
  format(round(value, 4), trim = TRUE, scientific = FALSE)
}

## Trial logic: Build the "value unit" display label for each discrete dose level -- accepts either one shared unit (recycled across all levels, the original behavior) or a per-dose-level unit vector (one unit per dose level, since different dose levels can be dosed in different units) (dose labeling, single-agent dose labeling)
build_discrete_dose_labels <- function(actual_dose_levels = NULL, dose_unit = NULL, num_doses = NULL) {
  if (is.null(num_doses)) {
    num_doses <- length(actual_dose_levels)
  }
  num_doses <- as.integer(num_doses)
  dose_unit_raw <- if (is.null(dose_unit)) "" else trimws(as.character(dose_unit))
  dose_unit_raw[is.na(dose_unit_raw)] <- ""
  if (length(dose_unit_raw) == num_doses && num_doses > 0L) {
    # Per-dose-level units: one unit per dose level.
    dose_units <- dose_unit_raw
  } else {
    # Single shared unit (or none): recycle it across every dose level, same
    # as the original behavior.
    dose_units <- rep(if (length(dose_unit_raw) >= 1L) dose_unit_raw[1] else "", num_doses)
  }
  actual_dose_levels <- suppressWarnings(as.numeric(actual_dose_levels))
  if (length(actual_dose_levels) != num_doses) {
    return(paste("Dose", seq_len(num_doses)))
  }

  # A level may have a number and unit ("20 mg"), or only a description (a combination such as
  # "10 mg + drug X 50 mg/m2"); a level with neither is labelled "Dose <level>".
  vapply(seq_len(num_doses), function(i) {
    value_text <- if (is.finite(actual_dose_levels[i])) format_dose_value(actual_dose_levels[i]) else ""
    label <- trimws(paste(value_text, dose_units[i]))
    if (nzchar(label)) label else paste("Dose", i)
  }, character(1))
}

## Trial logic: Units to label a summary's dose levels -- the per-level units when stored, otherwise the
## shared unit (summaries made before per-level units).
summary_dose_units <- function(inputs) {
  units <- inputs$actual_dose_units
  if (!is.null(units) && length(units) == length(inputs$actual_dose_levels)) units else inputs$dose_unit
}

label_discrete_dose_values <- function(standardized_doses, doselevels, dose_labels) {
  standardized_doses <- suppressWarnings(as.numeric(standardized_doses))
  vapply(standardized_doses, function(value) {
    if (!is.finite(value)) {
      return(NA_character_)
    }
    idx <- which.min(abs(doselevels - value))
    if (length(idx) == 0L || is.na(idx) || idx < 1L || idx > length(dose_labels)) {
      return(format_dose_value(value))
    }
    dose_labels[idx]
  }, character(1))
}

## Trial logic: Discrete design used by the simulation engine and its summary: level-based dose
## positions 0, 1/(K-1), ..., 1 (the actual doses and units are labels only).
prepare_discrete_simulation_design <- function(trial_inputs) {
  num_doses <- as.integer(trial_inputs$num_doses)
  units <- as.character(trial_inputs$actual_dose_units)
  if (length(units) != num_doses) units <- rep(if (length(trial_inputs$dose_unit) >= 1L) as.character(trial_inputs$dose_unit[1]) else "", num_doses)
  list(
    doselevels = as.numeric(seq(0, 1, length.out = num_doses)),
    doseprobs = as.numeric(trial_inputs$doseprobs),
    actual_dose_levels = suppressWarnings(as.numeric(trial_inputs$actual_dose_levels)),
    dose_units = units,
    startdose = as.integer(trial_inputs$startdose)
  )
}

## Stopping rule: MEWOC safety stop (Delta), shared by the simulation engines, Trial Conduct and the tree.
## Stop when Pr[P(DLT at the minimum dose) > theta] > Delta (strictly greater; changed from >= on 29 Oct 2026).
safety_stop_check <- function(ss, theta, deltastop) {
  probability <- mean(compute_pdlt(ss$rho0, ss$rho1, X = 0) > theta)
  list(
    probability = probability,
    stop = is.finite(probability) && probability > deltastop
  )
}

## Trial logic: Map the posterior MTD estimate to the nearest discrete dose level (dose scaling, one-drug discrete)
map_posterior_mtd_to_discrete_level <- function(mtd_draws, doselevels, lower_bound = 0, upper_bound = 1) {
  mtd_draws <- as.numeric(mtd_draws[is.finite(mtd_draws)])
  if (length(mtd_draws) == 0L || length(doselevels) == 0L) {
    return(list(dose_value = NA_real_, dose_index = NA_integer_, posterior_median = NA_real_))
  }
  posterior_median <- max(lower_bound, min(upper_bound, stats::median(mtd_draws, na.rm = TRUE)))
  dose_index <- which.min(abs(doselevels - posterior_median))
  list(
    dose_value = doselevels[dose_index],
    dose_index = as.integer(dose_index),
    posterior_median = posterior_median
  )
}

## Trial logic: True DLT probability under the one-drug continuous logistic toxicity model (true/prior DLT model, one-drug continuous)
true_continuous_pdlt <- function(standardized_dose, true_beta0, true_beta1) {
  plogis(true_beta0 + true_beta1 * standardized_dose)
}

## Trial logic: JAGS model string for the one-drug logistic toxicity model (posterior fitting, one-drug shared)
mewoc_rho1_bounds <- function() c(lower = 0.001, upper = 0.999)

build_jags_model_string <- function() {
  bounds <- mewoc_rho1_bounds()
  paste(
    " model {",
    "for (i in 1:N){",
    "Y[i] ~ dbern(p[i])",
    "logit(p[i]) <- logit(rho0) + X[i]*( logit(rho1)-logit(rho0) )",
    "}",
    sprintf("rho1 ~ dbeta(a0,b0) T(%g, %g)", bounds[["lower"]], bounds[["upper"]]),
    "temp ~ dbeta(a1,b1)",
    "rho0 <- rho1* temp",
    "mtd <- ( logit(theta)-logit(rho0) )/( logit(rho1)-logit(rho0) )",
    "}",
    sep = "\n"
  )
}

## Trial logic: Precompute the full alpha (feasibility bound) sequence across cohorts (dose recommendation, one-drug shared)
build_alpha_sequence <- function(lower_f_bound, increment, num_updates) {
  is_num1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!is_num1(num_updates) || num_updates < 0 || num_updates != round(num_updates)) {
    stop("num_updates must be a nonnegative integer.", call. = FALSE)
  }
  if (num_updates == 0) return(numeric(0))
  if (!is_num1(lower_f_bound) || lower_f_bound < 0 || lower_f_bound > 0.5) {
    stop("lower_f_bound must be between 0 and 0.5.", call. = FALSE)
  }
  if (!is_num1(increment) || increment <= 0) {
    stop("increment must be a positive finite number.", call. = FALSE)
  }
  pmin(lower_f_bound + (seq_len(num_updates) - 1L) * increment, 0.5)
}

## Trial logic: Observed DLT rate for one trial (post-trial summary, one-drug shared)
compute_dlt_percentage <- function(Y, X) {
  if (length(X) == 0L) {
    return(NA_real_)
  }
  sum(Y, na.rm = TRUE) / length(X)
}

## Trial logic: Advance the alpha step only when the observed cumulative DLT rate is below the alpha-increment cutoff (dose recommendation, one-drug shared)
advance_alpha_step_if_allowed <- function(alpha_step, alpha_values, Y, X, theta, alpha_increment_cutoff = theta) {
  if (length(alpha_values) == 0L) {
    return(0L)
  }
  
  alpha_step <- max(1L, min(as.integer(alpha_step), length(alpha_values)))
  dlt_percentage <- compute_dlt_percentage(Y, X)
  if (!is.finite(alpha_increment_cutoff)) {
    alpha_increment_cutoff <- theta
  }
  if (!is.finite(dlt_percentage) || !is.finite(alpha_increment_cutoff)) {
    return(alpha_step)
  }
  
  if (dlt_percentage < alpha_increment_cutoff) {
    return(min(alpha_step + 1L, length(alpha_values)))
  }
  
  alpha_step
}

## Trial logic: Mean and normal-approximation 95% CI, formatted as display text (post-trial summary, one-drug shared)
format_mean_ci <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return("Not reached")
  }
  mean_x <- mean(x)
  if (length(x) == 1) {
    return(paste0(round(mean_x, 3), " (", round(mean_x, 3), ", ", round(mean_x, 3), ")"))
  }
  se_x <- stats::sd(x) / sqrt(length(x))
  z_975 <- stats::qnorm(0.975)
  ci_lower <- mean_x - z_975 * se_x
  ci_upper <- mean_x + z_975 * se_x
  paste0(round(mean_x, 3), " (", round(ci_lower, 3), ", ", round(ci_upper, 3), ")")
}

## Trial logic: Round the allocation/MTD-recommendation percentage columns for display (post-trial summary, one-drug shared)
round_trial_efficiency_percentages <- function(combined_table) {
  if (is.null(combined_table) || nrow(combined_table) == 0) {
    return(combined_table)
  }
  percent_cols <- intersect(c("% Allocation", "% MTD recommendation", "Prop_Patients", "Prop_Trials_MTD"), names(combined_table))
  for (col_name in percent_cols) {
    col_value <- suppressWarnings(as.numeric(combined_table[[col_name]]))
    if (any(is.finite(col_value))) {
      combined_table[[col_name]] <- round(col_value, digits = 2)
    }
  }
  combined_table
}

## Trial logic: Split the "Burn-in" iterations into JAGS adaptation plus a true burn-in (MCMC, one-drug shared)
## Half of the iterations adapt the samplers (n.adapt), the rest are run with update() and discarded,
## so the total number of discarded iterations still equals `burn`.
split_burn_in <- function(burn) {
  burn <- max(0L, as.integer(burn))
  n_adapt <- as.integer(ceiling(burn / 2))
  list(n_adapt = n_adapt, n_burn = burn - n_adapt)
}

## Trial logic: Fit the one-drug JAGS model to the accumulated cohort data and return posterior draws (posterior fitting, one-drug shared)
check_single_chain <- function(chains) {
  if (!is.numeric(chains) || length(chains) != 1L || !is.finite(chains) || chains != 1) {
    stop("chains must be 1; MEWOC supports a single MCMC chain.", call. = FALSE)
  }
  invisible(TRUE)
}

fit_trial_posterior <- function(model_string, Y, X, opt1, theta, n, chains, burn, mm) {
  check_single_chain(chains)
  jags_inits <- list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = as.integer(sample.int(.Machine$integer.max, 1)))
  burn_split <- split_burn_in(burn)
  j <- rjags::jags.model(
    textConnection(model_string),
    data = list("Y" = Y, "X" = X, "a0" = opt1$a0, "a1" = opt1$a1, "b0" = opt1$b0, "b1" = opt1$b1, "theta" = theta, "N" = n),
    inits = jags_inits,
    n.chains = chains,
    n.adapt = burn_split$n_adapt,
    quiet = TRUE
  )
  if (burn_split$n_burn > 0L) {
    stats::update(j, n.iter = burn_split$n_burn, progress.bar = "none")
  }
  s <- rjags::coda.samples(j, c("rho0", "rho1", "mtd"), mm)
  as.data.frame(s[[1]])
}

## Trial logic: EWOC alpha-quantile next-dose recommendation, snapped to the discrete dose grid, with the first-time escalation rule applied (dose recommendation, one-drug discrete)
recommend_next_dose <- function(ss, alpha_value, doselevels, current_dose, assigned_doses, incr, lower_bound = 0, upper_bound = 1) {
  trmtd <- ss$mtd
  xx <- max(lower_bound, quantile(trmtd, alpha_value))
  xx <- min(xx, upper_bound)
  dif <- abs(xx - doselevels)
  index <- which(dif == min(dif))[1]
  xxdiscrete <- doselevels[index]
  
  xxdiscrete <- apply_first_time_escalation_rule(
    recommended_dose = xxdiscrete,
    current_dose = current_dose,
    assigned_doses = assigned_doses,
    incr = incr
  )
  
  dif <- abs(xxdiscrete - doselevels)
  min_dif <- min(dif)
  tied_indices <- which(dif == min_dif)
  if (length(tied_indices) == 1) {
    index <- tied_indices
  } else {
    dist_to_current <- abs(doselevels[tied_indices] - current_dose)
    index <- tied_indices[which.min(dist_to_current)]
  }
  
  list(
    dose_value = doselevels[index],
    dose_index = as.integer(index)
  )
}

## Trial logic: EWOC alpha-quantile next-dose recommendation on the continuous dose grid (dose recommendation, one-drug continuous)
recommend_next_continuous_dose <- function(ss, alpha_value, current_dose, max_increment_std, lower_bound = 0, upper_bound = 1) {
  posterior_mtd <- as.numeric(stats::quantile(ss$mtd, probs = alpha_value, na.rm = TRUE))
  if (!is.finite(posterior_mtd)) {
    stop("Posterior MTD quantile is not finite.")
  }
  if (!is.finite(current_dose)) {
    stop("Current dose is not finite.")
  }
  if (!is.finite(max_increment_std) || max_increment_std <= 0) {
    stop("Maximum dose increment percentage must be greater than 0.")
  }
  
  constrained <- min(posterior_mtd, current_dose + max_increment_std)
  constrained <- max(lower_bound, min(upper_bound, constrained))
  list(
    dose_value = constrained,
    posterior_mtd = posterior_mtd
  )
}

## Trial logic: Accelerated EWOC lead-in (one-drug discrete, original dose levels only). One patient at a
## time, one dose level up per patient (no dose is skipped). The lead-in ends at the first DLT or after the
## patient treated at the maximum accelerated dose level. There is no model fit and no Delta check during
## the lead-in; EWOC then uses all collected data to recommend the dose for the next cohort.
run_accelerated_leadin <- function(doselevels, doseprobs, startdose, max_accel_dose_level, NN) {
  startdose <- as.integer(startdose)
  cap_idx <- suppressWarnings(as.integer(max_accel_dose_level))
  if (length(cap_idx) != 1L || !is.finite(cap_idx)) cap_idx <- startdose
  cap_idx <- max(startdose, min(cap_idx, length(doselevels)))
  current_idx <- startdose
  X <- numeric(0)
  Y <- integer(0)
  repeat {
    X <- c(X, doselevels[current_idx])
    Y <- c(Y, as.integer(stats::rbinom(1, 1, doseprobs[current_idx])))
    if (Y[length(Y)] == 1L) {
      reason <- "DLT observed during accelerated phase"
      break
    }
    if (current_idx >= cap_idx) {
      reason <- "Maximum accelerated dose reached"
      break
    }
    if (length(X) >= NN) {
      reason <- "Maximum sample size reached"
      break
    }
    current_idx <- current_idx + 1L
  }
  list(X = X, Y = Y, accel_patients = length(X), accel_stop_reason = reason)
}

## Trial logic: EWOC cohort loop shared by all one-drug engines. Starting from the data already collected
## (X, Y), each step: fit the posterior; check the Delta stop; recommend the next dose; check the
## efficiency stop; treat the next cohort (the last cohort may be partial). After the loop the final
## posterior is fitted; when the trial reached NN without a stop, the Delta stop is checked once more on
## the final data. recommend(ss, alpha_value, current_dose, X) returns list(dose_value, dose_index);
## treat(dose_value, dose_index, n) returns n DLT outcomes.
run_ewoc_cohort_loop <- function(X, Y, cohort, NN, alpha, theta, deltastop, burn, mm, chains, opt1,
                                 stopping_rule_selected, num_patients_restrict, efficiency_width,
                                 alpha_increment_cutoff, recommend, treat) {
  model_string <- build_jags_model_string()
  efficiency_on <- efficiency_stop_enabled(stopping_rule_selected, num_patients_restrict)
  postmind0 <- rep(0, NN)
  alpha_step <- 1L
  first_delta_n <- NA_real_
  first_stop_rule_n <- NA_real_
  first_ewoc_index <- NA_integer_

  while (length(X) < NN) {
    n <- length(X)
    ss <- fit_trial_posterior(model_string, Y, X, opt1, theta, n, chains, burn, mm)

    ## Stopping rule: minimum dose too toxic (Delta)
    safety <- safety_stop_check(ss, theta, deltastop)
    postmind0[n] <- safety$probability
    if (isTRUE(safety$stop)) {
      first_delta_n <- n
      break
    }

    next_dose <- recommend(ss, alpha[alpha_step], tail(X, 1L), X)
    if (is.na(first_ewoc_index) && !is.null(next_dose$dose_index)) {
      first_ewoc_index <- as.integer(next_dose$dose_index)
    }

    ## Stopping rule: stopping for trial efficiency (N already treated at the recommended dose)
    if (efficiency_on && efficiency_stop_check(X, next_dose$dose_value, num_patients_restrict, efficiency_width)) {
      first_stop_rule_n <- n
      break
    }

    n_next <- min(cohort, NN - n)
    if (!is.finite(n_next) || n_next < 1 || n_next != round(n_next)) {
      stop("The cohort loop cannot advance: cohort and NN must be positive integers.", call. = FALSE)
    }
    X <- c(X, rep(next_dose$dose_value, n_next))
    Y <- c(Y, treat(next_dose$dose_value, next_dose$dose_index, n_next))
    if (length(X) <= n || length(Y) != length(X)) {
      stop("The cohort loop did not add the expected patient outcomes.", call. = FALSE)
    }
    alpha_step <- advance_alpha_step_if_allowed(alpha_step, alpha, Y, X, theta, alpha_increment_cutoff)
  }

  n <- length(X)
  final_ss <- fit_trial_posterior(model_string, Y, X, opt1, theta, n, chains, burn, mm)
  ## Stopping rule: Delta on the final data when the trial reached NN
  if (!is.finite(first_delta_n) && !is.finite(first_stop_rule_n)) {
    final_safety <- safety_stop_check(final_ss, theta, deltastop)
    postmind0[n] <- final_safety$probability
    if (isTRUE(final_safety$stop)) {
      first_delta_n <- n
    }
  }
  if (n < NN) {
    postmind0[(n + 1):NN] <- NA_real_
  }

  list(
    X = X,
    Y = Y,
    postmind0 = postmind0,
    final_ss = final_ss,
    first_delta_n = first_delta_n,
    first_stop_rule_n = first_stop_rule_n,
    first_ewoc_index = first_ewoc_index
  )
}

## Trial logic: Trial-level result list for the discrete engines (one-drug discrete/accelerated)
discrete_trial_result <- function(trial_id, phase, doselevels, NN, trial_mode, leadin = NULL) {
  n <- length(phase$X)
  delta_stop <- is.finite(phase$first_delta_n)
  final_mtd <- map_posterior_mtd_to_discrete_level(phase$final_ss$mtd, doselevels)
  patcntmaxd0d0 <- rep(0, NN)
  patcntmaxd0d0[1] <- 1
  list(
    success = TRUE,
    trial_id = trial_id,
    dose = phase$X,
    dlt = phase$Y,
    postmind0 = phase$postmind0,
    vrho0 = mean(phase$final_ss$rho0),
    # Stopped because the lowest dose is too toxic: no dose is recommended as MTD.
    vmtd = if (delta_stop) NA_real_ else final_mtd$dose_value,
    vmtd_last_assigned = phase$X[n],
    vmtd_level = if (delta_stop) NA_integer_ else final_mtd$dose_index,
    vmtd_posterior_median = final_mtd$posterior_median,
    vrho1 = mean(phase$final_ss$rho1),
    vdltrate = sum(phase$Y == 1) / n,
    nn_eff = n,
    stopval = as.integer(is.finite(phase$first_stop_rule_n)),
    first_delta_n = phase$first_delta_n,
    first_stop_rule_n = phase$first_stop_rule_n,
    patcntmind0d0 = rep(0, NN),
    patcntmaxd0d0 = patcntmaxd0d0,
    trial_mode = trial_mode,
    accel_patients = if (is.null(leadin)) NA_integer_ else as.integer(leadin$accel_patients),
    accel_stop_reason = if (is.null(leadin)) NA_character_ else leadin$accel_stop_reason,
    ewoc_entry_dose = if (is.null(leadin)) NA_integer_ else phase$first_ewoc_index,
    ewoc_entry_patient_n = if (is.null(leadin)) NA_integer_ else as.integer(leadin$accel_patients)
  )
}

## Trial logic: Discrete next-dose and outcome functions for run_ewoc_cohort_loop (one-drug discrete)
discrete_recommend_fn <- function(doselevels, incr) {
  function(ss, alpha_value, current_dose, X) {
    recommend_next_dose(ss, alpha_value, doselevels, current_dose, X, incr)
  }
}

discrete_treat_fn <- function(doseprobs) {
  function(dose_value, dose_index, n) {
    as.integer(stats::rbinom(n, 1, doseprobs[dose_index]))
  }
}

## Trial logic: Run one complete one-drug trial (Standard Discrete engine) -- the first cohort at the starting dose, then the EWOC cohort loop with the Delta and efficiency stopping rules (trial loop, one-drug discrete)
run_single_trial_simulation_standard <- function(trial_id, doselevels, doseprobs, startdose, cohort, NN, alpha,
                                                 theta, deltastop, burn, mm, chains, opt1,
                                                 stopping_rule_selected, num_patients_restrict, incr,
                                                 alpha_increment_cutoff = theta) {
  tryCatch({
    first_n <- min(cohort, NN)
    X <- rep(doselevels[startdose], first_n)
    Y <- as.integer(stats::rbinom(first_n, 1, doseprobs[startdose]))
    phase <- run_ewoc_cohort_loop(
      X, Y, cohort, NN, alpha, theta, deltastop, burn, mm, chains, opt1,
      stopping_rule_selected, num_patients_restrict, efficiency_width = 0,
      alpha_increment_cutoff = alpha_increment_cutoff,
      recommend = discrete_recommend_fn(doselevels, incr), treat = discrete_treat_fn(doseprobs)
    )
    discrete_trial_result(trial_id, phase, doselevels, NN, "Standard EWOC")
  }, error = function(e) {
    list(
      success = FALSE,
      trial_id = trial_id,
      error = conditionMessage(e)
    )
  })
}

## Trial logic: Run one complete one-drug trial (Accelerated EWOC engine) -- the singleton lead-in, then the EWOC cohort loop starting at the starting alpha (trial loop, one-drug accelerated)
run_single_trial_simulation_accelerated <- function(trial_id, doselevels, doseprobs, startdose, cohort, NN, alpha,
                                                    theta, deltastop, burn, mm, chains, opt1,
                                                    stopping_rule_selected, num_patients_restrict, incr,
                                                    max_accel_dose_level, alpha_increment_cutoff = theta) {
  tryCatch({
    leadin <- run_accelerated_leadin(
      doselevels = doselevels,
      doseprobs = doseprobs,
      startdose = startdose,
      max_accel_dose_level = max_accel_dose_level,
      NN = NN
    )
    phase <- run_ewoc_cohort_loop(
      leadin$X, leadin$Y, cohort, NN, alpha, theta, deltastop, burn, mm, chains, opt1,
      stopping_rule_selected, num_patients_restrict, efficiency_width = 0,
      alpha_increment_cutoff = alpha_increment_cutoff,
      recommend = discrete_recommend_fn(doselevels, incr), treat = discrete_treat_fn(doseprobs)
    )
    discrete_trial_result(trial_id, phase, doselevels, NN, "Accelerated EWOC", leadin = leadin)
  }, error = function(e) {
    list(
      success = FALSE,
      trial_id = trial_id,
      error = conditionMessage(e)
    )
  })
}

## Trial logic: Run one complete one-drug trial (Standard Continuous engine) -- the first cohort at the minimum dose, then the EWOC cohort loop on the continuous scale with the maximum dose increment, Delta and efficiency stopping rules (trial loop, one-drug continuous)
run_single_trial_simulation_standard_continuous <- function(trial_id, min_dose, max_dose, true_beta0, true_beta1,
                                                            true_mtd, true_p_min, true_p_max,
                                                            max_dose_increment, cohort, NN, alpha,
                                                            theta, deltastop, burn, mm, chains, opt1,
                                                            stopping_rule_selected, num_patients_restrict,
                                                            efficiency_stop_tolerance = 1,
                                                            alpha_increment_cutoff = theta) {
  tryCatch({
    # Previous real-dose increment standardization:
    # max_increment_std <- max_dose_increment / (max_dose - min_dose)
    max_increment_std <- max_dose_increment / 100
    efficiency_width <- suppressWarnings(as.numeric(efficiency_stop_tolerance)) / 100
    first_n <- min(cohort, NN)
    X <- rep(0, first_n)
    Y <- as.integer(stats::rbinom(first_n, 1, true_continuous_pdlt(0, true_beta0, true_beta1)))
    phase <- run_ewoc_cohort_loop(
      X, Y, cohort, NN, alpha, theta, deltastop, burn, mm, chains, opt1,
      stopping_rule_selected, num_patients_restrict, efficiency_width = efficiency_width,
      alpha_increment_cutoff = alpha_increment_cutoff,
      recommend = function(ss, alpha_value, current_dose, X) {
        recommend_next_continuous_dose(ss, alpha_value, current_dose, max_increment_std)
      },
      treat = function(dose_value, dose_index, n) {
        as.integer(stats::rbinom(n, 1, true_continuous_pdlt(dose_value, true_beta0, true_beta1)))
      }
    )

    n <- length(phase$X)
    final_mtd_std <- max(0, min(1, stats::median(phase$final_ss$mtd, na.rm = TRUE)))
    # Stopped because the lowest dose is too toxic: no dose is recommended as MTD.
    vmtd_std <- if (is.finite(phase$first_delta_n)) NA_real_ else final_mtd_std
    patcntmaxd0d0 <- rep(0, NN)
    patcntmaxd0d0[1] <- 1

    list(
      success = TRUE,
      trial_id = trial_id,
      dose = phase$X,
      actual_dose = actualize_standardized_dose(phase$X, min_dose, max_dose),
      dlt = phase$Y,
      postmind0 = phase$postmind0,
      vrho0 = mean(phase$final_ss$rho0),
      vmtd = actualize_standardized_dose(vmtd_std, min_dose, max_dose),
      vmtd_standardized = vmtd_std,
      vrho1 = mean(phase$final_ss$rho1),
      vdltrate = sum(phase$Y == 1) / n,
      nn_eff = n,
      stopval = as.integer(is.finite(phase$first_stop_rule_n)),
      first_delta_n = phase$first_delta_n,
      first_stop_rule_n = phase$first_stop_rule_n,
      patcntmind0d0 = rep(0, NN),
      patcntmaxd0d0 = patcntmaxd0d0,
      trial_mode = "Standard EWOC",
      accel_patients = NA_integer_,
      accel_stop_reason = NA_character_,
      ewoc_entry_dose = NA_real_,
      ewoc_entry_patient_n = NA_integer_,
      true_mtd = true_mtd,
      true_p_min = true_p_min,
      true_p_max = true_p_max
    )
  }, error = function(e) {
    list(
      success = FALSE,
      trial_id = trial_id,
      error = conditionMessage(e)
    )
  })
}

## Trial logic: Discrete-mode dispatcher -- routes to the Accelerated engine when trial_mode is "Accelerated EWOC", otherwise to the Standard Discrete engine (trial loop dispatch, one-drug discrete/accelerated only -- Continuous is dispatched separately, see run_trial_simulations_background)
#' Run one single-agent MEWOC trial simulation
#'
#' Simulates one trial with discrete dose levels under a true DLT scenario. Each
#' model-based dose is the alpha-quantile of the posterior MTD, bounded to the
#' trial range and rounded to the nearest discrete level, subject to the
#' no-skipping rule. Nearest-level rounding can give a posterior overdose
#' probability above alpha; alpha is not a strict bound at the rounded dose. The
#' trial stops early for safety when the posterior probability that the DLT
#' probability at the minimum dose exceeds `theta` is greater than `deltastop`, or,
#' if selected, for efficiency. Posterior computation uses 'JAGS' through 'rjags'.
#'
#' @param trial_id Trial identifier stored in the result.
#' @param doselevels Standardized dose levels, increasing from 0 (minimum dose)
#'   to 1 (maximum dose).
#' @param doseprobs True DLT probability at each dose level (same length as
#'   `doselevels`).
#' @param startdose Starting dose level (index into `doselevels`).
#' @param cohort Cohort size.
#' @param NN Maximum sample size.
#' @param alpha Feasibility bound: a single value, or the sequence of values used
#'   for successive cohorts (for example `seq(0.25, 0.5, by = 0.05)`).
#' @param theta Target probability of a DLT, strictly between 0 and 1.
#' @param deltastop Safety stopping threshold (default 0.85).
#' @param burn Number of MCMC burn-in iterations.
#' @param mm Number of retained MCMC samples.
#' @param chains Number of MCMC chains; must be 1 (the default). Other values
#'   are rejected before simulation.
#' @param opt1 Prior hyperparameters with elements `a0`, `b0`, `a1`, `b1`, for
#'   example the result of [run_mewoc_grid_search()].
#' @param stopping_rule_selected Logical; use the efficiency stopping rule.
#' @param num_patients_restrict Efficiency rule: stop when this many patients have
#'   already been treated at the dose recommended for the next cohort (`NA` when
#'   the rule is not used).
#' @param incr Standardized distance between adjacent dose levels (for equally
#'   spaced levels, `1 / (number of levels - 1)`); an untried level is never
#'   skipped.
#' @param trial_mode `"Standard EWOC"` or `"Accelerated EWOC"` (one patient per
#'   level until the first DLT or `max_accel_dose_level`).
#' @param max_accel_dose_level Highest dose level of the accelerated lead-in.
#' @param alpha_increment_cutoff The feasibility bound increases only while the
#'   observed DLT rate is below this value (default `theta`).
#'
#' @return A list. When the run succeeds (`success = TRUE`) it contains
#'   `trial_id`; `dose` and `dlt`, the standardized dose and DLT outcome (1/0) of
#'   each patient; `nn_eff`, the number of patients treated; `vmtd` and
#'   `vmtd_level`, the recommended MTD (standardized dose and level; `NA` after a
#'   safety stop); `vmtd_posterior_median`; `vrho0` and `vrho1`, the posterior
#'   means of the DLT probabilities at the minimum and maximum dose; `vdltrate`,
#'   the observed DLT rate; `postmind0`, the safety-stop probability after each
#'   patient; `first_delta_n` and `first_stop_rule_n`, the sample size at a safety
#'   or efficiency stop (`NA` if none); `stopval` (1 for an efficiency stop); and
#'   lead-in details for accelerated designs. When a run fails it contains
#'   `success = FALSE`, `trial_id` and `error`.
#'
#' @references
#' Babb J, Rogatko A, Zacks S (1998). Cancer phase I clinical trials: efficient
#' dose escalation with overdose control. Statistics in Medicine, 17(10),
#' 1103-1120.
#'
#' @seealso [run_trial_simulations_background()] for many trials.
#' @examples
#' prior <- data.frame(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)
#' set.seed(1)
#' trial <- run_single_trial_simulation(
#'   trial_id = 1, doselevels = seq(0, 1, length.out = 4),
#'   doseprobs = c(0.05, 0.15, 0.33, 0.50), startdose = 1, cohort = 3, NN = 12,
#'   alpha = seq(0.25, 0.5, by = 0.05), theta = 0.33, burn = 200, mm = 300,
#'   chains = 1, opt1 = prior, stopping_rule_selected = FALSE,
#'   num_patients_restrict = NA, incr = 1 / 3)
#' trial$dose
#' trial$vmtd_level
#' @export
run_single_trial_simulation <- function(trial_id, doselevels, doseprobs, startdose, cohort, NN, alpha,
                                        theta, deltastop = 0.85, burn, mm, chains = 1L, opt1,
                                        stopping_rule_selected, num_patients_restrict, incr,
                                        trial_mode = "Standard EWOC",
                                        max_accel_dose_level = NA_integer_,
                                        alpha_increment_cutoff = theta) {
  check_single_chain(chains)
  check_single_trial_inputs(doselevels, doseprobs, startdose, cohort, NN, theta)
  check_simulation_controls(theta, deltastop, burn, mm, stopping_rule_selected,
                            num_patients_restrict, trial_mode, alpha_increment_cutoff)
  check_prior_parameters(opt1)
  if (!is.numeric(alpha) || length(alpha) < 1L || any(!is.finite(alpha)) ||
      any(alpha < 0 | alpha > 0.5) || any(diff(alpha) < 0)) {
    stop("alpha must be a nondecreasing numeric vector between 0 and 0.5.", call. = FALSE)
  }
  check_numeric_scalar(incr, "incr", lower = 0, upper = 1, lower_open = TRUE)
  if (identical(trial_mode, "Accelerated EWOC")) {
    check_numeric_scalar(max_accel_dose_level, "max_accel_dose_level", lower = startdose,
                         upper = length(doselevels), integer = TRUE)
  }
  if (identical(trial_mode, "Accelerated EWOC")) {
    return(run_single_trial_simulation_accelerated(
      trial_id = trial_id,
      doselevels = doselevels,
      doseprobs = doseprobs,
      startdose = startdose,
      cohort = cohort,
      NN = NN,
      alpha = alpha,
      theta = theta,
      deltastop = deltastop,
      burn = burn,
      mm = mm,
      chains = chains,
      opt1 = opt1,
      stopping_rule_selected = stopping_rule_selected,
      num_patients_restrict = num_patients_restrict,
      incr = incr,
      max_accel_dose_level = max_accel_dose_level,
      alpha_increment_cutoff = alpha_increment_cutoff
    ))
  }

  run_single_trial_simulation_standard(
    trial_id = trial_id,
    doselevels = doselevels,
    doseprobs = doseprobs,
    startdose = startdose,
    cohort = cohort,
    NN = NN,
    alpha = alpha,
    theta = theta,
    deltastop = deltastop,
    burn = burn,
    mm = mm,
    chains = chains,
    opt1 = opt1,
    stopping_rule_selected = stopping_rule_selected,
    num_patients_restrict = num_patients_restrict,
    incr = incr,
    alpha_increment_cutoff = alpha_increment_cutoff
  )
}

## Trial logic: Build one row per stopped trial classifying which stopping rule(s) fired, from the trial's recorded stop-n values (post-trial summary, one-drug shared)
extract_stopped_trials_df <- function(sim_res, doselevels, dose_labels = NULL) {
  finite_scalar <- function(x) {
    length(x) == 1L && is.finite(x)
  }
  successful_idx <- which(vapply(sim_res, function(res) isTRUE(res$success), logical(1)))
  stopped_idx <- successful_idx[vapply(sim_res[successful_idx], function(res) {
    isTRUE(as.integer(res$stopval) == 1L) || finite_scalar(res$first_delta_n)
  }, logical(1))]
  
  if (length(stopped_idx) == 0) {
    return(data.frame(
      Trial = integer(0),
      Trial_Mode = character(0),
      Stop_Type = character(0),
      Actual_Stop_N = integer(0),
      Delta_Stop_N = integer(0),
      Max_Patient_Stop_N = integer(0),
      Accelerated_Patients = integer(0),
      Accelerated_Stop_Reason = character(0),
      EWOC_Entry_Dose = character(0),
      N_Dose_Values = integer(0),
      Dose_Values = character(0),
      DLT_Values = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, lapply(stopped_idx, function(tt) {
    x_vals <- if (!is.null(sim_res[[tt]]$actual_dose)) sim_res[[tt]]$actual_dose else sim_res[[tt]]$dose
    y_vals <- sim_res[[tt]]$dlt
    first_delta_stop_n <- sim_res[[tt]]$first_delta_n
    first_allocation_stop_n <- sim_res[[tt]]$first_stop_rule_n
    actual_stop_n <- as.integer(sim_res[[tt]]$nn_eff)
    stop_reasons <- character(0)
    if (finite_scalar(first_delta_stop_n) && actual_stop_n == as.integer(first_delta_stop_n)) {
      stop_reasons <- c(stop_reasons, "Delta")
    }
    if (finite_scalar(first_allocation_stop_n) && actual_stop_n == as.integer(first_allocation_stop_n)) {
      stop_reasons <- c(stop_reasons, "Maximum patient allocation")
    }
    if (length(stop_reasons) == 0L) {
      stop_reasons <- c(
        if (finite_scalar(first_delta_stop_n)) "Delta",
        if (finite_scalar(first_allocation_stop_n)) "Maximum patient allocation"
      )
    }
    stop_type <- if (length(stop_reasons) == 0L) "Unknown" else paste(unique(stop_reasons), collapse = " and ")
    
    dose_level_vals <- if (length(doselevels) > 0 && !is.null(dose_labels)) {
      label_discrete_dose_values(sim_res[[tt]]$dose, doselevels, dose_labels)
    } else if (length(doselevels) > 0) {
      vapply(
        sim_res[[tt]]$dose,
        function(x) paste("Dose", which.min(abs(doselevels - x))),
        character(1)
      )
    } else {
      round(x_vals, 4)
    }
    ewoc_entry <- if (!is.null(sim_res[[tt]]$ewoc_entry_dose)) sim_res[[tt]]$ewoc_entry_dose else NA_real_
    ewoc_entry_display <- if (length(doselevels) > 0 && !is.null(dose_labels) && is.finite(ewoc_entry)) {
      entry_idx <- as.integer(ewoc_entry)
      if (entry_idx >= 1L && entry_idx <= length(dose_labels)) dose_labels[entry_idx] else as.character(ewoc_entry)
    } else {
      as.character(ewoc_entry)
    }
    
    data.frame(
      Trial = if (!is.null(sim_res[[tt]]$trial_id)) sim_res[[tt]]$trial_id else tt,
      Trial_Mode = if (!is.null(sim_res[[tt]]$trial_mode)) sim_res[[tt]]$trial_mode else "Standard EWOC",
      Stop_Type = stop_type,
      Actual_Stop_N = actual_stop_n,
      Delta_Stop_N = if (finite_scalar(first_delta_stop_n)) as.integer(first_delta_stop_n) else NA_integer_,
      Max_Patient_Stop_N = if (finite_scalar(first_allocation_stop_n)) as.integer(first_allocation_stop_n) else NA_integer_,
      Accelerated_Patients = if (!is.null(sim_res[[tt]]$accel_patients)) sim_res[[tt]]$accel_patients else NA_integer_,
      Accelerated_Stop_Reason = if (!is.null(sim_res[[tt]]$accel_stop_reason)) sim_res[[tt]]$accel_stop_reason else NA_character_,
      EWOC_Entry_Dose = ewoc_entry_display,
      N_Dose_Values = length(x_vals),
      Dose_Values = paste(dose_level_vals, collapse = ", "),
      DLT_Values = paste(y_vals, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
}

## Trial logic: folder of per-trial completion markers next to the progress file (live progress), or NULL.
trial_progress_done_dir <- function(progress_path) {
  if (!is.character(progress_path) || length(progress_path) != 1L || !nzchar(progress_path)) return(NULL)
  paste0(progress_path, "_done")
}

## Trial logic: run one simulated trial for the background runner and leave a completion marker in
## done_dir, so the app can count finished trials while a parallel batch is still running.
run_trial_and_mark_done <- function(engine, kk, args, seed, done_dir = NULL) {
  restore_random_state <- save_random_state()
  on.exit(restore_random_state(), add = TRUE)
  set.seed(seed)
  result <- do.call(engine, c(list(trial_id = kk), args))
  if (is.character(done_dir) && length(done_dir) == 1L && nzchar(done_dir)) {
    try(file.create(file.path(done_dir, paste0(kk, ".done"))), silent = TRUE)
  }
  result
}

## Trial logic: the function each parallel worker runs for one trial. From the installed package its
## environment is the MEWOC namespace (sent to the workers by name; each worker loads the package). In the
## stand-alone app it is the global environment, where every worker has sourced trial_logic.R. Either way
## sending it to a worker sends only the function itself. (A function defined inside run_trial_simulations_background would carry that call's
## environment and everything it encloses, which in the app made every task slow to send.)
trial_worker_task <- function() {
  task <- function(kk, params) {
    run_trial_and_mark_done(params$engine, kk, params$common_args, params$simulation_seeds[[kk]], params$done_dir)
  }
  task <- utils::removeSource(task)
  code_env <- topenv(environment(trial_worker_task))
  environment(task) <- if (is_mewoc_namespace(code_env)) code_env else globalenv()
  task
}

## Trial logic: number of finished trials from the completion markers (0 when none).
count_trial_progress_done <- function(progress_path) {
  done_dir <- trial_progress_done_dir(progress_path)
  if (is.null(done_dir) || !dir.exists(done_dir)) return(0L)
  length(list.files(done_dir, pattern = "[.]done$"))
}

write_trial_progress <- function(progress_path, completed, requested, status = "running",
                                 launched = NULL) {
  if (is.null(progress_path) || !nzchar(progress_path)) {
    return(invisible(NULL))
  }
  
  if (is.null(launched) || !is.finite(launched)) {
    launched <- completed
  }
  
  saveRDS(
    list(
      completed = as.integer(completed),
      launched = as.integer(launched),
      requested = as.integer(requested),
      status = as.character(status),
      updated_at = Sys.time()
    ),
    file = progress_path
  )
  invisible(NULL)
}

## Trial logic: Assemble the final stopping-rule summary table (% stopped, count, average sample size with 95% CI) for display and download (post-trial summary, one-drug shared)
build_stopping_summary_table <- function(pstop_overall, n_overall_stop, overall_avg_n_ci_text,
                                         pstop_delta, n_delta_stop, avg_n_delta_ci_text,
                                         pstop_rule, n_allocation_stop, avg_n_stop_rule_ci_text,
                                         stopping_rule_selected) {
  rows <- list(
    list(label = "Overall", pct = round(pstop_overall * 100, 3), count = n_overall_stop, avg_sample = overall_avg_n_ci_text),
    list(label = "Safety Stop: minimum dose level too toxic", pct = round(pstop_delta * 100, 3), count = n_delta_stop, avg_sample = avg_n_delta_ci_text)
  )
  if (isTRUE(stopping_rule_selected)) {
    rows <- c(rows, list(list(
      label = "Stopping for trial efficiency",
      pct = round(pstop_rule * 100, 3),
      count = n_allocation_stop,
      avg_sample = avg_n_stop_rule_ci_text
    )))
  }
  data.frame(
    ` ` = vapply(rows, function(row) row$label, character(1)),
    `% Trial Stopped` = vapply(rows, function(row) as.character(row$pct), character(1)),
    `Stopped Count` = vapply(rows, function(row) as.character(row$count), character(1)),
    `Average Sample Size (95% CI)` = vapply(rows, function(row) as.character(row$avg_sample), character(1)),
    check.names = FALSE
  )
}

## Trial logic: Flatten a wide stopping-summary table into long Parameter/Value rows for the combined CSV export (post-trial summary, one-drug shared)
flatten_stopping_summary_table_to_long <- function(stopping_summary_table, section_name = "Stopping Summary") {
  if (is.null(stopping_summary_table) || nrow(stopping_summary_table) == 0L) {
    return(data.frame(Section = character(0), Parameter = character(0), Value = character(0), stringsAsFactors = FALSE))
  }
  metric_cols <- names(stopping_summary_table)[-1]
  data.frame(
    Section = section_name,
    Parameter = paste0(
      rep(stopping_summary_table[[1]], each = length(metric_cols)), " - ",
      rep(metric_cols, times = nrow(stopping_summary_table))
    ),
    Value = as.vector(t(as.matrix(stopping_summary_table[, metric_cols, drop = FALSE]))),
    stringsAsFactors = FALSE
  )
}

## Trial logic: save the session's random-number state and return a function that restores it, so
## functions that call set.seed() for reproducibility leave the caller's random numbers unchanged.
save_random_state <- function() {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  function() {
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }
}

## Trial logic: TRUE when the code runs from the MEWOC package namespace (not from the app's sourced files).
is_mewoc_namespace <- function(env) {
  isNamespace(env) && identical(unname(getNamespaceName(env)), "MEWOC")
}

## Trial logic: prepare the parallel workers. From the installed package each worker gets the same library
## paths and loads MEWOC (the worker task refers to the package namespace), so no file is read from the
## working directory. The stand-alone app, which sources its own R files, has the workers source the app's
## trial_logic.R instead.
setup_trial_workers <- function(cl) {
  if (is_mewoc_namespace(topenv(environment(setup_trial_workers)))) {
    parallel::clusterCall(cl, .libPaths, .libPaths())
    parallel::clusterEvalQ(cl, {
      if (!requireNamespace("rjags", quietly = TRUE) || !requireNamespace("coda", quietly = TRUE)) stop("Packages rjags and coda are required.", call. = FALSE)
      if (!requireNamespace("MEWOC", quietly = TRUE)) stop("Package MEWOC must be installed for parallel simulations.", call. = FALSE)
      NULL
    })
  } else {
    worker_trial_logic_path <- trial_logic_worker_path()
    parallel::clusterExport(cl, varlist = "worker_trial_logic_path", envir = environment())
    parallel::clusterEvalQ(cl, {
      if (!requireNamespace("rjags", quietly = TRUE) || !requireNamespace("coda", quietly = TRUE)) stop("Packages rjags and coda are required.", call. = FALSE)
      source(worker_trial_logic_path, local = .GlobalEnv)
      NULL
    })
  }
  invisible(cl)
}

## Trial logic: argument checks for run_single_trial_simulation() (clear messages for invalid input).
check_numeric_scalar <- function(value, field, lower = -Inf, upper = Inf,
                                 integer = FALSE, lower_open = FALSE, upper_open = FALSE) {
  valid <- is.numeric(value) && length(value) == 1L && is.finite(value)
  if (valid) valid <- if (lower_open) value > lower else value >= lower
  if (valid) valid <- if (upper_open) value < upper else value <= upper
  if (valid && integer) valid <- value == round(value) && abs(value) <= .Machine$integer.max
  if (!valid) stop(field, " must be a finite ", if (integer) "integer" else "number",
                   " in the permitted range ", if (lower_open) "(" else "[", lower, ", ", upper,
                   if (upper_open) ")." else "].", call. = FALSE)
  invisible(TRUE)
}

check_prior_parameters <- function(opt1) {
  if (!is.list(opt1) || (is.data.frame(opt1) && nrow(opt1) != 1L)) {
    stop("opt1 must be a list or one-row data frame of prior hyperparameters.", call. = FALSE)
  }
  for (field in c("a0", "b0", "a1", "b1")) {
    check_numeric_scalar(opt1[[field]], paste0("opt1$", field), lower = 0, lower_open = TRUE)
  }
  invisible(TRUE)
}

check_simulation_controls <- function(theta, deltastop, burn, mm, stopping_rule_selected,
                                      num_patients_restrict, trial_mode, alpha_increment_cutoff) {
  check_numeric_scalar(theta, "theta", 0, 1, lower_open = TRUE, upper_open = TRUE)
  check_numeric_scalar(deltastop, "deltastop", 0, 1)
  check_numeric_scalar(burn, "burn", 0, integer = TRUE)
  check_numeric_scalar(mm, "mm", 1, integer = TRUE)
  if (!is.logical(stopping_rule_selected) || length(stopping_rule_selected) != 1L || is.na(stopping_rule_selected)) {
    stop("stopping_rule_selected must be TRUE or FALSE.", call. = FALSE)
  }
  if (stopping_rule_selected) check_numeric_scalar(num_patients_restrict, "num_patients_restrict", 1, integer = TRUE)
  if (!is.character(trial_mode) || length(trial_mode) != 1L || is.na(trial_mode) ||
      !trial_mode %in% c("Standard EWOC", "Accelerated EWOC")) {
    stop('trial_mode must be "Standard EWOC" or "Accelerated EWOC".', call. = FALSE)
  }
  check_numeric_scalar(alpha_increment_cutoff, "alpha_increment_cutoff", 0, theta, lower_open = TRUE)
  invisible(TRUE)
}

check_single_trial_inputs <- function(doselevels, doseprobs, startdose, cohort, NN, theta) {
  is_num1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!is.numeric(doselevels) || length(doselevels) < 2L || any(!is.finite(doselevels)) ||
      any(doselevels < 0 | doselevels > 1) || any(diff(doselevels) <= 0)) {
    stop("doselevels must be strictly increasing finite standardized doses between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(doseprobs) || length(doseprobs) != length(doselevels) || anyNA(doseprobs) || any(doseprobs < 0 | doseprobs > 1)) {
    stop("doseprobs must give one true DLT probability between 0 and 1 for each dose level.", call. = FALSE)
  }
  if (!is_num1(startdose) || startdose != round(startdose) || startdose < 1 || startdose > length(doselevels)) {
    stop("startdose must be a dose level between 1 and the number of dose levels.", call. = FALSE)
  }
  check_numeric_scalar(cohort, "cohort", 1, integer = TRUE)
  check_numeric_scalar(NN, "NN", 1, integer = TRUE)
  if (!is_num1(theta) || theta <= 0 || theta >= 1) stop("theta must be a single number strictly between 0 and 1.", call. = FALSE)
  invisible(TRUE)
}

## Trial logic: required fields of trial_inputs for run_trial_simulations_background(); returns trial_inputs
## (for continuous designs with the true curve derived when only true_mtd and true_p_min are given).
normalize_continuous_scenario <- function(trial_inputs) {
  num1 <- function(x) {
    if (is.numeric(x) && length(x) == 1L && is.finite(x)) x else NA_real_
  }
  min_dose <- num1(trial_inputs$min_dose)
  max_dose <- num1(trial_inputs$max_dose)
  theta <- num1(trial_inputs$theta)
  if (!is.finite(min_dose) || !is.finite(max_dose) || max_dose <= min_dose) {
    stop("Continuous min_dose and max_dose must be finite, with min_dose < max_dose.", call. = FALSE)
  }
  if (!is.finite(theta) || theta <= 0 || theta >= 1) {
    stop("theta must be a single number strictly between 0 and 1.", call. = FALSE)
  }
  beta0 <- num1(trial_inputs$true_beta0)
  beta1 <- num1(trial_inputs$true_beta1)
  supplied <- function(x) !is.null(x) && !(length(x) == 1L && is.na(x))
  if (supplied(trial_inputs$true_beta0) != supplied(trial_inputs$true_beta1)) {
    stop("The true dose-toxicity curve requires both true_beta0 and true_beta1, or true_mtd and true_p_min.", call. = FALSE)
  }
  for (field in c("true_beta0", "true_beta1", "true_mtd", "true_p_min", "true_p_max")) {
    if (supplied(trial_inputs[[field]])) check_numeric_scalar(trial_inputs[[field]], field)
  }
  if (!is.finite(beta0) || !is.finite(beta1)) {
    if (is.finite(num1(trial_inputs$true_p_min)) &&
        (trial_inputs$true_p_min <= 0 || trial_inputs$true_p_min >= 1)) {
      stop("true_p_min must be strictly between 0 and 1 to define the true dose-toxicity curve.", call. = FALSE)
    }
    mtd_std <- (num1(trial_inputs$true_mtd) - min_dose) / (max_dose - min_dose)
    beta0 <- stats::qlogis(num1(trial_inputs$true_p_min))
    beta1 <- (stats::qlogis(theta) - beta0) / mtd_std
  }
  if (!is.finite(beta0) || !is.finite(beta1) || beta1 <= 0) {
    stop("Continuous simulations need the true dose-toxicity curve: give true_beta0 and true_beta1, or true_mtd and true_p_min that define an increasing curve. See ?run_trial_simulations_background.", call. = FALSE)
  }
  trial_inputs$true_beta0 <- beta0
  trial_inputs$true_beta1 <- beta1
  expected <- list(true_mtd = min_dose + (max_dose - min_dose) * (stats::qlogis(theta) - beta0) / beta1,
                   true_p_min = stats::plogis(beta0), true_p_max = stats::plogis(beta0 + beta1))
  for (field in names(expected)) {
    value <- trial_inputs[[field]]
    if (!is.null(value) && !(length(value) == 1L && is.na(value)) &&
        (!is.finite(num1(value)) || !isTRUE(all.equal(value, expected[[field]], tolerance = 1e-7, check.attributes = FALSE)))) {
      stop(field, " is inconsistent with the true dose-toxicity curve.", call. = FALSE)
    }
  }
  if (!is.finite(num1(trial_inputs$true_mtd))) {
    trial_inputs$true_mtd <- min_dose + (max_dose - min_dose) * (stats::qlogis(theta) - beta0) / beta1
  }
  if (!is.finite(num1(trial_inputs$true_p_min))) trial_inputs$true_p_min <- stats::plogis(beta0)
  if (!is.finite(num1(trial_inputs$true_p_max))) trial_inputs$true_p_max <- stats::plogis(beta0 + beta1)
  trial_inputs
}

check_trial_inputs <- function(trial_inputs) {
  if (!is.list(trial_inputs) || is.null(names(trial_inputs)) || anyNA(names(trial_inputs)) ||
      any(!nzchar(names(trial_inputs))) || anyDuplicated(names(trial_inputs))) {
    stop("trial_inputs must be a named list with unique, nonempty names.", call. = FALSE)
  }
  if (!identical(trial_inputs$dose_type, "Discrete") && !identical(trial_inputs$dose_type, "Continuous")) {
    stop("trial_inputs$dose_type must be \"Discrete\" or \"Continuous\".", call. = FALSE)
  }
  required <- c("M", "NN", "cohort", "dose_type", "theta", "deltastop", "lower_f_bound", "increment",
                "burn", "mm", "seed", "trial_mode", "stopping_rule_selected",
                if (identical(trial_inputs$dose_type, "Continuous")) c("min_dose", "max_dose") else c("num_doses", "startdose", "doseprobs"))
  missing_fields <- setdiff(required, names(trial_inputs))
  if (length(missing_fields) > 0L) {
    stop("trial_inputs is missing: ", paste(missing_fields, collapse = ", "), ". See ?run_trial_simulations_background.", call. = FALSE)
  }
  for (field in c("M", "NN", "cohort")) check_numeric_scalar(trial_inputs[[field]], field, 1, integer = TRUE)
  check_numeric_scalar(trial_inputs$seed, "seed", 0, integer = TRUE)
  if (is.null(trial_inputs$alpha_increment_cutoff)) trial_inputs$alpha_increment_cutoff <- trial_inputs$theta
  check_simulation_controls(trial_inputs$theta, trial_inputs$deltastop, trial_inputs$burn, trial_inputs$mm,
                            trial_inputs$stopping_rule_selected, trial_inputs$num_patients_restrict,
                            trial_inputs$trial_mode, trial_inputs$alpha_increment_cutoff)
  check_numeric_scalar(trial_inputs$lower_f_bound, "lower_f_bound", 0, 0.5)
  check_numeric_scalar(trial_inputs$increment, "increment", 0, lower_open = TRUE)
  if (identical(trial_inputs$dose_type, "Continuous")) {
    if (!identical(trial_inputs$trial_mode, "Standard EWOC")) stop("Continuous trial_mode must be Standard EWOC.", call. = FALSE)
    check_numeric_scalar(trial_inputs$max_dose_increment, "max_dose_increment", 0, 100, lower_open = TRUE)
    if (trial_inputs$stopping_rule_selected) {
      check_numeric_scalar(trial_inputs$efficiency_stop_tolerance, "efficiency_stop_tolerance", 0, 100, lower_open = TRUE)
    }
    trial_inputs <- normalize_continuous_scenario(trial_inputs)
  } else {
    check_numeric_scalar(trial_inputs$num_doses, "num_doses", 2, integer = TRUE)
    check_single_trial_inputs(seq(0, 1, length.out = trial_inputs$num_doses), trial_inputs$doseprobs,
                              trial_inputs$startdose, trial_inputs$cohort, trial_inputs$NN, trial_inputs$theta)
    if (identical(trial_inputs$trial_mode, "Accelerated EWOC")) {
      check_numeric_scalar(trial_inputs$max_accel_dose_level, "max_accel_dose_level", trial_inputs$startdose,
                           trial_inputs$num_doses, integer = TRUE)
    }
  }
  trial_inputs
}

## Trial logic: Path to trial_logic.R for the parallel simulation workers -- the app's own copy when
## running from the app folder, otherwise the copy installed with the package.
trial_logic_worker_path <- function() {
  candidates <- c(
    file.path("R", "trial_logic.R"),
    system.file("shiny", "mewoc", "R", "trial_logic.R", package = "MEWOC")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0L) {
    stop("Could not find trial_logic.R for the simulation workers.", call. = FALSE)
  }
  normalizePath(candidates[1], winslash = "/")
}

get_trial_worker_count <- function(requested_M, ncores = NULL) {
  check_numeric_scalar(requested_M, "M", 1, integer = TRUE)
  if (!is.null(ncores)) check_numeric_scalar(ncores, "ncores", 1, integer = TRUE)
  
  cores_avail <- parallel::detectCores(logical = TRUE)
  if (is.na(cores_avail) || cores_avail < 1L) {
    cores_avail <- 1L
  }
  
  available_workers <- max(1L, as.integer(cores_avail - 1L))
  configured_max <- getOption("mewoc.max_trial_workers", NULL)
  if (is.null(configured_max)) configured_max <- .Machine$integer.max
  check_numeric_scalar(configured_max, "mewoc.max_trial_workers", 1, integer = TRUE)
  # R CMD check (and CRAN) limits packages to 2 cores: _R_CHECK_LIMIT_CORES_ is set during checks.
  limit_cores <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(limit_cores) && !identical(tolower(limit_cores), "false")) {
    configured_max <- min(configured_max, 2L)
  }
  
  as.integer(max(1L, min(requested_M, if (is.null(ncores)) available_workers else ncores, configured_max)))
}

get_app_trial_worker_count <- function(requested_M) {
  count <- get_trial_worker_count(requested_M)
  limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  # The app already uses a background coordinator; avoid nested clusters in checks.
  if (nzchar(limit) && tolower(limit) != "false") count <- 1L
  count
}

is_trial_stop_requested <- function(stop_flag_path) {
  is.character(stop_flag_path) && length(stop_flag_path) == 1L &&
    nzchar(stop_flag_path) && file.exists(stop_flag_path)
}

## Trial logic: Background/parallel execution wrapper -- dispatches many simulated trials by dose_type (Continuous via run_single_trial_simulation_standard_continuous directly, or Discrete via the run_single_trial_simulation dispatcher) across worker processes with progress reporting (trial loop orchestration, one-drug shared)
#' Run multiple single-agent MEWOC trial simulations
#'
#' Runs a batch of simulated trials with discrete or continuous doses, optionally in
#' parallel. This is the simulation engine of the MEWOC app; the seed in
#' `trial_inputs` makes results reproducible, and the same seed gives the same
#' results whatever the number of workers.
#'
#' `trial_inputs` is a named list with
#' * `M` number of simulated trials; `NN` maximum sample size; `cohort` cohort size;
#' * `dose_type` `"Discrete"` or `"Continuous"`;
#' * `theta` target DLT probability; `deltastop` safety stopping threshold;
#' * `lower_f_bound` starting feasibility bound alpha and `increment` its increase
#'   per cohort (up to 0.5); `alpha_increment_cutoff` alpha increases only while
#'   the observed DLT rate is below this value (usually `theta`);
#' * `burn`, `mm` MCMC burn-in and retained samples; `seed` random seed;
#' * `trial_mode` `"Standard EWOC"` or `"Accelerated EWOC"` (discrete only) and
#'   `max_accel_dose_level`;
#' * `stopping_rule_selected` (logical) and `num_patients_restrict` for the
#'   efficiency stopping rule;
#' * discrete designs: `num_doses`, `startdose` and `doseprobs` (true DLT
#'   probability at each level);
#' * continuous designs: `min_dose`, `max_dose`, `max_dose_increment` (largest
#'   escalation step, in percent of the dose range), `efficiency_stop_tolerance`
#'   (dose-region width for the efficiency rule, in percent of the dose range), and
#'   the true dose-toxicity curve as `true_mtd` and `true_p_min` (true DLT
#'   probability at the minimum dose) or as `true_beta0` and `true_beta1`.
#'
#' @param trial_inputs Named list of design settings, true scenario and simulation
#'   controls (see Details).
#' @param opt1 Prior hyperparameters with elements `a0`, `b0`, `a1`, `b1`, for
#'   example the result of [run_mewoc_grid_search()].
#' @param stop_flag_path Optional path of a file whose existence stops the batch
#'   (used by the app's Stop button).
#' @param progress_path Optional path where progress information is written (used
#'   by the app).
#' @param ncores Number of parallel worker processes. By default all cores but
#'   one (at most 2 during `R CMD check`); the option `mewoc.max_trial_workers`
#'   sets an upper limit, including when `ncores` is supplied explicitly. Explicit
#'   counts must be positive integers and also respect the check-time two-core cap.
#'
#' @return A list with `status` (`"completed"`, `"stopped"` or `"failed"`),
#'   `results` (a list with one element per trial, as returned by
#'   [run_single_trial_simulation()]), `completed_M` and `requested_M`, and, when
#'   a trial failed, `failed_trials`. Continuous batches also include `trial_inputs`,
#'   the normalized design and true scenario used for simulation. The summary
#'   function uses this record to check the supplied scenario for conflicts.
#'
#' @seealso [summarize_trial_simulation_results()] for operating characteristics.
#' @examples
#' prior <- data.frame(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)
#' inputs <- list(M = 2, NN = 6, cohort = 3, dose_type = "Discrete", num_doses = 4,
#'                startdose = 1, doseprobs = c(0.05, 0.15, 0.33, 0.50), theta = 0.33,
#'                deltastop = 0.85, lower_f_bound = 0.25, increment = 0.05,
#'                alpha_increment_cutoff = 0.33, burn = 200, mm = 300, seed = 988,
#'                trial_mode = "Standard EWOC", max_accel_dose_level = NA,
#'                stopping_rule_selected = FALSE, num_patients_restrict = NA)
#' sims <- run_trial_simulations_background(inputs, prior, ncores = 1)
#' sims$status
#' sapply(sims$results, function(trial) trial$vmtd_level)
#' @export
run_trial_simulations_background <- function(trial_inputs, opt1,
                                             stop_flag_path = NULL,
                                             progress_path = NULL,
                                             ncores = NULL) {
  if (!requireNamespace("rjags", quietly = TRUE) || !requireNamespace("coda", quietly = TRUE)) stop("Packages rjags and coda are required.", call. = FALSE)
  trial_inputs <- check_trial_inputs(trial_inputs)
  check_prior_parameters(opt1)
  get_trial_worker_count(trial_inputs$M, ncores)
  restore_random_state <- save_random_state()
  on.exit(restore_random_state(), add = TRUE)
  
  requested_M <- as.integer(trial_inputs$M)
  done_dir <- trial_progress_done_dir(progress_path)
  if (!is.null(done_dir)) dir.create(done_dir, showWarnings = FALSE, recursive = TRUE)
  if (identical(trial_inputs$dose_type, "Continuous")) {
    NN <- as.integer(trial_inputs$NN)
    cohort <- as.integer(trial_inputs$cohort)
    trial_mode <- as.character(trial_inputs$trial_mode)
    alpha_updates <- max(1L, ceiling(NN / cohort))
    alpha <- build_alpha_sequence(trial_inputs$lower_f_bound, trial_inputs$increment, alpha_updates)
    chains <- 1L

    ncores <- get_trial_worker_count(requested_M, ncores)
    batch_size <- ncores
    
    set.seed(as.integer(trial_inputs$seed))
    simulation_seeds <- sample.int(.Machine$integer.max, size = requested_M, replace = FALSE)
    common_args <- list(
      min_dose = trial_inputs$min_dose,
      max_dose = trial_inputs$max_dose,
      true_beta0 = trial_inputs$true_beta0,
      true_beta1 = trial_inputs$true_beta1,
      true_mtd = trial_inputs$true_mtd,
      true_p_min = trial_inputs$true_p_min,
      true_p_max = trial_inputs$true_p_max,
      max_dose_increment = trial_inputs$max_dose_increment,
      cohort = cohort,
      NN = NN,
      alpha = alpha,
      theta = trial_inputs$theta,
      deltastop = trial_inputs$deltastop,
      burn = trial_inputs$burn,
      mm = trial_inputs$mm,
      chains = chains,
      opt1 = opt1,
      stopping_rule_selected = trial_inputs$stopping_rule_selected,
      num_patients_restrict = trial_inputs$num_patients_restrict,
      efficiency_stop_tolerance = trial_inputs$efficiency_stop_tolerance,
      alpha_increment_cutoff = trial_inputs$alpha_increment_cutoff
    )
    
    write_trial_progress(
      progress_path,
      completed = 0L,
      requested = requested_M,
      status = "running",
      launched = 0L
    )
    
    cl <- NULL
    if (ncores > 1L) {
      cl <- parallel::makeCluster(ncores)
      on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
      setup_trial_workers(cl)
    }
    
    sim_res <- list()
    completed <- 0L
    
    while (completed < requested_M) {
      if (is_trial_stop_requested(stop_flag_path)) {
        write_trial_progress(progress_path, completed = completed, requested = requested_M, status = "stopped", launched = completed)
        return(list(status = "stopped", results = sim_res, completed_M = completed, requested_M = requested_M,
                    trial_inputs = trial_inputs))
      }
      
      next_start <- completed + 1L
      next_end <- min(requested_M, next_start + batch_size - 1L)
      trial_ids <- seq.int(next_start, next_end)
      write_trial_progress(progress_path, completed = completed, requested = requested_M, status = "running", launched = next_end)
      
      batch_results <- tryCatch(
        if (is.null(cl)) {
          lapply(trial_ids, function(kk) {
            run_trial_and_mark_done("run_single_trial_simulation_standard_continuous", kk, common_args, simulation_seeds[[kk]], done_dir)
          })
        } else {
          parallel::parLapply(
            cl = cl,
            X = as.list(trial_ids),
            fun = trial_worker_task(),
            params = list(
              engine = "run_single_trial_simulation_standard_continuous",
              simulation_seeds = simulation_seeds,
              common_args = common_args,
              done_dir = done_dir
            )
          )
        },
        error = function(e) {
          list(list(success = FALSE, trial_id = trial_ids[1], error = conditionMessage(e)))
        }
      )
      
      failed_trials <- Filter(function(res) !isTRUE(res$success), batch_results)
      if (length(failed_trials) > 0) {
        sim_res <- c(sim_res, Filter(function(res) isTRUE(res$success), batch_results))
        completed <- length(sim_res)
        write_trial_progress(progress_path, completed = completed, requested = requested_M, status = "failed", launched = completed)
        return(list(status = "failed", results = sim_res, completed_M = completed, requested_M = requested_M,
                    failed_trials = failed_trials, trial_inputs = trial_inputs))
      }
      
      sim_res <- c(sim_res, batch_results)
      completed <- length(sim_res)
      write_trial_progress(progress_path, completed = completed, requested = requested_M, status = "running", launched = completed)
    }
    
    write_trial_progress(progress_path, completed = completed, requested = requested_M, status = "completed", launched = completed)
    return(list(status = "completed", results = sim_res, completed_M = completed, requested_M = requested_M,
                trial_inputs = trial_inputs))
  }
  
  num_doses <- as.integer(trial_inputs$num_doses)
  NN <- as.integer(trial_inputs$NN)
  cohort <- as.integer(trial_inputs$cohort)
  trial_mode <- as.character(trial_inputs$trial_mode)
  alpha_updates <- max(1L, ceiling(NN / cohort))
  alpha <- build_alpha_sequence(trial_inputs$lower_f_bound, trial_inputs$increment, alpha_updates)
  # Level-based dose positions; one-level escalation step.
  discrete_design <- prepare_discrete_simulation_design(trial_inputs)
  doselevels <- discrete_design$doselevels
  incr <- min(diff(sort(unique(doselevels))))
  trial_inputs$doseprobs <- discrete_design$doseprobs
  startdose <- discrete_design$startdose
  chains <- 1L
  
  ncores <- get_trial_worker_count(requested_M, ncores)
  batch_size <- ncores
  
  set.seed(as.integer(trial_inputs$seed))
  simulation_seeds <- sample.int(.Machine$integer.max, size = requested_M, replace = FALSE)
  common_args <- list(
    doselevels = doselevels,
    doseprobs = trial_inputs$doseprobs,
    startdose = startdose,
    cohort = cohort,
    NN = NN,
    alpha = alpha,
    theta = trial_inputs$theta,
    deltastop = trial_inputs$deltastop,
    burn = trial_inputs$burn,
    mm = trial_inputs$mm,
    chains = chains,
    opt1 = opt1,
    stopping_rule_selected = trial_inputs$stopping_rule_selected,
    num_patients_restrict = trial_inputs$num_patients_restrict,
    incr = incr,
    trial_mode = trial_mode,
    max_accel_dose_level = trial_inputs$max_accel_dose_level,
    alpha_increment_cutoff = trial_inputs$alpha_increment_cutoff
  )
  
  write_trial_progress(
    progress_path,
    completed = 0L,
    requested = requested_M,
    status = "running",
    launched = 0L
  )
  
  cl <- NULL
  if (ncores > 1L) {
    cl <- parallel::makeCluster(ncores)
    on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
    setup_trial_workers(cl)
  }
  
  sim_res <- list()
  completed <- 0L
  
  while (completed < requested_M) {
    if (is_trial_stop_requested(stop_flag_path)) {
      write_trial_progress(
        progress_path,
        completed = completed,
        requested = requested_M,
        status = "stopped",
        launched = completed
      )
      return(list(
        status = "stopped",
        results = sim_res,
        completed_M = completed,
        requested_M = requested_M
      ))
    }
    
    next_start <- completed + 1L
    next_end <- min(requested_M, next_start + batch_size - 1L)
    trial_ids <- seq.int(next_start, next_end)
    write_trial_progress(
      progress_path,
      completed = completed,
      requested = requested_M,
      status = "running",
      launched = next_end
    )
    
    batch_results <- tryCatch(
      if (is.null(cl)) {
        lapply(trial_ids, function(kk) {
          run_trial_and_mark_done("run_single_trial_simulation", kk, common_args, simulation_seeds[[kk]], done_dir)
        })
      } else {
        parallel::parLapply(
          cl = cl,
          X = as.list(trial_ids),
          fun = trial_worker_task(),
          params = list(
            engine = "run_single_trial_simulation",
            simulation_seeds = simulation_seeds,
            common_args = common_args,
            done_dir = done_dir
          )
        )
      },
      error = function(e) {
        list(list(
          success = FALSE,
          trial_id = trial_ids[1],
          error = conditionMessage(e)
        ))
      }
    )
    
    failed_trials <- Filter(function(res) !isTRUE(res$success), batch_results)
    if (length(failed_trials) > 0) {
      sim_res <- c(sim_res, Filter(function(res) isTRUE(res$success), batch_results))
      completed <- length(sim_res)
      write_trial_progress(
        progress_path,
        completed = completed,
        requested = requested_M,
        status = "failed",
        launched = completed
      )
      return(list(
        status = "failed",
        results = sim_res,
        completed_M = completed,
        requested_M = requested_M,
        failed_trials = failed_trials
      ))
    }
    
    sim_res <- c(sim_res, batch_results)
    completed <- length(sim_res)
    write_trial_progress(
      progress_path,
      completed = completed,
      requested = requested_M,
      status = "running",
      launched = completed
    )
  }
  
  write_trial_progress(
    progress_path,
    completed = completed,
    requested = requested_M,
    status = "completed",
    launched = completed
  )
  list(
    status = "completed",
    results = sim_res,
    completed_M = completed,
    requested_M = requested_M
  )
}

## Trial logic: Top-level post-trial summary -- stopping-rule percentages/CIs, dose allocation and MTD-recommendation tables, DLT rates (post-trial summary, one-drug shared)
#' Summarize single-agent MEWOC simulation results
#'
#' Computes operating characteristics from completed simulations: how often each
#' dose is selected as the MTD, patient allocation, DLT rates and stopping
#' summaries, as shown in the app and reports.
#'
#' @param sim_res The list returned by [run_trial_simulations_background()], or its
#'   `results` element (a list of single-trial results).
#' @param trial_inputs The `trial_inputs` list used for the simulations; for
#'   discrete designs it may also contain `actual_dose_levels` and `dose_unit` for
#'   the dose labels. Continuous scenarios accept either `true_mtd` and
#'   `true_p_min`, or `true_beta0` and `true_beta1`. For a continuous batch with
#'   recorded inputs, missing fields are filled from that record and conflicting
#'   scenario fields cause an error. Legacy batches and raw result lists require
#'   the scenario to be supplied here.
#'
#' @return A named list. Main elements: `combined_table` (MTD selection and
#'   allocation by dose), `dlt_summary` (DLT rates), `stopping_summary_table`
#'   (percentage of trials stopped and average sample sizes), `pstop_delta` and
#'   `pstop_rule` (proportion of trials stopped for safety and for efficiency),
#'   `completed_M` and `requested_M`; further elements hold per-trial values used
#'   by the app's reports.
#'
#' @examples
#' prior <- data.frame(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)
#' inputs <- list(M = 2, NN = 6, cohort = 3, dose_type = "Discrete", num_doses = 4,
#'                startdose = 1, doseprobs = c(0.05, 0.15, 0.33, 0.50), theta = 0.33,
#'                deltastop = 0.85, lower_f_bound = 0.25, increment = 0.05,
#'                alpha_increment_cutoff = 0.33, burn = 200, mm = 300, seed = 988,
#'                trial_mode = "Standard EWOC", max_accel_dose_level = NA,
#'                stopping_rule_selected = FALSE, num_patients_restrict = NA,
#'                actual_dose_levels = c(10, 20, 30, 40), dose_unit = "mg")
#' sims <- run_trial_simulations_background(inputs, prior, ncores = 1)
#' summary <- summarize_trial_simulation_results(sims, inputs)
#' summary$combined_table
#' @export
summarize_trial_simulation_results <- function(sim_res, trial_inputs) {
  # Accept the list returned by run_trial_simulations_background() as well as its $results element.
  if (is.list(sim_res) && is.list(sim_res$results) && !is.null(sim_res$status)) {
    recorded_inputs <- sim_res$trial_inputs
    if (is.list(recorded_inputs) && identical(recorded_inputs$dose_type, "Continuous")) {
      # Check supplied scenario fields before filling omitted values from the batch.
      scenario_fields <- c("dose_type", "min_dose", "max_dose", "theta", "true_mtd",
                           "true_p_min", "true_p_max", "true_beta0", "true_beta1")
      for (field in intersect(scenario_fields, names(trial_inputs))) {
        value <- trial_inputs[[field]]
        if (is.null(value) || (length(value) == 1L && is.na(value))) next
        if (!isTRUE(all.equal(value, recorded_inputs[[field]], check.attributes = FALSE))) {
          stop("trial_inputs$", field, " conflicts with the recorded continuous simulation scenario.", call. = FALSE)
        }
      }
      trial_inputs <- utils::modifyList(recorded_inputs, trial_inputs)
    }
    sim_res <- sim_res$results
  }
  successful_res <- Filter(function(res) isTRUE(res$success), sim_res)
  if (length(successful_res) == 0) {
    stop("No completed trial simulations are available to summarize.")
  }
  
  trial_order <- order(vapply(successful_res, function(res) as.integer(res$trial_id), integer(1)))
  successful_res <- successful_res[trial_order]
  
  requested_M <- as.integer(trial_inputs$M)
  completed_M <- length(successful_res)
  NN <- as.integer(trial_inputs$NN)
  if (identical(trial_inputs$dose_type, "Continuous")) {
    # Also support raw result lists and legacy batches without recorded inputs.
    trial_inputs <- normalize_continuous_scenario(trial_inputs)
    theta <- as.numeric(trial_inputs$theta)
    trial_mode <- as.character(trial_inputs$trial_mode)
    stopping_rule_selected <- isTRUE(trial_inputs$stopping_rule_selected)
    min_dose <- as.numeric(trial_inputs$min_dose)
    max_dose <- as.numeric(trial_inputs$max_dose)
    dose_unit <- as.character(trial_inputs$dose_unit)
    
    vrho0 <- numeric(completed_M)
    vrho1 <- numeric(completed_M)
    vmtd <- numeric(completed_M)
    vdltrate <- numeric(completed_M)
    effective_NN <- numeric(completed_M)
    first_delta_n <- rep(NA_real_, completed_M)
    accel_patients <- rep(NA_real_, completed_M)
    accel_stop_reason <- rep(NA_character_, completed_M)
    ewoc_entry_dose <- rep(NA_real_, completed_M)
    dose <- matrix(NA_real_, nrow = completed_M, ncol = NN)
    dlt <- matrix(NA_real_, nrow = completed_M, ncol = NN)
    postmind0 <- matrix(NA_real_, nrow = completed_M, ncol = NN)
    vstoppedtrials <- rep(NA_real_, completed_M)
    
    for (kk in seq_len(completed_M)) {
      current_nn <- as.integer(successful_res[[kk]]$nn_eff)
      effective_NN[kk] <- current_nn
      dose[kk, seq_len(current_nn)] <- successful_res[[kk]]$actual_dose
      dlt[kk, seq_len(current_nn)] <- successful_res[[kk]]$dlt
      postmind0[kk, ] <- successful_res[[kk]]$postmind0
      vrho0[kk] <- successful_res[[kk]]$vrho0
      vmtd[kk] <- successful_res[[kk]]$vmtd
      vrho1[kk] <- successful_res[[kk]]$vrho1
      vdltrate[kk] <- successful_res[[kk]]$vdltrate
      vstoppedtrials[kk] <- successful_res[[kk]]$stopval
      first_delta_n[kk] <- successful_res[[kk]]$first_delta_n
      accel_patients[kk] <- successful_res[[kk]]$accel_patients
      accel_stop_reason[kk] <- successful_res[[kk]]$accel_stop_reason
      ewoc_entry_dose[kk] <- successful_res[[kk]]$ewoc_entry_dose
    }
    
    pstop_delta <- sum(!is.na(first_delta_n)) / completed_M
    avg_n_delta <- if (all(is.na(first_delta_n))) NA_real_ else round(mean(first_delta_n, na.rm = TRUE), digits = 3)
    pstop_rule <- sum(as.numeric(vstoppedtrials) == 1, na.rm = TRUE) / completed_M
    stop_rule_samples <- effective_NN[as.numeric(vstoppedtrials) == 1]
    avg_n_stop_rule <- if (length(stop_rule_samples) == 0) NA_real_ else round(mean(stop_rule_samples, na.rm = TRUE), digits = 3)
    pstop <- if (stopping_rule_selected) pstop_rule else pstop_delta
    delta_stop_samples <- first_delta_n[!is.na(first_delta_n)]
    avg_n_delta_ci_text <- format_mean_ci(delta_stop_samples)
    avg_n_stop_rule_ci_text <- format_mean_ci(stop_rule_samples)
    overall_avg_n_ci_text <- format_mean_ci(effective_NN)
    
    overall_avg_n <- round(mean(effective_NN), digits = 3)
    total_patients_observed <- sum(effective_NN, na.rm = TRUE)
    avgdltrate <- if (total_patients_observed > 0) {
      sum(dlt == 1, na.rm = TRUE) / total_patients_observed
    } else {
      NA_real_
    }
    pctdltrategrtheta10 <- sum(vdltrate > theta + 0.1, na.rm = TRUE) / completed_M
    
    # Previous continuous dose-grid efficiency summary:
    # displayed_standardized <- build_continuous_increment_grid(trial_inputs$max_dose_increment)
    # displayed_doses <- round(actualize_standardized_dose(displayed_standardized, min_dose, max_dose), 4)
    # dose and MTD recommendations were grouped to the nearest increment-grid dose.
    vmtd_values <- as.numeric(vmtd[is.finite(vmtd)])
    true_mtd_actual <- suppressWarnings(as.numeric(trial_inputs$true_mtd))
    true_prob_at_estimated_mtd <- true_continuous_pdlt(
      standardize_actual_dose(vmtd_values, min_dose, max_dose),
      trial_inputs$true_beta0,
      trial_inputs$true_beta1
    )
    theta_tolerance <- 0.10 * theta
    percent_selection <- {
      # Protocol definition: selected if true P(DLT at estimated MTD) is within 10% of theta.
      # Denominator is all completed trials: trials stopped because the lowest dose is
      # too toxic recommend no MTD and count as not selected.
      100 * sum(abs(true_prob_at_estimated_mtd - theta) <= theta_tolerance, na.rm = TRUE) / completed_M
    }
    average_estimated_mtd <- if (length(vmtd_values) > 0) mean(vmtd_values, na.rm = TRUE) else NA_real_
    average_bias <- if (length(vmtd_values) > 0 && is.finite(true_mtd_actual)) {
      mean(vmtd_values - true_mtd_actual, na.rm = TRUE)
    } else {
      NA_real_
    }
    rmse <- if (length(vmtd_values) > 0 && is.finite(true_mtd_actual)) {
      sqrt(mean((vmtd_values - true_mtd_actual)^2, na.rm = TRUE))
    } else {
      NA_real_
    }
    toxic_alloc_counts <- vapply(seq_len(completed_M), function(kk) {
      assigned_doses <- dose[kk, seq_len(as.integer(effective_NN[kk]))]
      assigned_doses <- assigned_doses[is.finite(assigned_doses)]
      if (length(assigned_doses) == 0) {
        return(0)
      }
      assigned_true_prob <- true_continuous_pdlt(
        standardize_actual_dose(assigned_doses, min_dose, max_dose),
        trial_inputs$true_beta0,
        trial_inputs$true_beta1
      )
      sum(assigned_true_prob > theta, na.rm = TRUE)
    }, numeric(1))
    toxic_alloc_mean <- mean(toxic_alloc_counts, na.rm = TRUE)
    toxic_alloc_ci <- stats::quantile(
      toxic_alloc_counts,
      probs = c(0.05, 0.95),
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
    toxic_alloc_summary <- paste0(
      round(toxic_alloc_mean, 3),
      " (",
      round(toxic_alloc_ci[1], 3),
      ", ",
      round(toxic_alloc_ci[2], 3),
      ")"
    )
    
    combined_table <- data.frame(
      `True MTD` = round(true_mtd_actual, 4),
      `True MTD Standardized` = round(standardize_actual_dose(true_mtd_actual, min_dose, max_dose), 4),
      `Average Estimated MTD` = round(average_estimated_mtd, 4),
      `Average Bias` = round(average_bias, 4),
      RMSE = round(rmse, 4),
      `% Selection within 10% of theta` = round(percent_selection, 3),
      `Number allocated to toxic doses (mean, 90% CI)` = toxic_alloc_summary,
      check.names = FALSE
    )
    
    dlt_summary <- data.frame(
      Metric = c(
        "Average  % DLT rate",
        "% Trials: DLT rate  > \u03B8+0.10"
      ),
      Value = round(c(avgdltrate, pctdltrategrtheta10) * 100, digits = 3)
    )
    colnames(dlt_summary) <- c("DLT in Trial", "Value")
    
    stopped_df <- extract_stopped_trials_df(successful_res, numeric(0))
    stopping_summary_table <- build_stopping_summary_table(
      pstop_overall = mean(is.finite(first_delta_n) | as.numeric(vstoppedtrials) == 1, na.rm = TRUE),
      n_overall_stop = sum(is.finite(first_delta_n) | as.numeric(vstoppedtrials) == 1, na.rm = TRUE),
      overall_avg_n_ci_text = overall_avg_n_ci_text,
      pstop_delta = pstop_delta,
      n_delta_stop = sum(is.finite(first_delta_n)),
      avg_n_delta_ci_text = avg_n_delta_ci_text,
      pstop_rule = pstop_rule,
      n_allocation_stop = sum(as.numeric(vstoppedtrials) == 1, na.rm = TRUE),
      avg_n_stop_rule_ci_text = avg_n_stop_rule_ci_text,
      stopping_rule_selected = stopping_rule_selected
    )
    
    return(list(
      inputs = list(
        dose_type = "Continuous",
        M = requested_M,
        NN = trial_inputs$NN,
        cohort = trial_inputs$cohort,
        min_dose = min_dose,
        max_dose = max_dose,
        dose_unit = dose_unit,
        max_dose_increment = trial_inputs$max_dose_increment,
        true_mtd = trial_inputs$true_mtd,
        true_p_min = trial_inputs$true_p_min,
        true_p_max = trial_inputs$true_p_max,
        trial_mode = trial_mode,
        max_accel_dose_level = trial_inputs$max_accel_dose_level,
        deltastop = trial_inputs$deltastop,
        theta = theta,
        alpha_increment_cutoff = trial_inputs$alpha_increment_cutoff,
        use_custom_alpha_increment_cutoff = trial_inputs$use_custom_alpha_increment_cutoff,
        lower_f_bound = trial_inputs$lower_f_bound,
        increment = trial_inputs$increment,
        stopping_rule_selected = stopping_rule_selected,
        num_patients_restrict = trial_inputs$num_patients_restrict,
        efficiency_stop_tolerance = trial_inputs$efficiency_stop_tolerance,
        burn = trial_inputs$burn,
        mm = trial_inputs$mm,
        doseprobs = numeric(0)
      ),
      requested_M = requested_M,
      completed_M = completed_M,
      run_status = if (completed_M < requested_M) "stopped" else "completed",
      combined_table = combined_table,
      dlt_summary = dlt_summary,
      stopping_summary_table = stopping_summary_table,
      pstop_delta = pstop_delta,
      avg_n_delta = avg_n_delta,
      avg_n_delta_ci_text = avg_n_delta_ci_text,
      pstop_rule = pstop_rule,
      avg_n_stop_rule = avg_n_stop_rule,
      avg_n_stop_rule_ci_text = avg_n_stop_rule_ci_text,
      pstop = pstop,
      stopped_df = stopped_df,
      effective_NN = effective_NN,
      overall_avg_n = overall_avg_n,
      overall_avg_n_ci_text = overall_avg_n_ci_text,
      first_delta_n = first_delta_n,
      accel_patients = accel_patients,
      accel_stop_reason = accel_stop_reason,
      ewoc_entry_dose = ewoc_entry_dose,
      avgdltrate = avgdltrate,
      pctdltrategrtheta10 = pctdltrategrtheta10,
      vmtd = vmtd,
      sim_res = successful_res,
      generated_at = Sys.time()
    ))
  }
  
  num_doses <- as.integer(trial_inputs$num_doses)
  startdose <- as.integer(trial_inputs$startdose)
  theta <- as.numeric(trial_inputs$theta)
  doseprobs <- as.numeric(trial_inputs$doseprobs)
  trial_mode <- as.character(trial_inputs$trial_mode)
  stopping_rule_selected <- isTRUE(trial_inputs$stopping_rule_selected)
  dose_unit <- as.character(trial_inputs$dose_unit)
  # Same level-based design as the simulation run.
  discrete_design <- prepare_discrete_simulation_design(trial_inputs)
  doselevels <- discrete_design$doselevels
  doseprobs <- discrete_design$doseprobs
  actual_dose_levels <- discrete_design$actual_dose_levels
  num_doses <- length(doselevels)
  startdose <- discrete_design$startdose
  dose_labels <- build_discrete_dose_labels(actual_dose_levels, discrete_design$dose_units, num_doses)
  
  vrho0 <- numeric(completed_M)
  vrho1 <- numeric(completed_M)
  vmtd <- numeric(completed_M)
  vdltrate <- numeric(completed_M)
  effective_NN <- numeric(completed_M)
  first_delta_n <- rep(NA_real_, completed_M)
  accel_patients <- rep(NA_real_, completed_M)
  accel_stop_reason <- rep(NA_character_, completed_M)
  ewoc_entry_dose <- rep(NA_real_, completed_M)
  
  dose <- matrix(NA_real_, nrow = completed_M, ncol = NN)
  dlt <- matrix(NA_real_, nrow = completed_M, ncol = NN)
  postmind0 <- matrix(NA_real_, nrow = completed_M, ncol = NN)
  vstoppedtrials <- rep(NA_real_, completed_M)
  
  for (kk in seq_len(completed_M)) {
    current_nn <- as.integer(successful_res[[kk]]$nn_eff)
    effective_NN[kk] <- current_nn
    dose[kk, seq_len(current_nn)] <- successful_res[[kk]]$dose
    dlt[kk, seq_len(current_nn)] <- successful_res[[kk]]$dlt
    postmind0[kk, ] <- successful_res[[kk]]$postmind0
    vrho0[kk] <- successful_res[[kk]]$vrho0
    vmtd[kk] <- successful_res[[kk]]$vmtd
    vrho1[kk] <- successful_res[[kk]]$vrho1
    vdltrate[kk] <- successful_res[[kk]]$vdltrate
    vstoppedtrials[kk] <- successful_res[[kk]]$stopval
    first_delta_n[kk] <- successful_res[[kk]]$first_delta_n
    accel_patients[kk] <- successful_res[[kk]]$accel_patients
    accel_stop_reason[kk] <- successful_res[[kk]]$accel_stop_reason
    ewoc_entry_dose[kk] <- successful_res[[kk]]$ewoc_entry_dose
  }
  
  pstop_delta <- sum(!is.na(first_delta_n)) / completed_M
  avg_n_delta <- if (all(is.na(first_delta_n))) NA_real_ else round(mean(first_delta_n, na.rm = TRUE), digits = 3)
  pstop_rule <- sum(as.numeric(vstoppedtrials) == 1, na.rm = TRUE) / completed_M
  stop_rule_samples <- effective_NN[as.numeric(vstoppedtrials) == 1]
  avg_n_stop_rule <- if (length(stop_rule_samples) == 0) NA_real_ else round(mean(stop_rule_samples, na.rm = TRUE), digits = 3)
  pstop <- if (stopping_rule_selected) pstop_rule else pstop_delta
  
  delta_stop_samples <- first_delta_n[!is.na(first_delta_n)]
  avg_n_delta_ci_text <- format_mean_ci(delta_stop_samples)
  avg_n_stop_rule_ci_text <- format_mean_ci(stop_rule_samples)
  overall_avg_n_ci_text <- format_mean_ci(effective_NN)
  
  overall_avg_n <- round(mean(effective_NN), digits = 3)
  total_patients_observed <- sum(effective_NN, na.rm = TRUE)
  avgdltrate <- if (total_patients_observed > 0) {
    sum(dlt == 1, na.rm = TRUE) / total_patients_observed
  } else {
    NA_real_
  }
  
  pctdltrategrtheta10 <- sum(vdltrate > theta + 0.1, na.rm = TRUE) / completed_M
  
  doseproppt <- as.data.frame(table(dose) / total_patients_observed)
  doselevelmtd <- as.data.frame(table(vmtd))
  d_lev <- seq(from = 1, length.out = length(doselevels))
  
  combined_table <- data.frame(
    Dose_Level = dose_labels,
    Dose_Probability = doseprobs,
    Prop_Patients = seq(0, 0, length = length(doselevels)),
    Prop_Trials_MTD = seq(0, 0, length = length(doselevels))
  )
  
  if (is.data.frame(doseproppt)) {
    for (i in seq_along(doselevels)) {
      idx <- which(doseproppt$dose == doselevels[i])
      if (length(idx) > 0) {
        combined_table$Prop_Patients[i] <- round(100 * doseproppt$Freq[idx], digits = 2)
      }
    }
  }
  
  if (is.data.frame(doselevelmtd)) {
    for (i in seq_along(doselevels)) {
      idx <- which(doselevelmtd$vmtd == doselevels[i])
      if (length(idx) > 0) {
        combined_table$Prop_Trials_MTD[i] <- round(100 * doselevelmtd$Freq[idx] / completed_M, digits = 2)
      }
    }
  }
  
  combined_table$Prop_Patients <- round(combined_table$Prop_Patients, digits = 2)
  combined_table$Prop_Trials_MTD <- round(combined_table$Prop_Trials_MTD, digits = 2)
  colnames(combined_table) <- c("Actual Dose", "Probability", "% Allocation", "% MTD recommendation")
  combined_table <- round_trial_efficiency_percentages(combined_table)
  
  dlt_summary <- data.frame(
    Metric = c(
      "Average  % DLT rate",
      "% Trials: DLT rate  > \u03B8+0.10"
    ),
    Value = round(c(avgdltrate, pctdltrategrtheta10) * 100, digits = 3)
  )
  colnames(dlt_summary) <- c("DLT in Trial", "Value")
  
  stopped_df <- extract_stopped_trials_df(successful_res, doselevels, dose_labels)
  stopping_summary_table <- build_stopping_summary_table(
    pstop_overall = mean(is.finite(first_delta_n) | as.numeric(vstoppedtrials) == 1, na.rm = TRUE),
    n_overall_stop = sum(is.finite(first_delta_n) | as.numeric(vstoppedtrials) == 1, na.rm = TRUE),
    overall_avg_n_ci_text = overall_avg_n_ci_text,
    pstop_delta = pstop_delta,
    n_delta_stop = sum(is.finite(first_delta_n)),
    avg_n_delta_ci_text = avg_n_delta_ci_text,
    pstop_rule = pstop_rule,
    n_allocation_stop = sum(as.numeric(vstoppedtrials) == 1, na.rm = TRUE),
    avg_n_stop_rule_ci_text = avg_n_stop_rule_ci_text,
    stopping_rule_selected = stopping_rule_selected
  )
  
  list(
    inputs = list(
      M = requested_M,
      NN = trial_inputs$NN,
      cohort = trial_inputs$cohort,
      startdose = startdose,
      trial_mode = trial_mode,
      max_accel_dose_level = trial_inputs$max_accel_dose_level,
      deltastop = trial_inputs$deltastop,
      theta = theta,
      alpha_increment_cutoff = trial_inputs$alpha_increment_cutoff,
      use_custom_alpha_increment_cutoff = trial_inputs$use_custom_alpha_increment_cutoff,
      lower_f_bound = trial_inputs$lower_f_bound,
      increment = trial_inputs$increment,
      stopping_rule_selected = stopping_rule_selected,
      num_patients_restrict = trial_inputs$num_patients_restrict,
      burn = trial_inputs$burn,
      mm = trial_inputs$mm,
      doseprobs = doseprobs,
      actual_dose_levels = actual_dose_levels,
      actual_dose_units = discrete_design$dose_units,
      dose_unit = dose_unit
    ),
    requested_M = requested_M,
    completed_M = completed_M,
    run_status = if (completed_M < requested_M) "stopped" else "completed",
    combined_table = combined_table,
    dlt_summary = dlt_summary,
    stopping_summary_table = stopping_summary_table,
    pstop_delta = pstop_delta,
    avg_n_delta = avg_n_delta,
    avg_n_delta_ci_text = avg_n_delta_ci_text,
    pstop_rule = pstop_rule,
    avg_n_stop_rule = avg_n_stop_rule,
    avg_n_stop_rule_ci_text = avg_n_stop_rule_ci_text,
    pstop = pstop,
    stopped_df = stopped_df,
    effective_NN = effective_NN,
    overall_avg_n = overall_avg_n,
    overall_avg_n_ci_text = overall_avg_n_ci_text,
    first_delta_n = first_delta_n,
    accel_patients = accel_patients,
    accel_stop_reason = accel_stop_reason,
    ewoc_entry_dose = ewoc_entry_dose,
    avgdltrate = avgdltrate,
    pctdltrategrtheta10 = pctdltrategrtheta10,
    vmtd = vmtd,
    sim_res = successful_res,
    generated_at = Sys.time()
  )
}

## Trial logic: Flatten the full trial-simulation summary into one long Parameter/Value data frame for CSV export (post-trial summary, one-drug shared)
build_trial_summary_csv_df <- function(trial_data) {
  if (is.null(trial_data)) {
    return(data.frame(
      Section = "Status",
      Parameter = "Trial results",
      Value = "Trial simulation results have not been generated yet.",
      stringsAsFactors = FALSE
    ))
  }
  
  if (identical(trial_data$inputs$dose_type, "Continuous")) {
    rows <- list(
      data.frame(
        Section = "Input Parameters",
        Parameter = c(
          "Requested Number of Trials",
          "Completed Simulations Used in Summary",
          "Dose Type",
          "Total Sample Size",
          "Cohort Size",
          "Minimum Dose",
          "Maximum Dose",
          "Dose Unit",
          "Maximum Dose Increment (%)",
          "True MTD Dose",
          "True P(DLT) at Minimum Dose",
          "True P(DLT) at Maximum Dose",
          "Trial Mode",
          "Threshold, Delta",
          "Target Probability of DLT (theta)",
          "Alpha Increment Cutoff",
          "Custom Alpha Increment Cutoff Enabled",
          "Lower Feasibility Bound",
          "Feasibility Bound Increment",
          "Efficiency Stopping Rule Enabled",
          "Maximum Patients at Recommended Dose",
          "Dose-Region Width (%)",
          "Burn-in",
          "Samples"
        ),
        Value = c(
          trial_data$inputs$M,
          trial_data$completed_M,
          trial_data$inputs$dose_type,
          trial_data$inputs$NN,
          trial_data$inputs$cohort,
          trial_data$inputs$min_dose,
          trial_data$inputs$max_dose,
          trial_data$inputs$dose_unit,
          trial_data$inputs$max_dose_increment,
          trial_data$inputs$true_mtd,
          round(trial_data$inputs$true_p_min, 4),
          round(trial_data$inputs$true_p_max, 4),
          trial_data$inputs$trial_mode,
          trial_data$inputs$deltastop,
          trial_data$inputs$theta,
          trial_data$inputs$alpha_increment_cutoff,
          if (isTRUE(trial_data$inputs$use_custom_alpha_increment_cutoff)) "Yes" else "No",
          trial_data$inputs$lower_f_bound,
          trial_data$inputs$increment,
          if (isTRUE(trial_data$inputs$stopping_rule_selected)) "Yes" else "No",
          if (isTRUE(trial_data$inputs$stopping_rule_selected)) trial_data$inputs$num_patients_restrict else "Not applied",
          if (isTRUE(trial_data$inputs$stopping_rule_selected) && !is.null(trial_data$inputs$efficiency_stop_tolerance)) trial_data$inputs$efficiency_stop_tolerance else "Not applied",
          trial_data$inputs$burn,
          trial_data$inputs$mm
        ),
        stringsAsFactors = FALSE
      ),
      # SK: previously the CSV wrote its own ad-hoc "Overall Sample Size Summary",
      # "Delta-Based Summary" and
      # "Patient Allocation Stopping Rule Summary" sections here -- a standalone
      # overall-average-sample-size row plus separate pieces of what the UI now
      # shows as a single consolidated "Stopping Summary" table (with its own
      # "Overall" row). That duplicated the overall figure and could drift out of
      # sync with the actual Stopping Summary table. Now exports the same
      # stopping_summary_table the UI displays, so the numbers always match.
      flatten_stopping_summary_table_to_long(trial_data$stopping_summary_table)
    )
    
    if (!is.null(trial_data$combined_table) && nrow(trial_data$combined_table) > 0) {
      combined_table_rows <- do.call(rbind, lapply(seq_len(nrow(trial_data$combined_table)), function(i) {
        row_i <- trial_data$combined_table[i, , drop = FALSE]
        table_values <- paste(
          paste(names(row_i), as.character(row_i[1, ]), sep = " = "),
          collapse = "; "
        )
        data.frame(
          Section = "Table 1: Trial Efficiency Summary",
          Parameter = paste("Row", i),
          Value = table_values,
          stringsAsFactors = FALSE
        )
      }))
      rows <- c(rows, list(combined_table_rows))
    }
    
    if (!is.null(trial_data$dlt_summary) && nrow(trial_data$dlt_summary) > 0) {
      safety_rows <- data.frame(
        Section = "Table 2: Safety Summary",
        Parameter = trial_data$dlt_summary[[1]],
        Value = trial_data$dlt_summary[[2]],
        stringsAsFactors = FALSE
      )
      rows <- c(rows, list(safety_rows))
    }
    
    return(do.call(rbind, rows))
  }
  
  rows <- list(
    data.frame(
      Section = rep("Input Parameters", 17),
      Parameter = c(
        "Requested Number of Trials",
        "Completed Simulations Used in Summary",
        "Total Sample Size",
        "Cohort Size",
        "Starting Dose",
        "Trial Mode",
        "Maximum Dose for Acceleration",
        "Threshold, Delta",
        "Target Probability of DLT (theta)",
        "Alpha Increment Cutoff",
        "Custom Alpha Increment Cutoff Enabled",
        "Lower Feasibility Bound",
        "Feasibility Bound Increment",
        "Efficiency Stopping Rule Enabled",
        "Maximum Patients at Recommended Dose",
        "Burn-in",
        "Samples"
      ),
      Value = c(
        trial_data$inputs$M,
        trial_data$completed_M,
        trial_data$inputs$NN,
        trial_data$inputs$cohort,
        if (!is.null(trial_data$inputs$actual_dose_levels)) {
          build_discrete_dose_labels(
            trial_data$inputs$actual_dose_levels,
            summary_dose_units(trial_data$inputs),
            length(trial_data$inputs$actual_dose_levels)
          )[trial_data$inputs$startdose]
        } else {
          paste("Dose", trial_data$inputs$startdose)
        },
        trial_data$inputs$trial_mode,
        if (identical(trial_data$inputs$trial_mode, "Accelerated EWOC") && !is.null(trial_data$inputs$actual_dose_levels)) {
          build_discrete_dose_labels(
            trial_data$inputs$actual_dose_levels,
            summary_dose_units(trial_data$inputs),
            length(trial_data$inputs$actual_dose_levels)
          )[trial_data$inputs$max_accel_dose_level]
        } else if (identical(trial_data$inputs$trial_mode, "Accelerated EWOC")) {
          paste("Dose", trial_data$inputs$max_accel_dose_level)
        } else {
          "Not applied"
        },
        trial_data$inputs$deltastop,
        trial_data$inputs$theta,
        trial_data$inputs$alpha_increment_cutoff,
        if (isTRUE(trial_data$inputs$use_custom_alpha_increment_cutoff)) "Yes" else "No",
        trial_data$inputs$lower_f_bound,
        trial_data$inputs$increment,
        if (isTRUE(trial_data$inputs$stopping_rule_selected)) "Yes" else "No",
        if (isTRUE(trial_data$inputs$stopping_rule_selected)) trial_data$inputs$num_patients_restrict else "Not applied",
        trial_data$inputs$burn,
        trial_data$inputs$mm
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Section = "Input Parameters",
      Parameter = "Dose Probabilities",
      Value = paste(round(trial_data$inputs$doseprobs, 4), collapse = ", "),
      stringsAsFactors = FALSE
    ),
    # SK: previously the CSV wrote its own ad-hoc "Overall Sample Size Summary",
    # "Delta-Based Summary" and
    # "Patient Allocation Stopping Rule Summary" sections here -- a standalone
    # overall-average-sample-size row plus separate pieces of what the UI now
    # shows as a single consolidated "Stopping Summary" table (with its own
    # "Overall" row). That duplicated the overall figure and could drift out of
    # sync with the actual Stopping Summary table. Now exports the same
    # stopping_summary_table the UI displays, so the numbers always match.
    flatten_stopping_summary_table_to_long(trial_data$stopping_summary_table)
  )
  
  if (identical(trial_data$inputs$trial_mode, "Accelerated EWOC")) {
    rows <- c(rows, list(
      data.frame(
        Section = "Accelerated Startup Summary",
        Parameter = c(
          "Mean accelerated singleton patients",
          "Most common accelerated stop reason",
          "Most common EWOC entry dose"
        ),
        Value = c(
          round(mean(trial_data$accel_patients[is.finite(trial_data$accel_patients)]), 3),
          if (length(trial_data$accel_stop_reason) > 0) names(sort(table(trial_data$accel_stop_reason), decreasing = TRUE))[1] else NA_character_,
          if (length(trial_data$ewoc_entry_dose) > 0 && !is.null(trial_data$inputs$actual_dose_levels)) {
            entry_labels <- build_discrete_dose_labels(
              trial_data$inputs$actual_dose_levels,
              summary_dose_units(trial_data$inputs),
              length(trial_data$inputs$actual_dose_levels)
            )
            entry_values <- trial_data$ewoc_entry_dose[is.finite(trial_data$ewoc_entry_dose)]
            if (length(entry_values) > 0) {
              entry_idx <- as.integer(names(sort(table(entry_values), decreasing = TRUE))[1])
              if (entry_idx >= 1L && entry_idx <= length(entry_labels)) entry_labels[entry_idx] else as.character(entry_idx)
            } else {
              NA_character_
            }
          } else if (length(trial_data$ewoc_entry_dose) > 0) {
            names(sort(table(trial_data$ewoc_entry_dose), decreasing = TRUE))[1]
          } else {
            NA_character_
          }
        ),
        stringsAsFactors = FALSE
      )
    ))
  }
  
  if (!is.null(trial_data$combined_table) && nrow(trial_data$combined_table) > 0) {
    dose_label_col <- if ("Actual Dose" %in% names(trial_data$combined_table)) {
      "Actual Dose"
    } else if ("Dose Level" %in% names(trial_data$combined_table)) {
      "Dose Level"
    } else {
      names(trial_data$combined_table)[1]
    }
    combined_table_rows <- do.call(rbind, lapply(seq_len(nrow(trial_data$combined_table)), function(i) {
      row_i <- trial_data$combined_table[i, , drop = FALSE]
      data.frame(
        Section = "Table 1: Trial Efficiency Summary",
        Parameter = paste(dose_label_col, row_i[[dose_label_col]]),
        Value = paste(
          "Probability =", row_i[["Probability"]],
          "; % Allocation =", row_i[["% Allocation"]],
          "; % MTD recommendation =", row_i[["% MTD recommendation"]]
        ),
        stringsAsFactors = FALSE
      )
    }))
    rows <- c(rows, list(combined_table_rows))
  }
  
  if (!is.null(trial_data$dlt_summary) && nrow(trial_data$dlt_summary) > 0) {
    safety_rows <- data.frame(
      Section = "Table 2: Safety Summary",
      Parameter = trial_data$dlt_summary[[1]],
      Value = trial_data$dlt_summary[[2]],
      stringsAsFactors = FALSE
    )
    rows <- c(rows, list(safety_rows))
  }
  
  do.call(rbind, rows)
}
