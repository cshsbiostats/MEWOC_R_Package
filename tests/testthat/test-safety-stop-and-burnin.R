test_that("split_burn_in keeps the total number of discarded iterations", {
  expect_equal(split_burn_in(1000), list(n_adapt = 500L, n_burn = 500L))
  expect_equal(split_burn_in(101), list(n_adapt = 51L, n_burn = 50L))
  expect_equal(split_burn_in(0), list(n_adapt = 0L, n_burn = 0L))
})

test_that("a trial stopped because the lowest dose is too toxic recommends no MTD", {
  skip_if_not_installed("rjags")
  opt1 <- list(a0 = 1.47, b0 = 0.53, a1 = 0.67, b1 = 1.33)
  doselevels <- seq(0, 1, length.out = 5)
  set.seed(1)
  res <- run_single_trial_simulation(
    trial_id = 1L, doselevels = doselevels, doseprobs = rep(0.95, 5), startdose = 1L,
    cohort = 1L, NN = 12L, alpha = build_alpha_sequence(0.25, 0.05, 12L), theta = 0.33,
    deltastop = 0.5, burn = 200L, mm = 500L, chains = 1L, opt1 = opt1,
    stopping_rule_selected = FALSE, num_patients_restrict = 12L,
    incr = min(diff(doselevels)), trial_mode = "Standard EWOC"
  )
  expect_true(isTRUE(res$success))
  expect_true(is.finite(res$first_delta_n))
  expect_true(is.na(res$vmtd))
  expect_true(is.na(res$vmtd_level))
})

test_that("discrete summary counts safety-stopped trials as no MTD recommendation", {
  base <- list(
    success = TRUE, postmind0 = rep(0, 4), vrho0 = 0.05, vrho1 = 0.5,
    vmtd_last_assigned = 0, vmtd_posterior_median = 0.5, stopval = 0L,
    first_stop_rule_n = NA_real_,
    accel_patients = NA_integer_, accel_stop_reason = NA_character_, ewoc_entry_dose = NA_integer_
  )
  completed <- modifyList(base, list(
    trial_id = 1L, dose = c(0, 0.5, 0.5, 0.5), dlt = c(0, 0, 1, 0),
    vmtd = 0.5, vmtd_level = 2L, vdltrate = 0.25, nn_eff = 4L, first_delta_n = NA_real_
  ))
  stopped <- modifyList(base, list(
    trial_id = 2L, dose = c(0, 0), dlt = c(1, 1),
    vmtd = NA_real_, vmtd_level = NA_integer_, vdltrate = 1, nn_eff = 2L, first_delta_n = 2
  ))
  trial_inputs <- list(
    M = 2L, NN = 4L, cohort = 1L, dose_type = "Discrete", num_doses = 3L, startdose = 1L,
    doseprobs = c(0.05, 0.2, 0.35), actual_dose_levels = c(10, 20, 30), dose_unit = "mg",
    theta = 0.33, trial_mode = "Standard EWOC", max_accel_dose_level = NA_integer_,
    deltastop = 0.95, alpha_increment_cutoff = 0.33, use_custom_alpha_increment_cutoff = FALSE,
    lower_f_bound = 0.25, increment = 0.05, stopping_rule_selected = FALSE,
    num_patients_restrict = 4L, burn = 100L, mm = 100L
  )
  out <- summarize_trial_simulation_results(list(completed, stopped), trial_inputs)
  pct_mtd <- out$combined_table[["% MTD recommendation"]]
  expect_equal(pct_mtd, c(0, 50, 0))
  expect_equal(out$pstop_delta, 0.5)
})
