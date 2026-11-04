
# Stopping rules agreed on 2026-10-15: efficiency stop at the recommended dose, Delta stop at > Delta (strict since 29 Oct 2026)
# (also on the final data), and the accelerated
# singleton lead-in.

fake_posterior <- function(rho0, rho1) {
  data.frame(rho0 = rho0, rho1 = rho1, mtd = 0.5)
}

test_that("discrete efficiency stop needs N patients already treated at the recommended dose", {
  X <- c(0, 0, 0, 0.25, 0.25, 0.25)
  expect_false(efficiency_stop_check(X, next_dose = 0.25, max_patients = 4))
  expect_true(efficiency_stop_check(X, next_dose = 0.25, max_patients = 3))
  # N patients at another dose do not stop the trial when EWOC recommends a different dose.
  expect_false(efficiency_stop_check(X, next_dose = 0.5, max_patients = 3))
})

test_that("continuous efficiency window is centred on the recommended dose", {
  # 0.2099 and 0.2101 straddle a fixed 1% bin edge but are both within +/- 0.005 of 0.21.
  X <- c(0.2099, 0.2101, 0.2140)
  expect_true(efficiency_stop_check(X, next_dose = 0.21, max_patients = 3, width = 0.01))
  expect_false(efficiency_stop_check(X, next_dose = 0.21, max_patients = 4, width = 0.01))
  # A dose 0.0098 away is outside a 1% window.
  expect_false(efficiency_stop_check(c(0.2001, 0.2099), next_dose = 0.2001, max_patients = 2, width = 0.01))
})

test_that("efficiency stop is off unless selected with a usable patient limit", {
  expect_false(efficiency_stop_enabled(FALSE, 3))
  expect_false(efficiency_stop_enabled(TRUE, NA))
  expect_true(efficiency_stop_enabled(TRUE, 3))
})

test_that("the Delta stop fires only when the posterior probability is greater than Delta", {
  # P(DLT at dose 0) > theta in exactly 85 of 100 draws.
  ss <- fake_posterior(rho0 = c(rep(0.6, 85), rep(0.1, 15)), rho1 = 0.9)
  expect_false(safety_stop_check(ss, theta = 0.33, deltastop = 0.85)$stop)   # equal to Delta: no stop
  expect_true(safety_stop_check(ss, theta = 0.33, deltastop = 0.84)$stop)
  expect_false(safety_stop_check(ss, theta = 0.33, deltastop = 0.86)$stop)
})

test_that("accelerated lead-in escalates one level per patient and stops at the first DLT", {
  set.seed(1)
  leadin <- run_accelerated_leadin(seq(0, 1, length.out = 5), c(0, 0, 1, 1, 1), startdose = 1,
                                   max_accel_dose_level = 4, NN = 20)
  expect_equal(leadin$X, c(0, 0.25, 0.5))
  expect_equal(leadin$Y, c(0L, 0L, 1L))
  expect_equal(leadin$accel_stop_reason, "DLT observed during accelerated phase")
})

test_that("accelerated lead-in stops after the patient at the maximum accelerated dose", {
  leadin <- run_accelerated_leadin(seq(0, 1, length.out = 5), rep(0, 5), startdose = 2,
                                   max_accel_dose_level = 3, NN = 20)
  expect_equal(leadin$X, c(0.25, 0.5))
  expect_equal(leadin$accel_patients, 2L)
  expect_equal(leadin$accel_stop_reason, "Maximum accelerated dose reached")
})

test_that("accelerated trials enter EWOC without skipping an untried dose", {
  skip_if_not_installed("rjags")
  opt1 <- list(a0 = 1.47, b0 = 0.53, a1 = 0.67, b1 = 1.33)
  doselevels <- seq(0, 1, length.out = 6)
  set.seed(3)
  res <- run_single_trial_simulation(
    trial_id = 1L, doselevels = doselevels, doseprobs = rep(0, 6), startdose = 1L,
    cohort = 3L, NN = 9L, alpha = build_alpha_sequence(0.25, 0.05, 3L), theta = 0.33,
    deltastop = 0.99, burn = 200L, mm = 500L, chains = 1L, opt1 = opt1,
    stopping_rule_selected = FALSE, num_patients_restrict = NA, incr = 0.2,
    trial_mode = "Accelerated EWOC", max_accel_dose_level = 3L
  )
  expect_true(isTRUE(res$success))
  # Lead-in: doses 1, 2, 3 (one patient each); EWOC may go at most one level up, to dose 4.
  expect_equal(res$dose[1:3], doselevels[1:3])
  expect_lte(res$ewoc_entry_dose, 4L)
  expect_equal(res$accel_patients, 3L)
  # Lead-in patients count toward N; the last cohort is partial.
  expect_equal(res$nn_eff, 9L)
})

test_that("an efficiency stop ends the trial before the next cohort is treated and keeps the MTD", {
  skip_if_not_installed("rjags")
  opt1 <- list(a0 = 1.47, b0 = 0.53, a1 = 0.67, b1 = 1.33)
  doselevels <- seq(0, 1, length.out = 2)
  set.seed(5)
  res <- run_single_trial_simulation(
    trial_id = 1L, doselevels = doselevels, doseprobs = c(0.3, 0.9), startdose = 1L,
    cohort = 3L, NN = 30L, alpha = build_alpha_sequence(0.25, 0.05, 10L), theta = 0.33,
    deltastop = 0.99, burn = 200L, mm = 500L, chains = 1L, opt1 = opt1,
    stopping_rule_selected = TRUE, num_patients_restrict = 6L, incr = 1,
    trial_mode = "Standard EWOC"
  )
  expect_true(isTRUE(res$success))
  if (identical(res$stopval, 1L)) {
    expect_equal(res$nn_eff, res$first_stop_rule_n)
    expect_true(is.finite(res$vmtd))
    # N patients were already at the recommended dose when the trial stopped.
    expect_gte(max(table(res$dose)), 6L)
  }
})

test_that("the Delta stop is checked on the final data when the trial reaches N", {
  skip_if_not_installed("rjags")
  opt1 <- list(a0 = 1.47, b0 = 0.53, a1 = 0.67, b1 = 1.33)
  doselevels <- seq(0, 1, length.out = 5)
  set.seed(11)
  # One cohort only (N = cohort): no fit happens inside the loop, so any Delta stop comes from the final check.
  res <- run_single_trial_simulation(
    trial_id = 1L, doselevels = doselevels, doseprobs = rep(0.95, 5), startdose = 1L,
    cohort = 6L, NN = 6L, alpha = build_alpha_sequence(0.25, 0.05, 1L), theta = 0.33,
    deltastop = 0.5, burn = 200L, mm = 500L, chains = 1L, opt1 = opt1,
    stopping_rule_selected = FALSE, num_patients_restrict = NA, incr = 0.25,
    trial_mode = "Standard EWOC"
  )
  expect_true(isTRUE(res$success))
  expect_equal(res$nn_eff, 6L)
  expect_equal(res$first_delta_n, 6)
  expect_true(is.na(res$vmtd))
})
