
# The next-recommended-dose-too-toxic stop was removed on 2026-10-09; the only
# safety stop is Delta (lowest available dose too toxic).

no_next_stop_trial <- function(doseprobs, deltastop, ...) {
  opt1 <- list(a0 = 1.47, b0 = 0.53, a1 = 0.67, b1 = 1.33)
  doselevels <- seq(0, 1, length.out = 5)
  set.seed(7)
  run_single_trial_simulation(
    trial_id = 1L, doselevels = doselevels, doseprobs = doseprobs, startdose = 1L,
    cohort = 1L, NN = 12L, alpha = build_alpha_sequence(0.25, 0.05, 12L), theta = 0.33,
    deltastop = deltastop, burn = 200L, mm = 500L, chains = 1L, opt1 = opt1,
    stopping_rule_selected = FALSE, num_patients_restrict = 12L,
    incr = min(diff(doselevels)), trial_mode = "Standard EWOC", ...
  )
}

test_that("run_single_trial_simulation has Delta 0.85 and no next-dose threshold", {
  f <- formals(run_single_trial_simulation)
  expect_equal(f$deltastop, 0.85)
  expect_false("deltastop_next" %in% names(f))
  expect_false(exists("next_dose_too_toxic", mode = "function"))
  expect_false(exists("validate_deltastop_next", mode = "function"))
})

test_that("a trial runs to the maximum sample size unless Delta stops it", {
  skip_if_not_installed("rjags")
  p <- c(0.05, 0.12, 0.25, 0.38, 0.55)
  res <- no_next_stop_trial(p, 0.99)
  expect_true(isTRUE(res$success))
  expect_equal(res$nn_eff, 12L)
  expect_false(any(c("first_next_overdose_n", "next_overdose_stop", "next_overdose_probability") %in% names(res)))
})

test_that("continuous trials have no next-dose fields", {
  skip_if_not_installed("rjags")
  opt1 <- list(a0 = 1.47, b0 = 0.53, a1 = 0.67, b1 = 1.33)
  set.seed(7)
  res <- run_single_trial_simulation_standard_continuous(
    trial_id = 1L, min_dose = 100, max_dose = 500, true_beta0 = -1, true_beta1 = 3, true_mtd = NA,
    true_p_min = NA, true_p_max = NA, max_dose_increment = 50, cohort = 1L, NN = 12L,
    alpha = build_alpha_sequence(0.25, 0.05, 12L), theta = 0.33, deltastop = 0.99,
    burn = 200L, mm = 500L, chains = 1L, opt1 = opt1, stopping_rule_selected = FALSE, num_patients_restrict = 12L
  )
  expect_true(isTRUE(res$success))
  expect_equal(res$nn_eff, 12L)
  expect_false(any(c("first_next_overdose_n", "next_overdose_stop", "next_overdose_probability") %in% names(res)))
})

test_that("the conduct design table has no next-dose threshold row", {
  dt <- build_conduct_design_table(
    p0 = 0.1, p1 = 0.2, a0 = 1, b0 = 1, a1 = 1, b1 = 1, num_doses = 5, startdose = 1,
    theta = 0.33, deltastop = 0.85, lower_f_bound = 0.25, increment = 0.05,
    trial_mode = "No", max_accel_dose_level = NA, seed = 1, burn = 100, mm = 100
  )
  expect_false(any(grepl("Next-Dose", dt[[1]])))
  expect_true("Stopping Threshold Delta" %in% dt[[1]])
})
