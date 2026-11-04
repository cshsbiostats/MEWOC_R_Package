
# Decision tree with the same stopping rules as the simulations (2026-10-15).

tree_test_inputs <- function(...) {
  modifyList(list(
    dose_type = "Discrete", num_doses = 4L, startdose = 1L, NN = 9L, cohort = 1L, theta = 0.33,
    lower_f_bound = 0.25, increment = 0.05, deltastop = 0.85, actual_dose_levels = c(10, 20, 30, 40),
    actual_dose_units = rep("mg", 4), dose_unit = "mg", burn = 200L, mm = 300L, tree_steps = 3L,
    trial_mode = "Standard EWOC", max_accel_dose_level = NA_integer_, alpha_increment_cutoff = 0.33,
    stopping_rule_selected = FALSE, num_patients_restrict = NA_real_,
    efficiency_stop_tolerance = NA_real_
  ), list(...))
}

tree_test_opt1 <- list(a0 = 1.47, b0 = 0.53, a1 = 0.67, b1 = 1.33)

test_that("tree decisions stop only when the Delta probability is greater than Delta", {
  ss <- list(rho0 = c(rep(0.6, 85), rep(0.1, 15)), rho1 = rep(0.9, 100), mtd = rep(0.5, 100))
  # Pr = 0.85 exactly: no stop at Delta = 0.85, stop at Delta = 0.84.
  d_equal <- compute_tree_decision(ss, seq(0, 1, length.out = 4), theta = 0.33, alpha_value = 0.25, current_dose = 0,
                                   assigned_doses = 0, incr = 1 / 3, deltastop = 0.85)
  expect_false(d_equal$stop_trial)
  d <- compute_tree_decision(ss, seq(0, 1, length.out = 4), theta = 0.33, alpha_value = 0.25, current_dose = 0,
                             assigned_doses = 0, incr = 1 / 3, deltastop = 0.84)
  expect_true(d$stop_trial)
  expect_equal(d$stop_type, "safety")
})

test_that("tree shows efficiency stop leaves when the rule is used", {
  ss <- list(rho0 = rep(0.05, 100), rho1 = rep(0.95, 100), mtd = rep(0, 100))
  d <- compute_tree_decision(ss, seq(0, 1, length.out = 4), theta = 0.33, alpha_value = 0.25, current_dose = 0,
                             assigned_doses = c(0, 0), incr = 1 / 3, deltastop = 0.99,
                             efficiency = list(max_patients = 2, width = 0))
  expect_equal(d$next_dose_value, 0)
  expect_true(d$stop_trial)
  expect_equal(d$stop_type, "efficiency")
})

test_that("standard tree builds with candidate labels and alpha advancing after EWOC cohorts", {
  skip_if_not_installed("rjags")
  res <- build_tree_for_cohort_result(tree_test_inputs(), tree_test_opt1, cohort_selected = 1L, seed_value = 1)
  sc <- res$result$primary$sc
  expect_equal(sc$dose_labels, c("10 mg", "20 mg", "30 mg", "40 mg"))
  # Depth 1 decisions follow the starting-dose cohort: the starting alpha, not advanced.
  expect_true(all(sc$nodes$alpha_value_used[sc$nodes$depth == 1] == 0.25))
})

test_that("accelerated tree starts from the EWOC decision after the lead-in", {
  skip_if_not_installed("rjags")
  res <- build_tree_for_cohort_result(
    tree_test_inputs(trial_mode = "Accelerated EWOC", max_accel_dose_level = 2L),
    tree_test_opt1, cohort_selected = 1L, seed_value = 1
  )
  sc <- res$result$primary$sc
  # Lead-in 10 mg -> 20 mg without DLT; EWOC may go at most one level up (to 30 mg).
  expect_lte(sc$nodes$dose_idx[1], 3L)
  expect_match(sc$subtitle_text, "lead-in")
  expect_match(sc$x_labels[1], "after lead-in")
})
