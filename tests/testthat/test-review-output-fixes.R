review_continuous_inputs <- function() {
  list(M = 2L, NN = 9L, cohort = 3L, dose_type = "Continuous",
       min_dose = 10, max_dose = 50, dose_unit = "mg", max_dose_increment = 10,
       true_mtd = 30, true_p_min = 0.1, theta = 0.33, deltastop = 1,
       lower_f_bound = 0.25, increment = 0.05, alpha_increment_cutoff = 0.33,
       burn = 400L, mm = 500L, seed = 988L, trial_mode = "Standard EWOC",
       max_accel_dose_level = NA, stopping_rule_selected = FALSE,
       num_patients_restrict = NA, efficiency_stop_tolerance = NA)
}

review_continuous_result <- function(safety_stop = FALSE) {
  list(success = TRUE, trial_id = 1L, nn_eff = 3L, actual_dose = rep(50, 3),
       dlt = c(0, 0, 1), postmind0 = rep(0, 3), vrho0 = 0.1,
       vmtd = if (safety_stop) NA_real_ else 30, vrho1 = 0.6, vdltrate = 1 / 3,
       stopval = 0L, first_delta_n = if (safety_stop) 3 else NA_real_,
       accel_patients = NA_integer_, accel_stop_reason = NA_character_,
       ewoc_entry_dose = NA_real_)
}

test_that("continuous summary derives truth and counts truly toxic allocations", {
  inputs <- review_continuous_inputs()
  inputs$M <- 1L
  inputs$NN <- 3L
  out <- summarize_trial_simulation_results(list(review_continuous_result()), inputs)
  expect_equal(out$combined_table[["% Selection within 10% of theta"]], 100)
  expect_equal(out$combined_table[["Number allocated to toxic doses (mean, 90% CI)"]], "3 (3, 3)")

  # The beta parameterization must produce the same complete table, including bias.
  beta_inputs <- inputs
  beta_inputs$true_beta0 <- qlogis(0.1)
  beta_inputs$true_beta1 <- (qlogis(0.33) - qlogis(0.1)) / 0.5
  beta_inputs$true_mtd <- beta_inputs$true_p_min <- NULL
  by_beta <- summarize_trial_simulation_results(list(review_continuous_result()), beta_inputs)
  expect_equal(out$combined_table, by_beta$combined_table)
})

test_that("all safety-stopped continuous trials count as no MTD selection", {
  inputs <- review_continuous_inputs()
  inputs$M <- 1L
  inputs$NN <- 3L
  out <- summarize_trial_simulation_results(list(review_continuous_result(TRUE)), inputs)
  expect_equal(out$combined_table[["% Selection within 10% of theta"]], 0)
  expect_true(is.na(out$combined_table[["Average Estimated MTD"]]))
  expect_equal(out$pstop_delta, 1)
})

test_that("documented continuous batch workflow preserves its scenario", {
  inputs <- review_continuous_inputs()
  prior <- data.frame(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)
  sims <- run_trial_simulations_background(inputs, prior, ncores = 1)
  expect_identical(sims$status, "completed")
  expect_equal(sims$trial_inputs$true_beta0, qlogis(0.1))
  expect_equal(sims$trial_inputs$true_beta1, (qlogis(0.33) - qlogis(0.1)) / 0.5)
  original <- summarize_trial_simulation_results(sims, inputs)
  raw <- summarize_trial_simulation_results(sims$results, inputs)
  expect_equal(original$combined_table, raw$combined_table)
  expect_false(anyNA(original$combined_table[["% Selection within 10% of theta"]]))
  expect_equal(summarize_trial_simulation_results(sims, list())$combined_table, original$combined_table)
  expect_error(summarize_trial_simulation_results(sims, modifyList(inputs, list(true_mtd = 35))), "true_mtd.*conflicts")
  expect_error(summarize_trial_simulation_results(sims, modifyList(inputs, list(dose_type = "Discrete"))), "dose_type.*conflicts")

  # Legacy batches without metadata remain supported.
  legacy <- sims
  legacy$trial_inputs <- NULL
  expect_equal(summarize_trial_simulation_results(legacy, inputs)$combined_table, original$combined_table)

  flag <- tempfile()
  file.create(flag)
  on.exit(unlink(flag), add = TRUE)
  stopped <- run_trial_simulations_background(inputs, prior, stop_flag_path = flag, ncores = 1)
  expect_identical(stopped$status, "stopped")
  expect_equal(stopped$trial_inputs, sims$trial_inputs)

  # Preserve scenario metadata even if posterior fitting fails after validation.
  local_mocked_bindings(run_single_trial_simulation_standard_continuous = function(trial_id, ...) {
    list(success = FALSE, trial_id = trial_id, error = "simulated posterior failure")
  })
  failed <- run_trial_simulations_background(inputs, prior, ncores = 1)
  expect_identical(failed$status, "failed")
  expect_equal(failed$trial_inputs$true_beta1, sims$trial_inputs$true_beta1)
})

test_that("invalid continuous truth cannot silently produce zero toxic allocation", {
  inputs <- review_continuous_inputs()
  inputs$true_mtd <- inputs$true_p_min <- NULL
  expect_error(summarize_trial_simulation_results(list(review_continuous_result()), inputs), "true dose-toxicity curve")
})

test_that("workbooks preserve documented trial modes and compatibility aliases", {
  build <- function(mode) build_conduct_design_table(
    p0 = 0.1, p1 = 0.2, a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66,
    num_doses = 4, startdose = 1, theta = 0.33, deltastop = 0.85,
    lower_f_bound = 0.25, increment = 0.05, trial_mode = mode,
    max_accel_dose_level = 3, seed = 988, burn = 1000, mm = 1500)
  accelerated <- build("Accelerated EWOC")
  value <- function(tab, key) tab[[2]][tab[[1]] == key]
  expect_equal(value(accelerated, "Accelerated EWOC"), "Yes")
  expect_equal(value(accelerated, "Maximum Accelerated Dose"), "3")
  expect_equal(build("Yes"), accelerated)
  standard <- build("Standard EWOC")
  expect_equal(build("No"), standard)
  expect_equal(value(standard, "Accelerated EWOC"), "No")
  expect_equal(value(standard, "Maximum Accelerated Dose"), "")
  for (mode in list("Typo EWOC", NA_character_, NULL, c("Yes", "No"))) {
    expect_error(build(mode), "trial_mode")
  }

  file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file), add = TRUE)
  write_conduct_workbook(file, accelerated, build_conduct_patient_template())
  loaded <- openxlsx::read.xlsx(file, sheet = "Trial Design", check.names = FALSE)
  expect_equal(value(loaded, "Accelerated EWOC"), "Yes")
  expect_equal(value(loaded, "Maximum Accelerated Dose"), "3")
})
