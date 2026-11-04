test_that("summarize_trial_simulation_results summarizes synthetic discrete results", {
  sim_res <- list(
    list(
      success = TRUE,
      trial_id = 1L,
      dose = c(0, 0, 0.5, 0.5, 0.5, 0.5),
      dlt = c(0, 0, 0, 1, 0, 0),
      postmind0 = c(0, 0.05, 0.10, 0.12, 0.14, 0.18, NA, NA),
      vrho0 = 0.04,
      vmtd = 0.5,
      vmtd_last_assigned = 0.5,
      vmtd_level = 2L,
      vmtd_posterior_median = 0.5,
      vrho1 = 0.45,
      vdltrate = 1 / 6,
      nn_eff = 6L,
      stopval = 0L,
      first_delta_n = NA_real_,
      first_stop_rule_n = NA_real_,
      accel_patients = NA_integer_,
      accel_stop_reason = NA_character_,
      ewoc_entry_dose = NA_integer_
    ),
    list(
      success = TRUE,
      trial_id = 2L,
      dose = c(0, 0, 0.5, 0.5, 1, 1, 1, 1),
      dlt = c(0, 0, 0, 0, 1, 0, 0, 1),
      postmind0 = c(0, 0.04, 0.06, 0.08, 0.12, 0.14, 0.17, 0.20),
      vrho0 = 0.05,
      vmtd = 1,
      vmtd_last_assigned = 1,
      vmtd_level = 3L,
      vmtd_posterior_median = 1,
      vrho1 = 0.50,
      vdltrate = 2 / 8,
      nn_eff = 8L,
      stopval = 1L,
      first_delta_n = NA_real_,
      first_stop_rule_n = 8L,
      accel_patients = NA_integer_,
      accel_stop_reason = NA_character_,
      ewoc_entry_dose = NA_integer_
    )
  )

  trial_inputs <- list(
    M = 2L,
    NN = 8L,
    cohort = 2L,
    dose_type = "Discrete",
    num_doses = 3L,
    startdose = 1L,
    doseprobs = c(0.05, 0.2, 0.35),
    actual_dose_levels = c(10, 20, 30),
    dose_unit = "mg",
    theta = 0.33,
    trial_mode = "Standard EWOC",
    max_accel_dose_level = NA_integer_,
    deltastop = 0.95,
    alpha_increment_cutoff = 0.05,
    use_custom_alpha_increment_cutoff = FALSE,
    lower_f_bound = 0.25,
    increment = 0.05,
    stopping_rule_selected = TRUE,
    num_patients_restrict = 6L,
    burn = 100L,
    mm = 100L
  )

  out <- summarize_trial_simulation_results(sim_res, trial_inputs)

  expect_type(out, "list")
  expect_equal(out$requested_M, 2L)
  expect_equal(out$completed_M, 2L)
  expect_s3_class(out$combined_table, "data.frame")
  expect_s3_class(out$dlt_summary, "data.frame")
  expect_equal(nrow(out$dlt_summary), 2L)
  expect_false(any(grepl("0.05", out$dlt_summary[[1]], fixed = TRUE)))
  expect_null(out$pctdltrategrtheta05)
  expect_s3_class(out$stopping_summary_table, "data.frame")
  expect_equal(nrow(out$combined_table), 3L)
  expect_equal(out$pstop_rule, 0.5)
})
