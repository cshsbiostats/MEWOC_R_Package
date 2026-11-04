
# Trial Conduct evaluated cohort by cohort (rules agreed on 2026-10-15).

conduct_test_opt1 <- list(a0 = 1.47, b0 = 0.53, a1 = 0.67, b1 = 1.33)

conduct_test_design <- function(dose_type = "Discrete", num_doses = 5L, startdose = 1L) {
  if (identical(dose_type, "Continuous")) {
    design <- list(dose_type = "Continuous", dose_unit = "mg", min_dose = 100, max_dose = 500,
                   max_dose_increment = 25, num_doses = NA_integer_, startdose = NA_integer_,
                   actual_dose_levels = numeric(0), dose_units = character(0), doselevels = numeric(0))
  } else {
    actual <- seq(10, by = 10, length.out = num_doses)
    design <- list(dose_type = "Discrete", dose_unit = "mg", min_dose = NA_real_, max_dose = NA_real_,
                   max_dose_increment = NA_real_, num_doses = num_doses, startdose = startdose,
                   actual_dose_levels = actual, dose_units = rep("mg", num_doses),
                   doselevels = seq(0, 1, length.out = num_doses))
  }
  design
}

conduct_test_settings <- function(...) {
  modifyList(list(theta = 0.33, deltastop = 0.85, lower_alpha = 0.25, increment = 0.05,
                  alpha_increment_cutoff = 0.33, burn = 200L, mm = 500L, seed = 7L,
                  trial_mode = "Standard EWOC", max_accel_dose_level = NA, cohort_size = 3L,
                  total_sample_size = 30L, efficiency_rule = FALSE, num_patients_restrict = NA,
                  efficiency_width = 0.01), list(...))
}

conduct_test_rows <- function(cohort, dose, dlt, unevaluable = rep(FALSE, length(dose)),
                              rec = rep(NA_real_, length(dose)), alpha = rep(NA_real_, length(dose))) {
  data.frame(Cohort = as.character(cohort), Patient = as.character(seq_along(dose)),
             Administered_Dose = dose, DLT = as.integer(dlt), Is_Unevaluable = unevaluable,
             Uploaded_Recommended_Dose = rec, Uploaded_Alpha_Used = alpha, stringsAsFactors = FALSE)
}

test_that("blank cohorts are filled using the cohort size and lead-in cohorts of one", {
  out <- conduct_fill_cohorts(c("1", "", "", "", ""), cohort_size = 3)
  expect_equal(out$cohort, c("1", "1", "1", "2", "2"))
  expect_equal(out$filled, c(FALSE, TRUE, TRUE, TRUE, TRUE))
  leadin <- conduct_fill_cohorts(c("", "", "", ""), cohort_size = 3, leadin_flags = c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(leadin$cohort, c("1", "2", "3", "3"))
})

test_that("Trial Conduct seeds locally: reproducible fits, session random state unchanged", {
  set.seed(99)
  before <- .Random.seed
  a <- with_local_seed(7, runif(3))
  expect_identical(.Random.seed, before)
  b <- with_local_seed(7, runif(3))
  expect_identical(a, b)
  expect_identical(with_local_seed(NA, 5), 5)
})

test_that("numeric cohorts are evaluated in numeric order even when rows are not sorted", {
  skip_if_not_installed("rjags")
  rows <- conduct_test_rows(c(2, 2, 2, 1, 1, 1), c(20, 20, 20, 10, 10, 10), rep(0, 6))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  # Cohort 1 (rows 4-6) is the starting dose; cohort 2 (rows 1-3) is the first EWOC cohort.
  expect_equal(res$rows$Check_Recommended_Dose[4:6], rep(10, 3))
  expect_equal(res$rows$Check_Alpha[1:3], rep(0.25, 3))
})

test_that("trial rule rows include the efficiency settings only when the rule is used", {
  off <- conduct_trial_rule_rows(3, 30, "No")
  expect_equal(off[[1]], c("Cohort Size", "Maximum Sample Size", "Efficiency Stopping Rule"))
  on <- conduct_trial_rule_rows(3, 30, "Yes", 9, 1, dose_type = "Continuous")
  expect_true(all(c("Maximum Patients at Recommended Dose", "Dose-Region Width (%)") %in% on[[1]]))
})

test_that("the first cohort is the starting dose and blank recommendations and alphas are filled", {
  skip_if_not_installed("rjags")
  rows <- conduct_test_rows(c(1, 1, 1, 2, 2, 2), c(10, 10, 10, 20, 20, 20), c(0, 0, 0, 0, 0, 0))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res$status, "ok")
  expect_equal(res$rows$Check_Recommended_Dose[1:3], rep(10, 3))
  expect_true(all(res$rows$Recommended_Filled))
  # Cohort 2 is the first EWOC cohort: the starting alpha.
  expect_equal(res$rows$Check_Alpha[4:6], rep(0.25, 3))
  # No DLTs so far: alpha increases for the next cohort.
  expect_equal(res$alpha_for_next, 0.30)
  expect_true(is.finite(res$next_recommendation$dose))
})

test_that("a wrong alpha and a dose given that differs from the recommendation are flagged", {
  skip_if_not_installed("rjags")
  rows <- conduct_test_rows(c(1, 1, 1, 2, 2, 2), c(10, 10, 10, 30, 30, 30), rep(0, 6),
                            rec = c(10, 10, 10, 20, 20, 20), alpha = c(NA, NA, NA, 0.4, 0.4, 0.4))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_true(all(grepl("differs from the correct \u03b1", res$rows$Warning[4:6])))
  expect_true(all(grepl("differs from the recorded recommended dose", res$rows$Warning[4:6])))
  expect_false(any(nzchar(res$rows$Warning[1:3])))
})

test_that("the model, N and the efficiency rule count evaluable patients; pending patients give a warning", {
  skip_if_not_installed("rjags")
  # At 10 mg: 3 with a DLT result, 1 pending, 1 unevaluable.
  rows <- conduct_test_rows(c(1, 1, 1, 2, 2), c(10, 10, 10, 10, 10), c(0, NA, 0, 0, NA),
                            unevaluable = c(FALSE, TRUE, FALSE, FALSE, FALSE))
  design <- conduct_test_design(num_doses = 2L)
  res4 <- conduct_evaluate_trial(rows, design, conduct_test_settings(efficiency_rule = TRUE, num_patients_restrict = 4L),
                                 conduct_test_opt1)
  expect_equal(res4$n_patients, 3L)
  expect_equal(res4$n_treated, 3L)
  expect_equal(res4$n_pending, 1L)
  expect_equal(res4$n_unevaluable, 1L)
  expect_equal(res4$rows$Used_In_Model, c(TRUE, FALSE, TRUE, TRUE, FALSE))
  # 3 evaluable < 4: no stop. With the pending patient it would be 4: warning only.
  expect_null(res4$trial_stop)
  if (identical(res4$next_recommendation$dose, 10)) {
    expect_match(res4$pending_efficiency_warning, "Pending DLT status")
  }
  res3 <- conduct_evaluate_trial(rows, design, conduct_test_settings(efficiency_rule = TRUE, num_patients_restrict = 3L),
                                 conduct_test_opt1)
  # 3 evaluable patients reach a limit of 3: the trial stops if EWOC recommends 10 mg again.
  if (identical(res3$next_recommendation$dose, 10)) {
    expect_equal(res3$trial_stop$type, "efficiency")
    expect_equal(res3$next_recommendation$action, "Stop Trial")
  }
})

test_that("a Delta stop ends the trial and later cohorts are not used", {
  skip_if_not_installed("rjags")
  rows <- conduct_test_rows(rep(1:3, each = 3), rep(10, 9), c(1, 1, 1, 1, 1, 1, 0, 0, 0))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(deltastop = 0.5), conduct_test_opt1)
  expect_equal(res$trial_stop$type, "lowest_dose_too_toxic")
  expect_equal(res$next_recommendation$action, "Stop Trial")
  stopped_cohort <- res$trial_stop$cohort
  later <- as.numeric(res$rows$Cohort) >= as.numeric(stopped_cohort)
  expect_false(any(res$rows$Used_In_Model[later]))
  expect_true(all(grepl("stopped", res$rows$Warning[later])))
})

test_that("accelerated lead-in escalates one patient at a time and flags a skipped level", {
  skip_if_not_installed("rjags")
  rows <- conduct_test_rows(1:3, c(10, 20, 40), c(0, 0, 0))
  res <- conduct_evaluate_trial(rows, conduct_test_design(),
                                conduct_test_settings(trial_mode = "Accelerated EWOC", max_accel_dose_level = 4L),
                                conduct_test_opt1)
  expect_equal(res$rows$Check_Recommended_Dose, c(10, 20, 30))
  expect_true(grepl("skipped during the accelerated lead-in", res$rows$Warning[3]))
  expect_true(all(is.na(res$rows$Check_Alpha)))
})

test_that("after the lead-in reaches the maximum accelerated dose, EWOC starts at the starting alpha", {
  skip_if_not_installed("rjags")
  rows <- conduct_test_rows(1:3, c(10, 20, 30), c(0, 0, 0))
  res <- conduct_evaluate_trial(rows, conduct_test_design(),
                                conduct_test_settings(trial_mode = "Accelerated EWOC", max_accel_dose_level = 3L),
                                conduct_test_opt1)
  expect_false(res$leadin_active)
  expect_equal(res$alpha_for_next, 0.25)
  expect_equal(res$next_recommendation$action, "Recommend Dose")
  # No untried dose may be skipped: at most one level above 30 mg.
  expect_lte(res$next_recommendation$dose, 40)
})

test_that("entering more than N patients gives a warning and no next recommendation", {
  skip_if_not_installed("rjags")
  rows <- conduct_test_rows(c(1, 1, 1, 2, 2, 2, 2), c(10, 10, 10, 20, 20, 20, 20), rep(0, 7))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(total_sample_size = 6L), conduct_test_opt1)
  expect_null(res$next_recommendation)
  expect_true(any(grepl("More evaluable patients", res$messages)))
})

test_that("pending patients do not count toward N", {
  skip_if_not_installed("rjags")
  # 6 patients entered, 1 pending: 5 evaluable toward N = 6, so the trial continues.
  rows <- conduct_test_rows(c(1, 1, 1, 2, 2, 2), c(10, 10, 10, 20, 20, 20), c(0, 0, 0, 0, 0, NA))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(total_sample_size = 6L), conduct_test_opt1)
  expect_equal(res$n_treated, 5L)
  expect_equal(res$n_pending, 1L)
  expect_false(is.null(res$next_recommendation))
})

test_that("unevaluable patients do not count toward N", {
  skip_if_not_installed("rjags")
  # 6 patients entered, 1 unevaluable: 5 count toward N = 6, so the trial continues.
  rows <- conduct_test_rows(c(1, 1, 1, 2, 2, 2), c(10, 10, 10, 20, 20, 20), c(0, NA, 0, 0, 0, 0),
                            unevaluable = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(total_sample_size = 6L), conduct_test_opt1)
  expect_equal(res$n_treated, 5L)
  expect_equal(res$n_unevaluable, 1L)
  expect_false(is.null(res$next_recommendation))
  expect_false(any(grepl("maximum sample size", res$messages)))
})

test_that("continuous doses above the maximum increment are flagged", {
  skip_if_not_installed("rjags")
  rows <- conduct_test_rows(c(1, 1, 1, 2, 2, 2), c(100, 100, 100, 300, 300, 300), rep(0, 6))
  res <- conduct_evaluate_trial(rows, conduct_test_design("Continuous"), conduct_test_settings(), conduct_test_opt1)
  # 100 -> 300 mg is +50% of the 100-500 mg range; the maximum increment is 25%.
  expect_true(all(grepl("maximum dose increment", res$rows$Warning[4:6])))
})

test_that("an invalid entered dose blocks the entire conduct evaluation before fitting", {
  local_mocked_bindings(fit_trial_posterior = function(...) stop("must not fit"))
  rows <- conduct_test_rows(c(1, 1, 1, 2, 2, 2), c(10, 10, 10, 15, 20, 20), rep(0, 6))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res$status, "error")
  expect_match(res$message, "Row 4, cohort 2, patient 4")
  expect_match(res$message, "not one of the doses listed in the protocol")
  expect_match(res$message, "Correct the invalid dose")
  expect_null(res$next_recommendation)
  expect_true(all(is.na(res$rows$Recommended_Dose)))
  expect_equal(res$rows$Administered_Dose, rows$Administered_Dose)
  cont <- conduct_test_rows(c(1, 1, 1), c(100, 100, 600), rep(0, 3))
  res_c <- conduct_evaluate_trial(cont, conduct_test_design("Continuous"), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res_c$status, "error")
  expect_match(res_c$message, "outside the protocol dose range [(]100 to 500[)]")
})

test_that("reordered cohorts preserve recommendations and maximum-increment warnings", {
  local_mocked_bindings(fit_trial_posterior = function(...) {
    list(rho0 = rep(0.01, 200), rho1 = rep(0.1, 200), mtd = rep(2, 200))
  })
  for (type in c("Continuous", "Discrete")) {
    doses <- if (type == "Continuous") c(100, 200, 300) else c(10, 20, 30)
    rows <- conduct_test_rows(1:3, doses, rep(0, 3))
    design <- conduct_test_design(type)
    a <- conduct_evaluate_trial(rows, design, conduct_test_settings(cohort_size = 1), conduct_test_opt1)
    b <- conduct_evaluate_trial(rows[c(2, 1, 3), ], design, conduct_test_settings(cohort_size = 1), conduct_test_opt1)
    fields <- c("Patient", "Check_Recommended_Dose", "Check_Alpha", "Warning")
    expect_equal(a$rows[, fields], b$rows[match(a$rows$Patient, b$rows$Patient), fields], ignore_attr = TRUE)
    expect_equal(a$next_recommendation$dose, b$next_recommendation$dose)
    if (type == "Continuous") expect_equal(b$rows$Check_Recommended_Dose[3], 300)
  }
})

test_that("the next numeric cohort label is above the maximum existing label", {
  local_mocked_bindings(fit_trial_posterior = function(...) {
    list(rho0 = rep(0.01, 200), rho1 = rep(0.1, 200), mtd = rep(2, 200))
  })
  for (labels in list(c(1, 3), c(3, 1), c(10, 20))) {
    rows <- conduct_test_rows(labels, c(10, 20), c(0, 0))
    res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
    expect_equal(res$next_recommendation$cohort, paste("Cohort", max(labels) + 1))
  }
})

test_that("duplicate patient IDs are flagged", {
  rows <- transform(conduct_test_rows(c(1, 1, 1), c(10, 10, 10), rep(0, 3)), Patient = c("1", "1", "2"))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_true(all(grepl("Duplicate Patient ID [(]1[)]", res$rows$Warning[1:2])))
  expect_false(grepl("Duplicate", res$rows$Warning[3]))
})

test_that("a row marked both DLT and Unevaluable, or with an unknown status, is omitted", {
  rows <- conduct_test_rows(c(1, 1, 1), c(10, 10, 10), c(1, 0, NA), unevaluable = c(TRUE, FALSE, FALSE))
  rows$DLT_Status_Text <- c("DLT", "No DLT", "maybe")
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_match(res$rows$Warning[1], "marked both DLT and Unevaluable.*omitted from the calculation")
  expect_match(res$rows$Warning[3], "DLT Status \"maybe\" is not recognized.*omitted from the calculation")
  expect_equal(res$n_treated, 1L)
  expect_equal(res$n_unevaluable, 0L)
  expect_equal(res$n_pending, 0L)
})

test_that("an alpha entered for the starting dose is not used and is set to NA", {
  rows <- conduct_test_rows(c(1, 1, 1), c(10, 10, 10), rep(0, 3), alpha = c(0.4, NA, NA))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_true(all(is.na(res$rows$Alpha_Used)))
  expect_match(res$rows$Warning[1], "not used for the starting dose")
})

conduct_fixed_posterior <- function(...) list(rho0 = rep(0.01, 200), rho1 = rep(0.1, 200), mtd = rep(2, 200))

test_that("a dose that is text, or a blank dose with a DLT result, blocks the evaluation", {
  local_mocked_bindings(fit_trial_posterior = function(...) stop("must not fit"))
  rows <- conduct_test_rows(c(1, 1, 1), c(10, NA, 10), c(0, 0, 0))
  rows$Administered_Dose_Text <- c("10", "20mg", "10")
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res$status, "error")
  expect_match(res$message, "Row 2, cohort 1, patient 2: dose \"20mg\" is not a number", fixed = TRUE)
  expect_match(res$message, "Correct the invalid dose", fixed = TRUE)
  missing <- conduct_test_rows(c(1, 1, 1), c(10, NA, 10), c(0, 1, 0))
  res_missing <- conduct_evaluate_trial(missing, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res_missing$status, "error")
  expect_match(res_missing$message, "Row 2, cohort 1, patient 2: a DLT result is entered but the dose given is blank", fixed = TRUE)
})

test_that("a fully blank row is still accepted", {
  local_mocked_bindings(fit_trial_posterior = conduct_fixed_posterior)
  rows <- conduct_test_rows(c(1, 1, 1, NA), c(10, 10, 10, NA), c(0, 0, 0, NA))
  rows$Cohort[4] <- ""
  rows$Patient[4] <- ""
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res$status, "ok")
  expect_equal(res$rows$Decision[4], "Empty row")
})

test_that("DLT values other than 1, 0 or blank block the evaluation; valid codings are accepted", {
  local_mocked_bindings(fit_trial_posterior = conduct_fixed_posterior)
  base <- conduct_test_rows(c(1, 1, 1), c(10, 10, 10), c(0, 0, 0))
  for (bad in list(c(0, 0.9, 0), c(0, 2, 0), c(0, -1, 0))) {
    rows <- base
    rows$DLT <- bad
    res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
    expect_equal(res$status, "error")
    expect_match(res$message, paste0("Row 2, cohort 1, patient 2: DLT value \"", bad[2], "\" is not valid"), fixed = TRUE)
  }
  rows <- base
  rows$DLT <- factor(c("0", "1", "0"))
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res$status, "ok")
  expect_equal(res$rows$DLT[2], 1L)
  rows$DLT <- c(FALSE, TRUE, NA)
  res <- conduct_evaluate_trial(rows, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res$status, "ok")
  expect_equal(res$n_pending, 1L)
})

test_that("a cohort with fewer evaluable patients than the cohort size is reported as incomplete", {
  local_mocked_bindings(fit_trial_posterior = conduct_fixed_posterior)
  cohorts <- rep(1:3, each = 3)
  doses <- rep(c(10, 20, 20), each = 3)
  # Cohort 2: 1 evaluable, 2 pending.
  pending <- conduct_test_rows(cohorts, doses, c(0, 0, 0, 0, NA, NA, 0, 0, 0))
  res <- conduct_evaluate_trial(pending, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_equal(res$incomplete_cohorts$Cohort, "2")
  expect_equal(res$incomplete_cohorts$Pending, 2L)
  expect_match(res$incomplete_cohorts$Message, "cohort 2 has 1 of 3 evaluable patients (2 pending)", fixed = TRUE)
  expect_match(res$incomplete_cohorts$Message, "may change after the pending results are entered", fixed = TRUE)
  expect_false(is.null(res$next_recommendation))
  # Cohort 2: 2 evaluable, 1 unevaluable (not replaced).
  unev <- conduct_test_rows(cohorts, doses, c(0, 0, 0, 0, 0, NA, 0, 0, 0), unevaluable = c(rep(FALSE, 5), TRUE, rep(FALSE, 3)))
  res_unev <- conduct_evaluate_trial(unev, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_match(res_unev$incomplete_cohorts$Message, "(1 unevaluable)", fixed = TRUE)
  expect_match(res_unev$incomplete_cohorts$Message, "needs 1 more evaluable patient to reach the minimum cohort size of 3", fixed = TRUE)
  # Complete cohorts, and a last cohort still being enrolled, are not reported.
  complete <- conduct_test_rows(c(cohorts, 4, 4), c(doses, 30, 30), rep(0, 11))
  res_complete <- conduct_evaluate_trial(complete, conduct_test_design(), conduct_test_settings(), conduct_test_opt1)
  expect_null(res_complete$incomplete_cohorts)
  expect_true(any(grepl("Cohort 4 has 2 of at least 3 patients", res_complete$messages, fixed = TRUE)))
})
