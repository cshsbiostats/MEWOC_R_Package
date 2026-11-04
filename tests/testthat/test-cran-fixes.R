# CRAN readiness fixes (29 Oct 2026): input checks, the caller's random state, worker count and worker code,
# the batch engine, and summaries of its output.

cran_prior <- data.frame(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)

cran_discrete_inputs <- function(...) {
  modifyList(list(M = 2L, NN = 6L, cohort = 3L, dose_type = "Discrete", num_doses = 4L, startdose = 1L,
                  doseprobs = c(0.05, 0.15, 0.33, 0.50), actual_dose_levels = c(10, 20, 30, 40), dose_unit = "mg",
                  theta = 0.33, deltastop = 0.85, lower_f_bound = 0.25, increment = 0.05, alpha_increment_cutoff = 0.33,
                  burn = 1000L, mm = 200L, seed = 988L, trial_mode = "Standard EWOC", max_accel_dose_level = NA,
                  stopping_rule_selected = FALSE, num_patients_restrict = NA), list(...))
}

test_that("prior calibration rejects impossible targets with clear messages", {
  expect_error(run_mewoc_grid_search(theta = 1.5, p0 = 0.1, p1 = 0.2, condition = 2), "theta")
  expect_error(run_mewoc_grid_search(theta = 0.33, p0 = -0.1, p1 = 0.2, condition = 2), "p0")
  expect_error(run_mewoc_grid_search(theta = 0.33, p0 = 0.7, p1 = 0.6, condition = 2), "p0 \\+ p1")
})

test_that("functions that take a seed restore the caller's random state", {
  skip_if_not_installed("minqa")
  set.seed(5); before <- .Random.seed
  run_mewoc_grid_search(theta = 0.33, p0 = 0.1, p1 = 0.2, condition = 2, seed = 1, n_prior = 6000, bobyqa_starts = 1)
  expect_identical(.Random.seed, before)
  build_prior_plot(cran_prior, num_doses = 3, seed_value = 1)
  expect_identical(.Random.seed, before)
})

test_that("build_prior_plot needs standardized dose positions", {
  expect_error(build_prior_plot(cran_prior, num_doses = 3, seed_value = 1, dose_values = c(10, 20, 30)), "between 0 and 1")
})

test_that("single-trial simulation checks its inputs", {
  args <- list(trial_id = 1, doselevels = seq(0, 1, length.out = 4), doseprobs = c(0.05, 0.15, 0.33, 0.50), startdose = 1,
               cohort = 3, NN = 6, alpha = 0.25, theta = 0.33, burn = 100, mm = 100, chains = 1, opt1 = cran_prior,
               stopping_rule_selected = FALSE, num_patients_restrict = NA, incr = 1 / 3)
  expect_error(do.call(run_single_trial_simulation, modifyList(args, list(doseprobs = c(0.05, 0.1)))), "doseprobs")
  expect_error(do.call(run_single_trial_simulation, modifyList(args, list(startdose = 9))), "startdose")
})

test_that("worker count follows ncores and the 2-core limit of R CMD check", {
  expect_equal(get_trial_worker_count(10, ncores = 1), 1L)
  expect_equal(get_trial_worker_count(1, ncores = 4), 1L)
  old <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("_R_CHECK_LIMIT_CORES_") else Sys.setenv("_R_CHECK_LIMIT_CORES_" = old), add = TRUE)
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
  expect_lte(get_trial_worker_count(100), 2L)
})

test_that("batch engine checks trial_inputs, restores the random state and is reproducible", {
  skip_if_not_installed("rjags")
  expect_error(run_trial_simulations_background(list(M = 2L, dose_type = "Discrete"), cran_prior, ncores = 1), "missing")
  set.seed(5); before <- .Random.seed
  a <- run_trial_simulations_background(cran_discrete_inputs(), cran_prior, ncores = 1)
  expect_identical(.Random.seed, before)
  b <- run_trial_simulations_background(cran_discrete_inputs(), cran_prior, ncores = 1)
  expect_equal(a$status, "completed")
  expect_length(a$results, 2L)
  expect_identical(lapply(a$results, `[[`, "dose"), lapply(b$results, `[[`, "dose"))
  # summarize accepts the engine output and its $results element alike.
  s1 <- summarize_trial_simulation_results(a, cran_discrete_inputs())
  s2 <- summarize_trial_simulation_results(a$results, cran_discrete_inputs())
  expect_identical(s1$combined_table, s2$combined_table)
})

test_that("continuous batch derives the true curve from true_mtd and true_p_min", {
  skip_if_not_installed("rjags")
  inputs <- list(M = 1L, NN = 6L, cohort = 3L, dose_type = "Continuous", min_dose = 10, max_dose = 50, dose_unit = "mg",
                 max_dose_increment = 10, true_mtd = 30, true_p_min = 0.1, theta = 0.33, deltastop = 0.85,
                 lower_f_bound = 0.25, increment = 0.05, alpha_increment_cutoff = 0.33, burn = 1000L, mm = 200L, seed = 988L,
                 trial_mode = "Standard EWOC", max_accel_dose_level = NA, stopping_rule_selected = FALSE,
                 num_patients_restrict = NA, efficiency_stop_tolerance = NA)
  res <- run_trial_simulations_background(inputs, cran_prior, ncores = 1)
  expect_equal(res$status, "completed")
  expect_false(anyNA(res$results[[1]]$dlt))
  expect_error(run_trial_simulations_background(modifyList(inputs, list(true_mtd = NA)), cran_prior, ncores = 1),
               "true dose-toxicity curve")
})

test_that("parallel workers use the installed package, not a file in the working directory", {
  skip_if_not_installed("rjags")
  skip_if(!nzchar(system.file(package = "MEWOC")), "MEWOC is not installed")
  wd <- tempfile("mewoc_wd"); dir.create(file.path(wd, "R"), recursive = TRUE)
  writeLines("stop('a trial_logic.R in the working directory was used')", file.path(wd, "R", "trial_logic.R"))
  old_wd <- setwd(wd); on.exit({ setwd(old_wd); unlink(wd, recursive = TRUE) }, add = TRUE)
  res <- run_trial_simulations_background(cran_discrete_inputs(), cran_prior, ncores = 2)
  expect_equal(res$status, "completed")
})
