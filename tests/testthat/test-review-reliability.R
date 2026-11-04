reliability_inputs <- function() {
  list(M = 2L, NN = 6L, cohort = 3L, dose_type = "Discrete", num_doses = 4L,
       startdose = 1L, doseprobs = c(0.05, 0.15, 0.33, 0.5), theta = 0.33,
       deltastop = 0.85, lower_f_bound = 0.25, increment = 0.05,
       burn = 200L, mm = 300L, seed = 988L, trial_mode = "Standard EWOC",
       stopping_rule_selected = FALSE, num_patients_restrict = NA)
}

test_that("invalid public simulation inputs fail before any posterior fitting", {
  prior <- list(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)
  args <- list(trial_id = 1, doselevels = c(0, 0.5, 1), doseprobs = c(0.05, 0.2, 0.5),
               startdose = 1, cohort = 3, NN = 6, alpha = 0.25, theta = 0.33,
               burn = 200, mm = 300, opt1 = prior, stopping_rule_selected = FALSE,
               num_patients_restrict = NA, incr = 0.5)
  local_mocked_bindings(fit_trial_posterior = function(...) stop("Unexpected posterior call"))
  cases <- list(NN = 6.5, cohort = 2.5, burn = 1.5, mm = 0, alpha = c(0.4, 0.2),
                deltastop = 1.1, incr = 0, trial_mode = "Typo EWOC", stopping_rule_selected = "Yes",
                doselevels = c(0, 2, -1), doseprobs = c(0.1, Inf, 0.5))
  for (field in names(cases)) {
    bad <- args; bad[field] <- list(cases[[field]])
    expect_error(do.call(run_single_trial_simulation, bad), field)
  }
  bad <- args; bad$opt1$a0 <- NA_real_
  expect_error(do.call(run_single_trial_simulation, bad), "a0")
  bad <- args; bad$stopping_rule_selected <- TRUE; bad$num_patients_restrict <- 2.5
  expect_error(do.call(run_single_trial_simulation, bad), "num_patients_restrict")
})

test_that("batch validation rejects fractions missing values and unsupported designs", {
  cases <- list(M = 0, NN = 6.5, cohort = NA_real_, seed = Inf, mm = c(200, 300),
                burn = -1, increment = 0, lower_f_bound = 0.6, num_doses = 3.5,
                startdose = 9, trial_mode = "Typo EWOC")
  for (field in names(cases)) {
    bad <- reliability_inputs(); bad[field] <- list(cases[[field]])
    expect_error(check_trial_inputs(bad), field)
  }
  expect_error(check_trial_inputs(unname(reliability_inputs())), "named list")
  duplicated <- reliability_inputs(); duplicated <- c(duplicated, list(M = 2))
  expect_error(check_trial_inputs(duplicated), "unique")
  continuous <- modifyList(reliability_inputs(), list(dose_type = "Continuous", min_dose = 10,
                             max_dose = 50, max_dose_increment = 10, true_mtd = 30, true_p_min = 0.1))
  expect_error(check_trial_inputs(modifyList(continuous, list(max_dose_increment = 0))), "max_dose_increment")
  expect_error(check_trial_inputs(modifyList(continuous, list(max_dose = 10))), "min_dose.*max_dose")
  expect_error(check_trial_inputs(modifyList(continuous, list(true_beta0 = -2))), "true_beta0.*true_beta1")
  expect_error(check_trial_inputs(modifyList(continuous, list(true_p_min = 0))), "true_p_min")
  expect_error(check_trial_inputs(modifyList(continuous, list(trial_mode = "Accelerated EWOC"))), "Continuous trial_mode")
  expect_error(check_trial_inputs(modifyList(continuous, list(true_beta0 = -2, true_beta1 = 1))), "inconsistent")
  expect_error(run_trial_simulations_background(reliability_inputs(), list(a0 = 0), ncores = 1), "a0")
})

test_that("the cohort loop has an independent no-progress guard", {
  local_mocked_bindings(fit_trial_posterior = function(...) {
    data.frame(rho0 = 0.01, rho1 = 0.8, mtd = 0.5)
  })
  expect_error(run_ewoc_cohort_loop(rep(0, 6), rep(0, 6), 2.5, 6.5, 0.25,
                0.33, 0.85, 200, 300, 1, list(), FALSE, NA, 0, 0.33,
                recommend = function(...) list(dose_value = 0), treat = function(...) integer()),
               "cannot advance")
})

test_that("prior calibration and plotting reject fractional controls and bad shapes", {
  expect_error(run_mewoc_grid_search(0.33, 0.1, 0.2, 2, n_prior = 1000.5), "n_prior")
  expect_error(run_mewoc_grid_search(0.33, 0.1, 0.2, 2, bobyqa_starts = 0), "bobyqa_starts")
  expect_error(build_prior_plot(list(a0 = -1, b0 = 1, a1 = 1, b1 = 1), 4, 1), "a0")
})

test_that("explicit workers obey the option and the check-time cap", {
  old <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  opt <- options(mewoc.max_trial_workers = NULL)
  on.exit({options(opt); if (is.na(old)) Sys.unsetenv("_R_CHECK_LIMIT_CORES_") else Sys.setenv('_R_CHECK_LIMIT_CORES_' = old)}, add = TRUE)
  Sys.setenv('_R_CHECK_LIMIT_CORES_' = "TRUE")
  expect_equal(get_trial_worker_count(20, 8), 2L)
  expect_equal(get_app_trial_worker_count(20), 1L)
  options(mewoc.max_trial_workers = 1L)
  expect_equal(get_trial_worker_count(20, 8), 1L)
  Sys.setenv('_R_CHECK_LIMIT_CORES_' = "FALSE")
  options(mewoc.max_trial_workers = 3L)
  expect_equal(get_trial_worker_count(20, 8), 3L)
  expect_equal(get_trial_worker_count(1, 8), 1L)
  for (bad in list(0, 1.5, NA_real_, c(1, 2), "2")) expect_error(get_trial_worker_count(20, bad), "ncores")
})

test_that("app construction is private and has no lifecycle side effects", {
  for (pkg in c("future", "promises", "readxl")) skip_if_not_installed(pkg)
  appdir <- system.file("shiny", "mewoc", package = "MEWOC")
  oldwd <- setwd(appdir); on.exit(setwd(oldwd), add = TRUE)
  before <- ls(globalenv(), all.names = TRUE)
  search_before <- search()
  plan_before <- future::plan()
  resources_before <- shiny::resourcePaths()
  env <- new.env(parent = globalenv())
  app <- source("app.R", local = env)$value
  expect_s3_class(app, "shiny.appobj")
  expect_setequal(ls(globalenv(), all.names = TRUE), before)
  expect_identical(search(), search_before)
  expect_identical(future::plan(), plan_before)
  expect_identical(shiny::resourcePaths(), resources_before)
  expect_true(exists("run_trial_simulations_background", envir = env, inherits = FALSE))
  expect_true(is.function(app$onStart))
})

test_that("app lifecycle restores caller resources and future plan even after start failure", {
  skip_if_not_installed("future")
  state <- new.env(); state$plan <- "caller"; state$fail <- FALSE
  stop_callback <- NULL
  local_mocked_bindings(onStop = function(fun, session = NULL) { stop_callback <<- fun }, .package = "shiny")
  local_mocked_bindings(plan = function(strategy, ...) {
    if (missing(strategy)) return(state$plan)
    if (is.function(strategy)) {
      state$plan <- "app"
      if (state$fail) stop("simulated worker startup failure")
    } else state$plan <- strategy
  }, .package = "future")
  appdir <- system.file("shiny", "mewoc", package = "MEWOC")
  original_paths <- shiny::resourcePaths()
  old <- unname(original_paths["mewoc-images"])
  on.exit({if (length(old) == 1L && !is.na(old)) shiny::addResourcePath("mewoc-images", old)
           else shiny::removeResourcePath("mewoc-images")}, add = TRUE)
  shiny::addResourcePath("mewoc-images", tempdir())
  prior_paths <- shiny::resourcePaths()
  cleanup <- start_mewoc_app(appdir)
  expect_identical(state$plan, "app")
  expect_true(is.function(stop_callback))
  stop_callback()
  expect_identical(state$plan, "caller")
  expect_identical(shiny::resourcePaths(), prior_paths)
  cleanup() # cleanup is idempotent
  state$fail <- TRUE
  expect_error(start_mewoc_app(appdir), "simulated worker startup failure")
  expect_identical(state$plan, "caller")
  expect_identical(shiny::resourcePaths(), prior_paths)
})

test_that("the launch wrapper restores the plan after normal return or error", {
  skip_if_not_installed("future")
  state <- "caller"
  local_mocked_bindings(plan = function(strategy, ...) {
    if (missing(strategy)) return(state)
    state <<- strategy
  }, .package = "future")
  fail <- FALSE
  local_mocked_bindings(runApp = function(...) {
    future::plan("app")
    if (fail) stop("simulated run failure")
    42
  }, .package = "shiny")
  expect_equal(run_mewoc_app(), 42)
  expect_identical(state, "caller")
  fail <- TRUE
  expect_error(run_mewoc_app(), "simulated run failure")
  expect_identical(state, "caller")
})
