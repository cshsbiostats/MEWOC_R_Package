test_that("the public API supports only one chain and defaults to it", {
  prior <- data.frame(a0 = 1.23, b0 = 0.77, a1 = 0.34, b1 = 1.66)
  args <- list(trial_id = 1, doselevels = c(0, 0.5, 1), doseprobs = c(0.05, 0.2, 0.5),
               startdose = 1, cohort = 3, NN = 3, alpha = 0.25, theta = 0.33,
               burn = 400, mm = 300, opt1 = prior, stopping_rule_selected = FALSE,
               num_patients_restrict = NA, incr = 0.5)
  for (bad in list(0, 2, 1.5, NA_real_, Inf, c(1, 2), TRUE, "1", NULL)) {
    expect_error(do.call(run_single_trial_simulation, c(args, list(chains = bad))), "chains must be 1")
  }
  set.seed(123)
  implicit <- do.call(run_single_trial_simulation, args)
  set.seed(123)
  explicit <- do.call(run_single_trial_simulation, c(args, list(chains = 1L)))
  expect_true(implicit$success)
  expect_identical(implicit, explicit)
  expect_error(fit_trial_posterior(build_jags_model_string(), c(0, 0, 0), c(0, 0, 0),
                                  prior, 0.33, 3, 2, 400, 300), "chains must be 1")
})

test_that("nearest-level rounding is preserved across simulation conduct and tree", {
  ss <- data.frame(mtd = seq(0.3, 0.7, length.out = 1001), rho0 = 0.01, rho1 = 0.7)
  levels <- c(0, 0.5, 1)
  sim <- recommend_next_dose(ss, 0.25, levels, 0.5, c(0, 0.5), 0.5)
  design <- list(doselevels = levels, actual_dose_levels = c(10, 20, 30))
  conduct <- recommend_conduct_categorical_dose(ss, 0.25, design, 0.5, c(0, 0.5))
  tree <- compute_tree_decision(ss, levels, 0.33, 0.25, 0.5, c(0, 0.5), 0.5)
  expect_equal(as.numeric(quantile(ss$mtd, 0.25)), 0.4)
  expect_equal(sim$dose_value, 0.5)
  expect_equal(conduct$dose_value, sim$dose_value)
  expect_equal(tree$next_dose_value, sim$dose_value)
  expect_gt(mean(ss$mtd < sim$dose_value), 0.25)

  tie <- ss
  tie$mtd <- rep(0.25, nrow(tie))
  expect_equal(recommend_next_dose(tie, 0.25, levels, 0.5, c(0, 0.5), 0.5)$dose_value, 0)
  expect_equal(recommend_conduct_categorical_dose(tie, 0.25, design, 0.5, c(0, 0.5))$dose_value, 0)
})

test_that("alpha reaches exactly 0.5 and remains there without exceeding it", {
  expect_equal(build_alpha_sequence(0.25, 0.1, 6), c(0.25, 0.35, 0.45, 0.5, 0.5, 0.5))
  expect_identical(tail(build_alpha_sequence(0.25, 0.1, 6), 1), 0.5)
  expect_equal(build_alpha_sequence(0.25, 0.05, 8), c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.5, 0.5))
  expect_equal(build_alpha_sequence(0.49, 0.1, 3), c(0.49, 0.5, 0.5))
  expect_equal(build_alpha_sequence(0.5, 0.1, 3), rep(0.5, 3))
  expect_equal(build_alpha_sequence(0.25, 0.1, 0), numeric())
  expect_error(build_alpha_sequence(0.25, 0, 3), "increment")
  expect_error(build_alpha_sequence(0.6, 0.1, 3), "lower_f_bound")
  expect_error(build_alpha_sequence(0.25, 0.1, 2.5), "num_updates")
  schedule <- build_alpha_sequence(0.25, 0.1, 5)
  expect_equal(advance_alpha_step_if_allowed(2, schedule, c(1, 1), c(0, 0), 0.33), 2)
})

test_that("conduct and simulation alpha caps agree for a nondividing increment", {
  local_mocked_bindings(fit_trial_posterior = function(...) {
    data.frame(rho0 = rep(0.01, 100), rho1 = rep(0.7, 100), mtd = rep(0.4, 100))
  })
  design <- list(dose_type = "Discrete", num_doses = 3L, startdose = 1L,
                 actual_dose_levels = c(10, 20, 30), doselevels = c(0, 0.5, 1),
                 dose_unit = "mg", dose_units = rep("mg", 3))
  settings <- list(theta = 0.33, deltastop = 0.85, lower_alpha = 0.25, increment = 0.1,
                   alpha_increment_cutoff = 0.33, seed = 7, cohort_size = 1L,
                   total_sample_size = 20L, trial_mode = "Standard EWOC",
                   efficiency_rule = FALSE, num_patients_restrict = NA)
  rows <- data.frame(Cohort = as.character(1:4), Patient = as.character(1:4),
                     Administered_Dose = rep(10, 4), DLT = rep(0L, 4),
                     Is_Unevaluable = FALSE, Uploaded_Recommended_Dose = NA,
                     Uploaded_Alpha_Used = NA)
  out <- conduct_evaluate_trial(rows, design, settings, list(a0 = 1, b0 = 1, a1 = 1, b1 = 1))
  expect_identical(out$alpha_for_next, 0.5)
  expect_equal(out$alpha_for_next, tail(build_alpha_sequence(0.25, 0.1, 4), 1))
})

test_that("rho1 draws follow the conditional beta CDF on the retained bounds", {
  p <- c(0, 0.01, 0.25, 0.5, 0.75, 0.99, 1)
  expect_equal(q_mewoc_rho1(p, 1, 1), 0.001 + p * 0.998)
  for (shapes in list(c(0.1, 1.9), c(1.9, 0.1), c(2, 3))) {
    x <- q_mewoc_rho1(p, shapes[1], shapes[2])
    lo <- pbeta(0.001, shapes[1], shapes[2])
    mass <- pbeta(0.999, shapes[1], shapes[2]) - lo
    expect_equal((pbeta(x, shapes[1], shapes[2]) - lo) / mass, p, tolerance = 1e-10)
    expect_true(all(x >= 0.001 & x <= 0.999))
  }
  expect_match(build_jags_model_string(), "T(0.001, 0.999)", fixed = TRUE)
  expect_error(q_mewoc_rho1(0.5, 0, 1), "positive finite beta shapes")
})

test_that("calibration probabilities agree with the actual truncated prior", {
  prior <- run_mewoc_grid_search(0.33, 0.1, 0.2, 2, seed = 321,
                                 n_prior = 20000, bobyqa_starts = 1)
  mass <- pbeta(0.999, prior$a0, prior$b0) - pbeta(0.001, prior$a0, prior$b0)
  expected_p1 <- (pbeta(0.33, prior$a0, prior$b0) - pbeta(0.001, prior$a0, prior$b0)) / mass
  expected_p0 <- integrate(function(rho1) {
    dbeta(rho1, prior$a0, prior$b0) * pbeta(0.33 / rho1, prior$a1, prior$b1, lower.tail = FALSE)
  }, lower = 0.33, upper = 0.999, rel.tol = 1e-8)$value / mass
  expect_lt(abs(prior$matched_p1 - expected_p1), 5 * sqrt(expected_p1 * (1 - expected_p1) / 20000) + 0.001)
  expect_lt(abs(prior$matched_p0 - expected_p0), 5 * sqrt(expected_p0 * (1 - expected_p0) / 20000) + 0.001)
})

test_that("prior plots use the same truncated maximum-dose prior", {
  prior <- data.frame(a0 = 0.1, b0 = 1.9, a1 = 0.5, b1 = 1.5)
  plot <- build_prior_plot(prior, num_doses = 2, seed_value = 123)
  rho1 <- plot$data$value[plot$data$variable == "2"]
  expect_length(rho1, 60000)
  expect_true(all(rho1 >= 0.001 & rho1 <= 0.999))
  expected <- (pbeta(0.1, 0.1, 1.9) - pbeta(0.001, 0.1, 1.9)) /
              (pbeta(0.999, 0.1, 1.9) - pbeta(0.001, 0.1, 1.9))
  expect_equal(mean(rho1 < 0.1), expected, tolerance = 0.01)
})
