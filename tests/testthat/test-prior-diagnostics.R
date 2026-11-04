test_that("prior calibration returns absolute matching errors and validation MCSEs", {
  prior <- run_mewoc_grid_search(0.33, 0.1, 0.2, 2, seed = 1,
                                 n_prior = 6000, bobyqa_starts = 1)
  fields <- c("abs_error_p0", "abs_error_p1", "mcse_p0", "mcse_p1")
  expect_true(all(fields %in% names(prior)))
  expect_equal(prior$abs_error_p0, abs(prior$matched_p0 - prior$target_p0))
  expect_equal(prior$abs_error_p1, abs(prior$matched_p1 - prior$target_p1))
  expect_equal(prior$mcse_p0, sqrt(prior$matched_p0 * (1 - prior$matched_p0) / 6000))
  expect_equal(prior$mcse_p1, sqrt(prior$matched_p1 * (1 - prior$matched_p1) / 6000))
  expect_true(all(is.finite(unlist(prior[fields]))))
  expect_true(all(unlist(prior[fields]) >= 0))
  # No optimizer/acceptance status or rejection threshold is introduced.
  expect_false(any(c("accepted", "converged", "acceptance_threshold") %in% names(prior)))
})
