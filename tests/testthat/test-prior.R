test_that("run_mewoc_grid_search returns calibrated prior candidates", {
  skip_if_not_installed("minqa")

  set.seed(20260913)
  res <- run_mewoc_grid_search(
    theta = 0.33,
    p0 = 0.3,
    p1 = 0.1,
    condition = 2,
    n_prior = 1200,
    n_search_prior = 1000,
    bobyqa_starts = 1,
    bobyqa_maxfun = 40,
    seed = 20260913
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(c("a0", "b0", "a1", "b1") %in% names(res)))
  expect_equal(nrow(res), 1L)
  expect_true(all(is.finite(unlist(res[c("a0", "b0", "a1", "b1")]))))
})

test_that("build_prior_plot returns a ggplot", {
  skip_if_not_installed("ggplot2")

  prior <- data.frame(a0 = 1.2, b0 = 2.8, a1 = 1.1, b1 = 2.9)
  plot <- build_prior_plot(prior, num_doses = 3, seed_value = 123)
  expect_s3_class(plot, "ggplot")
})
test_that("prior search is BOBYQA only: no search_method argument and no fixed grid", {
  expect_false("search_method" %in% names(formals(run_mewoc_grid_search)))
  expect_false(exists("run_mewoc_fixed_grid_search", mode = "function"))
  expect_error(run_mewoc_grid_search(theta = 0.33, p0 = 0.1, p1 = 0.2, condition = 2, search_method = "fixed_grid"),
               "unused argument")
})
