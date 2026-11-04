test_that("continuous tree caps escalation at the maximum increment and labels actual doses", {
  ss <- list(rho0 = rep(0.01, 200), rho1 = rep(0.05, 200), mtd = rep(3, 200))
  d <- compute_continuous_tree_decision(ss, theta = 0.33, alpha_value = 0.25, current_dose = 0.25, max_increment_std = 0.25)
  expect_equal(d$next_dose_value, 0.5)
  expect_equal(continuous_tree_dose_label(0.25, 100, 500, "mg"), "200.0 mg\n(x=0.25)")
})
