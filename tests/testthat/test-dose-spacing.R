test_that("discrete dose labels allow mixed units and combination levels", {
  labels <- build_discrete_dose_labels(c(10, 1, 5, NA), c("mg", "g", "mg + drug X 50 mg/m2", "drug Y only"), 4)
  expect_equal(labels, c("10 mg", "1 g", "5 mg + drug X 50 mg/m2", "drug Y only"))
  expect_equal(build_discrete_dose_labels(c(NA, 20), c("", "mg"), 2), c("Dose 1", "20 mg"))
})

test_that("discrete simulation design is level-based whatever the actual doses are", {
  inputs <- list(
    num_doses = 4L, startdose = 1L, doseprobs = c(0.05, 0.1, 0.2, 0.35),
    actual_dose_levels = c(10, 1, 5, 7), actual_dose_units = c("mg", "g", "mg + X", "mg + Y"),
    dose_unit = ""
  )
  design <- prepare_discrete_simulation_design(inputs)
  expect_equal(design$doselevels, seq(0, 1, length.out = 4))
  expect_equal(design$startdose, 1L)
})
