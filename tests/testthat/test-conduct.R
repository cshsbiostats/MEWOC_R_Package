test_that("conduct workbook helpers build expected tables", {
  design <- build_conduct_design_table(
    p0 = 0.3, p1 = 0.1,
    a0 = 1.2, b0 = 2.3, a1 = 3.4, b1 = 4.5,
    num_doses = 4, startdose = 1,
    theta = 0.33, deltastop = 0.25,
    lower_f_bound = 0.25, increment = 0.05,
    trial_mode = "No", max_accel_dose_level = NA,
    seed = 123, burn = 100, mm = 200
  )

  expect_s3_class(design, "data.frame")
  expect_equal(ncol(design), 2L)
  expect_true("Trial Design Parameter" %in% names(design))
  expect_true("Value" %in% names(design))
  expect_true("P(MTD < Min)" %in% design[["Trial Design Parameter"]])

  patients <- build_conduct_patient_template()
  expect_s3_class(patients, "data.frame")
  expect_equal(
    names(patients),
    c("Cohort", "Patient", "Dose Given", "DLT Status", "Dose Recommended", "\u03b1")
  )
  expect_equal(nrow(patients), 3L)
})

test_that("write_conduct_workbook writes an xlsx file", {
  skip_if_not_installed("openxlsx")

  design <- build_conduct_design_table(
    p0 = 0.3, p1 = 0.1,
    a0 = 1.2, b0 = 2.3, a1 = 3.4, b1 = 4.5,
    num_doses = 4, startdose = 1,
    theta = 0.33, deltastop = 0.25,
    lower_f_bound = 0.25, increment = 0.05,
    trial_mode = "No", max_accel_dose_level = NA,
    seed = 123, burn = 100, mm = 200
  )
  patients <- build_conduct_patient_template()
  outfile <- tempfile(fileext = ".xlsx")

  expect_equal(write_conduct_workbook(outfile, design, patients), outfile)
  expect_true(file.exists(outfile))
  expect_gt(file.info(outfile)$size, 0)
})
