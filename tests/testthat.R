if (!requireNamespace("testthat", quietly = TRUE)) {
  message("testthat is not installed; skipping tests")
  quit(save = "no", status = 0)
}
library(testthat)
library(MEWOC)

test_check("MEWOC")