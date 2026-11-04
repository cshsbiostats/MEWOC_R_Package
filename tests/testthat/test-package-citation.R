test_that("package citation names the confirmed authors and installed version", {
  entry <- utils::citation("MEWOC")
  expect_length(entry, 1L)
  expect_equal(unlist(entry$author$given, use.names = FALSE), c("Shaowli", "Xuyan", "Mourad"))
  expect_equal(unlist(entry$author$family, use.names = FALSE), c("Kabir", "Fan", "Tighiouart"))
  expect_match(entry$note, as.character(utils::packageVersion("MEWOC")), fixed = TRUE)
  expect_equal(entry$bibtype, "Manual")
})
