context("Run data quality check on Bandon Valley pipeline output")

test_that("BAN pipeline works...", {

  suppressWarnings(run_pipelines(path = path, PopID = "BAN"))

})

test_that("BAN outputs all files...", {

  expect_true(file.exists("Brood_data_BAN.csv"))
  expect_true(file.exists("Capture_data_BAN.csv"))
  expect_true(file.exists("Individual_data_BAN.csv"))
  expect_true(file.exists("Location_data_BAN.csv"))

})
