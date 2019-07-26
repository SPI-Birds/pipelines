context("Run data quality check on University of Antwerp pipeline output")

test_that("UAN pipeline works...", {

  suppressWarnings(run_pipelines(path = path, PopID = c("BOS", "PEE")))

})

test_that("UAN outputs all files...", {

  expect_true(file.exists("Brood_data_UAN.csv"))
  expect_true(file.exists("Capture_data_UAN.csv"))
  expect_true(file.exists("Individual_data_UAN.csv"))
  expect_true(file.exists("Location_data_UAN.csv"))

})
