context("Run data quality check on Harjavalta pipeline output")

test_that("HAR pipeline works...", {

  suppressWarnings(run_pipelines(path = path, PopID = "HAR"))

})

test_that("HAR outputs all files...", {

  expect_true(file.exists("Brood_data_HAR.csv"))
  expect_true(file.exists("Capture_data_HAR.csv"))
  expect_true(file.exists("Individual_data_HAR.csv"))
  expect_true(file.exists("Location_data_HAR.csv"))

})
