context("Run data quality check on Choupal pipeline output")

test_that("CHO pipeline works...", {

  suppressWarnings(run_pipelines(path = path, PopID = "CHO"))

})

test_that("CHO outputs all files...", {

  expect_true(file.exists("Brood_data_CHO.csv"))
  expect_true(file.exists("Capture_data_CHO.csv"))
  expect_true(file.exists("Individual_data_CHO.csv"))
  expect_true(file.exists("Location_data_CHO.csv"))

})
