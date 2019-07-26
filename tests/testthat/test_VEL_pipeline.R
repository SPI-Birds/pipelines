context("Run data quality check on Velky Kosir pipeline output")

test_that("VEL pipeline works...", {

  suppressWarnings(run_pipelines(path = path, PopID = "VEL"))

})

test_that("VEL outputs all files...", {

  expect_true(file.exists("Brood_data_VEL.csv"))
  expect_true(file.exists("Capture_data_VEL.csv"))
  expect_true(file.exists("Individual_data_VEL.csv"))
  expect_true(file.exists("Location_data_VEL.csv"))

})
