context("Run data quality check on Santo Stefano Quisquina pipeline output")

test_that("SSQ pipeline works...", {

  suppressWarnings(run_pipelines(path = path, PopID = "SSQ"))

})

test_that("SSQ outputs all files...", {

  expect_true(file.exists("Brood_data_SSQ.csv"))
  expect_true(file.exists("Capture_data_SSQ.csv"))
  expect_true(file.exists("Individual_data_SSQ.csv"))
  expect_true(file.exists("Location_data_SSQ.csv"))

})
