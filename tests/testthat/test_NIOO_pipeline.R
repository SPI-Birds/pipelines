context("Run data quality check on NIOO pipeline output")

test_that("NIOO pipeline works...", {

  suppressWarnings(run_pipelines(path = path,
                                 PopID = c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES")))

})

test_that("NIOO outputs all files...", {

  expect_true(file.exists("Brood_data_NIOO.csv"))
  expect_true(file.exists("Capture_data_NIOO.csv"))
  expect_true(file.exists("Individual_data_NIOO.csv"))
  expect_true(file.exists("Location_data_NIOO.csv"))

})
