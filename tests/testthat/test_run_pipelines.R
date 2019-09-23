context("Test that run_pipelines functions as expected...")

test_that("All 4 tables are created...", {

  expect_true(exists("Brood_data", where = pipeline_output))
  expect_true(exists("Capture_data", where = pipeline_output))
  expect_true(exists("Individual_data", where = pipeline_output))
  expect_true(exists("Location_data", where = pipeline_output))

})
