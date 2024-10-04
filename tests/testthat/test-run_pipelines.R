testthat::skip_if(!exists("data_path"))

pipeline_output <- run_pipelines(path = data_path, PopID = c("CHO", "SSQ"))

test_that("All 4 tables are created...", {

  expect_true(exists("Brood_data", where = pipeline_output))
  expect_true(exists("Capture_data", where = pipeline_output))
  expect_true(exists("Individual_data", where = pipeline_output))
  expect_true(exists("Location_data", where = pipeline_output))
  expect_true(exists("protocol_version", where = pipeline_output))
  expect_length(pipeline_output$protocol_version, 1)

})
