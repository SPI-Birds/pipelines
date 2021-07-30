context("Test general tests...")

## Correctly formatted pipeline outputs
pipeline_output <- lst(Brood_data = brood_data_template,
                       Capture_data = capture_data_template,
                       Individual_data = individual_data_template,
                       Location_data = location_data_template)

## Create some additional ID values
pipeline_output[[1]]$FemaleID <- rep("123456", 7) ## All digits
pipeline_output[[1]]$MaleID <- rep("12345B", 7) ## With a character
pipeline_output[[2]]$IndvID <- rep("12345B", 6) ## With a character

## Create incorrectly formatted dummy data
pipeline_output_incorrect <- lst(Brood_data = tibble::tibble(BroodID = c(1,1,NA),
                                                             PopID = NA,
                                                             BreedingSeason = NA,
                                                             Species = NA,
                                                             ClutchType_observed = "nonsense",
                                                             ClutchType_calculated = "nonsense",
                                                             OriginalTarsusMethod = "nonsense",
                                                             ExperimentID = "nonsense"),
                                 Capture_data = tibble::tibble(CaptureID = c(1,1,NA),
                                                               IndvID = NA,
                                                               Species = NA,
                                                               BreedingSeason = NA,
                                                               CaptureDate = NA,
                                                               CapturePopID = NA,
                                                               ExperimentID = "nonsense"),
                                 Individual_data = tibble::tibble(IndvID = c(1,1,NA),
                                                                  Species = NA,
                                                                  PopID = NA,
                                                                  RingSeason = NA,
                                                                  RingAge = "nonsense",
                                                                  Sex_calculated = "nonsense",
                                                                  Sex_genetic = "nonsense"),
                                 Location_data = tibble::tibble(LocationID = NA,
                                                                PopID = NA,
                                                                LocationType = "nonsense",
                                                                HabitatType = "nonsense"))


## General tests (for pipelines formatted to standard protocol version 1.1.0)

test_that("Column class test function works", {

  ## Brood data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Brood")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location")

  ## Should be errors. These fail because the number of columns are not the same.
  expect_error(test_col_classes(pipeline_output_incorrect, "Brood"))
  expect_error(test_col_classes(pipeline_output_incorrect, "Capture"))
  expect_error(test_col_classes(pipeline_output_incorrect, "Individual"))
  expect_error(test_col_classes(pipeline_output_incorrect, "Location"))

})


test_that("Column present test function works", {

  ## Brood data: Test that columns are present
  test_col_present(pipeline_output, "Brood")

  ## Capture data: Test that columns are present
  test_col_present(pipeline_output, "Capture")

  ## Individual data: Test that columns are present
  test_col_present(pipeline_output, "Individual")

  ## Location data: Test that columns are present
  test_col_present(pipeline_output, "Location")

  ## Should be errors. These fail because the number of columns are not the same.
  expect_error(test_col_present(pipeline_output_incorrect, "Brood"))
  expect_error(test_col_present(pipeline_output_incorrect, "Capture"))
  expect_error(test_col_present(pipeline_output_incorrect, "Individual"))
  expect_error(test_col_present(pipeline_output_incorrect, "Location"))

})


test_that("ID columns match the expected format for the pipeline", {

  ## FemaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[:digit:]+$")

  ## MaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[[:digit:][:alpha:]]+$")

  ## IndvID format in Capture data is as expected
  test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[[:digit:][:alpha:]]+$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[[:digit:][:alpha:]]+$")

  ## Should be errors. These fail because the ID format is incorrect
  expect_error(test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[:digit:]+$"))
  expect_error(test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[:alpha:]+$"))
  expect_error(test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[:alpha:]+$"))
  expect_error(test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[:digit:]+$"))



})


test_that("Key columns only contain unique values", {

  ## BroodID has only unique values
  test_unique_values(pipeline_output, "BroodID")

  ## CaptureID has only unique values
  test_unique_values(pipeline_output, "CaptureID")

  ## PopID-IndvID has only unique values
  test_unique_values(pipeline_output, "PopID-IndvID")

  ## Should be errors. These fail because there are duplicate values.
  expect_error(test_unique_values(pipeline_output_incorrect, "BroodID"))
  expect_error(test_unique_values(pipeline_output_incorrect, "Capture"))
  expect_error(test_unique_values(pipeline_output_incorrect, "Individual"))

})


test_that("Key columns in each table do not have NAs", {

  ## Brood
  test_NA_columns(pipeline_output, "Brood")

  ## Capture
  test_NA_columns(pipeline_output, "Capture")

  ## Individual
  test_NA_columns(pipeline_output, "Individual")

  ## Location
  test_NA_columns(pipeline_output, "Location")

  ## Should be errors. These fail because there are NAs.
  expect_error(test_NA_columns(pipeline_output_incorrect, "BroodID"))
  expect_error(test_NA_columns(pipeline_output_incorrect, "Capture"))
  expect_error(test_NA_columns(pipeline_output_incorrect, "Individual"))

})


test_that("Categorical columns do not have unexpected values", {

  ## Brood
  test_category_columns(pipeline_output, "Brood")

  ## Capture
  test_category_columns(pipeline_output, "Capture")

  ## Individual
  test_category_columns(pipeline_output, "Individual")

  ## Location
  test_category_columns(pipeline_output, "Location")

  ## Should be errors. These fail because there are unexpected categories.
  expect_error(test_category_columns(pipeline_output_incorrect, "Brood"))
  expect_error(test_category_columns(pipeline_output_incorrect, "Capture"))
  expect_error(test_category_columns(pipeline_output_incorrect, "Individual"))
  expect_error(test_category_columns(pipeline_output_incorrect, "Location"))


})
