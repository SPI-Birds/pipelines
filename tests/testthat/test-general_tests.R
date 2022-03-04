context("Test general tests...")

set.seed(74)

## Correctly formatted pipeline outputs
pipeline_output_correct <- tibble::lst(

  Brood_data = data_templates$v1.1$Brood_data %>%
    dplyr::slice(-1) %>%
    dplyr::add_row(
      BroodID = as.character(paste0("B-", 1:7)),
      PopID = as.character("HOG"),
      BreedingSeason = as.integer(2021),
      Species = as.character("PARMAJ"),
      FemaleID = replicate(7, paste(sample(1:9, 6, replace = TRUE), collapse = "")), # Generate random IDs (6 digits)
      MaleID = replicate(7, paste0(paste(sample(1:9, 5, replace = TRUE),  collapse = ""), sample(LETTERS, 1))), # Generate random IDs (5 digits, 1 number)
      OriginalTarsusMethod = c("Alternative", "Standard", "Oxford", NA, NA, NA, NA),
      ExperimentID = c("PHENOLOGY","COHORT", "PARENTAGE", "SURVIVAL", "OTHER", "SURVIVAL; OTHER", NA)
    ),

  Capture_data = data_templates$v1.1$Capture_data %>%
    dplyr::slice(-1) %>%
    dplyr::add_row(
      CaptureID = as.character(paste0("C-", 1:14)),
      IndvID = c(Brood_data$FemaleID, c(Brood_data$MaleID)),
      Species = as.character("PARMAJ"),
      BreedingSeason = as.integer(2021),
      CaptureDate = as.Date("2021-04-01"),
      CaptureAlive = TRUE,
      ReleaseAlive = TRUE,
      CapturePopID = as.character("HOG"),
      ReleasePopID = as.character("HOG"),
      ExperimentID = rep(c("PHENOLOGY","COHORT", "PARENTAGE", "SURVIVAL", "OTHER", "SURVIVAL; OTHER", NA), 2)
    ),

  Individual_data = data_templates$v1.1$Individual_data %>%
    dplyr::slice(-1) %>%
    dplyr::add_row(
      IndvID = c(Brood_data$FemaleID, c(Brood_data$MaleID)),
      Species = as.character("PARMAJ"),
      PopID = as.character("HOG"),
      RingSeason = as.integer(2021),
      RingAge = rep(c(rep(c("chick", "adult"), 3), NA), 2),
      Sex_calculated = c(rep("F", 5), "C", NA, rep("M", 5), "C", NA),
      Sex_genetic = c(rep("F", 5), "C", NA, rep("M", 5), "C", NA)
    ),

  Location_data = data_templates$v1.1$Location_data %>%
    dplyr::slice(-1) %>%
    dplyr::add_row(
      LocationID = as.character(paste0("NB-"), 1:5),
      LocationType = c(rep("NB", 4), "MN"),
      PopID = as.character("HOG"),
      HabitatType = c("deciduous","evergreen","mixed", "urban", NA)
    )

)


## Create incorrectly formatted dummy data
pipeline_output_incorrect <- tibble::lst(

  Brood_data = data_templates$v1.1$Brood_data %>%
    dplyr::slice(-1) %>%
    dplyr::add_row(
      BroodID = as.character(c(1, 1, NA)),
      PopID = NA,
      BreedingSeason = NA,
      Species = NA,
      ClutchType_observed = "nonsense",
      ClutchType_calculated = "nonsense",
      OriginalTarsusMethod = "nonsense",
      ExperimentID = "nonsense"
    ) %>%
    dplyr::mutate(BroodID = as.numeric(.data$BroodID)) %>% # Wrong class
    dplyr::select(-.data$ClutchSize_observed), # Missing column

  Capture_data = data_templates$v1.1$Capture_data %>%
    dplyr::slice(-1) %>%
    dplyr::add_row(
      CaptureID = as.character(c(1, 1, NA)),
      IndvID = NA,
      Species = NA,
      BreedingSeason = NA,
      CaptureDate = NA,
      CapturePopID = NA,
      ExperimentID = "nonsense"
    ) %>%
    dplyr::mutate(CaptureID = as.numeric(.data$CaptureID)) %>% # Wrong class
    dplyr::select(-.data$Mass), # Missing column

  Individual_data = data_templates$v1.1$Individual_data %>%
    dplyr::slice(-1) %>%
    dplyr::add_row(
      IndvID = as.character(c(1, 1, NA)),
      Species = NA,
      PopID = NA,
      RingSeason = NA,
      RingAge = "nonsense",
      Sex_calculated = "nonsense",
      Sex_genetic = "nonsense"
    ) %>%
    dplyr::mutate(IndvID = as.numeric(.data$IndvID)) %>% # Wrong class
    dplyr::select(-.data$BroodIDLaid), # Missing column

  Location_data = data_templates$v1.1$Location_data %>%
    dplyr::slice(-1) %>%
    dplyr::add_row(
      LocationID = NA,
      PopID = NA,
      LocationType = "nonsense",
      HabitatType = "nonsense"
    ) %>%
    dplyr::mutate(LocationID = as.numeric(.data$LocationID)) %>% # Wrong class
    dplyr::select(-.data$StartSeason) # Missing column

)


## General tests (for pipelines formatted to standard protocol version 1.1.0)

test_that("Column class test function works", {

  ## Brood data: Test that all column classes are expected
  test_col_classes(pipeline_output_correct, "Brood")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output_correct, "Capture")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output_correct, "Individual")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output_correct, "Location")

  ## Should be errors. These fail because the number of columns are not the same.
  expect_error(test_col_classes(pipeline_output_incorrect, "Brood"))
  expect_error(test_col_classes(pipeline_output_incorrect, "Capture"))
  expect_error(test_col_classes(pipeline_output_incorrect, "Individual"))
  expect_error(test_col_classes(pipeline_output_incorrect, "Location"))

})


test_that("Column present test function works", {

  ## Brood data: Test that columns are present
  test_col_present(pipeline_output_correct, "Brood")

  ## Capture data: Test that columns are present
  test_col_present(pipeline_output_correct, "Capture")

  ## Individual data: Test that columns are present
  test_col_present(pipeline_output_correct, "Individual")

  ## Location data: Test that columns are present
  test_col_present(pipeline_output_correct, "Location")

  ## Should be errors. These fail because the number of columns are not the same.
  expect_error(test_col_present(pipeline_output_incorrect, "Brood"))
  expect_error(test_col_present(pipeline_output_incorrect, "Capture"))
  expect_error(test_col_present(pipeline_output_incorrect, "Individual"))
  expect_error(test_col_present(pipeline_output_incorrect, "Location"))

})


test_that("ID columns match the expected format for the pipeline", {

  ## FemaleID format is as expected
  test_ID_format(pipeline_output_correct, ID_col = "FemaleID", ID_format = "^[:digit:]+$")

  ## MaleID format is as expected
  test_ID_format(pipeline_output_correct, ID_col = "MaleID", ID_format = "^[[:digit:][:alpha:]]+$")

  ## IndvID format in Capture data is as expected
  test_ID_format(pipeline_output_correct, ID_col = "C-IndvID", ID_format = "^[[:digit:][:alpha:]]+$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output_correct, ID_col = "I-IndvID", ID_format = "^[[:digit:][:alpha:]]+$")

  ## Should be errors. These fail because the ID format is incorrect
  expect_error(test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[:digit:]+$"))
  expect_error(test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[:alpha:]+$"))
  expect_error(test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[:alpha:]+$"))
  expect_error(test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[:digit:]+$"))

})


test_that("Key columns only contain unique values", {

  ## BroodID has only unique values
  test_unique_values(pipeline_output_correct, "BroodID")

  ## CaptureID has only unique values
  test_unique_values(pipeline_output_correct, "CaptureID")

  ## PopID-IndvID has only unique values
  test_unique_values(pipeline_output_correct, "PopID-IndvID")

  ## Should be errors. These fail because there are duplicate values.
  expect_error(test_unique_values(pipeline_output_incorrect, "BroodID"))
  expect_error(test_unique_values(pipeline_output_incorrect, "Capture"))
  expect_error(test_unique_values(pipeline_output_incorrect, "Individual"))

})


test_that("Key columns in each table do not have NAs", {

  ## Brood
  test_NA_columns(pipeline_output_correct, "Brood")

  ## Capture
  test_NA_columns(pipeline_output_correct, "Capture")

  ## Individual
  test_NA_columns(pipeline_output_correct, "Individual")

  ## Location
  test_NA_columns(pipeline_output_correct, "Location")

  ## Should be errors. These fail because there are NAs.
  expect_error(test_NA_columns(pipeline_output_incorrect, "BroodID"))
  expect_error(test_NA_columns(pipeline_output_incorrect, "Capture"))
  expect_error(test_NA_columns(pipeline_output_incorrect, "Individual"))

})


test_that("Categorical columns do not have unexpected values", {

  ## Brood
  test_category_columns(pipeline_output_correct, "Brood")

  ## Capture
  test_category_columns(pipeline_output_correct, "Capture")

  ## Individual
  test_category_columns(pipeline_output_correct, "Individual")

  ## Location
  test_category_columns(pipeline_output_correct, "Location")

  ## Should be errors. These fail because there are unexpected categories.
  expect_error(test_category_columns(pipeline_output_incorrect, "Brood"))
  expect_error(test_category_columns(pipeline_output_incorrect, "Capture"))
  expect_error(test_category_columns(pipeline_output_incorrect, "Individual"))
  expect_error(test_category_columns(pipeline_output_incorrect, "Location"))


})
