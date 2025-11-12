context("Test general tests...")

# Create correctly formatted standard data v1.1.0 (based on dummy data; see R/dummy_data.R)
# NB: does contain duplicated IDs (relevant for test_unique_values()) and unexpected categorical values (relevant for test_category_columns())
correct_outputs <- create_dummy_data()  %>%
  purrr::map(.f = ~{

    .x %>%
      dplyr::select(-"Row", -"CheckID")

  })

# Test general tests (see R/test_general_format.R)

test_that("Expected columns are present", {

  # Create standard data with missing columns
  incorrect_outputs <- tibble::lst(Brood_data = correct_outputs$Brood_data %>% dplyr::select(-"ExperimentID"),
                                   Capture_data = correct_outputs$Capture_data %>% dplyr::select(-"CaptureID"),
                                   Individual_data = correct_outputs$Individual_data %>% dplyr::select(-"RingAge"),
                                   Location_data = correct_outputs$Location_data %>% dplyr::select(-"EndSeason"))

  ## Brood data: Test that all columns are present
  test_col_present(correct_outputs, "Brood", protocol_version = "1.1.0")

  ## Capture data: Test that all columns are present
  test_col_present(correct_outputs, "Capture", protocol_version = "1.1.0")

  ## Individual data: Test that all columns are present
  test_col_present(correct_outputs, "Individual", protocol_version = "1.1.0")

  ## Location data: Test that all columns are present
  test_col_present(correct_outputs, "Location", protocol_version = "1.1.0")

  ## Should be errors. These fail because of missing columns.
  testthat::expect_failure(test_col_present(incorrect_outputs, "Brood", protocol_version = "1.1.0"))
  testthat::expect_failure(test_col_present(incorrect_outputs, "Capture", protocol_version = "1.1.0"))
  testthat::expect_failure(test_col_present(incorrect_outputs, "Individual", protocol_version = "1.1.0"))
  testthat::expect_failure(test_col_present(incorrect_outputs, "Location", protocol_version = "1.1.0"))

})


testthat::test_that("Column class test function works", {

  # Create standard data with wrong column classes
  incorrect_outputs <- tibble::lst(Brood_data = correct_outputs$Brood_data %>% dplyr::mutate(BreedingSeason = as.numeric(.data$BreedingSeason)),
                                   Capture_data = correct_outputs$Capture_data %>% dplyr::mutate(Age_observed = as.numeric(.data$Age_observed)),
                                   Individual_data = correct_outputs$Individual_data %>% dplyr::mutate(RingSeason = as.numeric(.data$RingSeason)),
                                   Location_data = correct_outputs$Location_data %>% dplyr::mutate(StartSeason = as.numeric(.data$StartSeason)))


  ## Brood data: Test that all column classes are expected
  test_col_classes(correct_outputs, "Brood", protocol_version = "1.1.0")

  ## Capture data: Test that all column classes are expected
  test_col_classes(correct_outputs, "Capture", protocol_version = "1.1.0")

  ## Individual data: Test that all column classes are expected
  test_col_classes(correct_outputs, "Individual", protocol_version = "1.1.0")

  ## Location data: Test that all column classes are expected
  test_col_classes(correct_outputs, "Location", protocol_version = "1.1.0")

  ## Should be errors. These fail because the number of columns are not the same.
  testthat::expect_failure(test_col_classes(incorrect_outputs, "Brood", protocol_version = "1.1.0"))
  testthat::expect_failure(test_col_classes(incorrect_outputs, "Capture", protocol_version = "1.1.0"))
  testthat::expect_failure(test_col_classes(incorrect_outputs, "Individual", protocol_version = "1.1.0"))
  testthat::expect_failure(test_col_classes(incorrect_outputs, "Location", protocol_version = "1.1.0"))

})


testthat::test_that("ID columns match the expected format for the pipeline", {

  # Create standard data with wrong ID formats
  incorrect_outputs <- correct_outputs
  incorrect_outputs$Brood_data[1, "FemaleID"] <- "2D53"
  incorrect_outputs$Brood_data[2, "MaleID"] <- "ZE745"
  incorrect_outputs$Individual_data[1, "IndvID"] <- "Z2-1421"
  incorrect_outputs$Capture_data[1, "IndvID"] <- "Z2-1421"

  ## FemaleID format is as expected
  test_ID_format(correct_outputs, column = "FemaleID", format = "^[:alpha:]{1}[:digit:]{2,4}$")

  ## MaleID format is as expected
  test_ID_format(correct_outputs, column = "MaleID", format = "^[:alpha:]{1}[:digit:]{2,4}$")

  ## IndvID format in Capture data is as expected
  test_ID_format(correct_outputs, column = "IndvID", table = "Capture", format = "^[:alpha:]{1}[:digit:]{0,1}[_]{0,1}[:digit:]{1,4}$")

  ## IndvID format in Individual data is as expected
  test_ID_format(correct_outputs, column = "IndvID", table = "Individual", format = "^[:alpha:]{1}[:digit:]{0,1}[_]{0,1}[:digit:]{1,4}$")

  ## Should be errors. These fail because the ID format is incorrect
  testthat::expect_failure(test_ID_format(incorrect_outputs, column = "MaleID", format = "^[:alpha:]{1}[:digit:]{2,4}$"))
  testthat::expect_failure(test_ID_format(incorrect_outputs, column = "FemaleID", format = "^[:alpha:]{1}[:digit:]{2,4}$"))
  testthat::expect_failure(test_ID_format(incorrect_outputs, column = "IndvID",
                                          table = "Capture", format = "^[:alpha:]{1}[:digit:]{0,1}[_]{0,1}[:digit:]{1,4}$"))
  testthat::expect_failure(test_ID_format(incorrect_outputs, column = "IndvID",
                                          table = "Individual", format = "^[:alpha:]{1}[:digit:]{0,1}[_]{0,1}[:digit:]{1,4}$"))

})

testthat::test_that("Key columns only contain unique values", {

  # Dummy data contain duplicated IDs
  duplicated_outputs <- correct_outputs

  # Remove duplicated IDs
  non_duplicated_outputs <- tibble::lst(Brood_data = correct_outputs$Brood_data[1:5, ],
                                        Capture_data = correct_outputs$Capture_data[1:5, ],
                                        Individual_data = correct_outputs$Individual_data[1:5, ],
                                        Location_data = correct_outputs$Location_data[1:5, ])

  ## BroodID has only unique values
  test_unique_values(non_duplicated_outputs, "BroodID")

  ## CaptureID has only unique values
  test_unique_values(non_duplicated_outputs, "CaptureID")

  ## PopID-IndvID has only unique values
  test_unique_values(non_duplicated_outputs, "IndvID")

  ## Should be errors. These fail because there are duplicate values.
  testthat::expect_failure(test_unique_values(duplicated_outputs, "BroodID"))
  testthat::expect_failure(test_unique_values(duplicated_outputs, "CaptureID"))
  testthat::expect_failure(test_unique_values(duplicated_outputs, "IndvID"))

})


testthat::test_that("Key columns in each table do not have NAs", {

  # Create standard data with NAs in columns that should not
  incorrect_outputs <- correct_outputs
  incorrect_outputs$Brood_data[1, "BreedingSeason"] <- NA_integer_
  incorrect_outputs$Individual_data[1, "RingSeason"] <- NA_integer_
  incorrect_outputs$Capture_data[1, "Species"] <- NA_character_
  incorrect_outputs$Location_data[1, "LocationType"] <- NA_character_

  ## Brood
  test_NA_columns(correct_outputs, "Brood")

  ## Capture
  test_NA_columns(correct_outputs, "Capture")

  ## Individual
  test_NA_columns(correct_outputs, "Individual")

  ## Location
  test_NA_columns(correct_outputs, "Location")

  ## Should be errors. These fail because there are NAs.
  testthat::expect_failure(test_NA_columns(incorrect_outputs, "Brood"))
  testthat::expect_failure(test_NA_columns(incorrect_outputs, "Capture"))
  testthat::expect_failure(test_NA_columns(incorrect_outputs, "Individual"))
  testthat::expect_failure(test_NA_columns(incorrect_outputs, "Location"))

})

testthat::test_that("Categorical columns do not have unexpected values", {

  # Dummy data contain unexpected categorical values
  incorrect_outputs <- correct_outputs

  # Only use expected categorical values
  correct_outputs$Brood_data <- correct_outputs$Brood_data %>%
    dplyr::mutate(PopID = dplyr::case_when(!is.na(.data$PopID) ~ "HOG",
                                           TRUE ~ .data$PopID))

  correct_outputs$Capture_data <- correct_outputs$Capture_data %>%
    dplyr::mutate(CapturePopID = dplyr::case_when(!is.na(.data$CapturePopID) ~ "HOG",
                                                  TRUE ~ .data$CapturePopID))

  correct_outputs$Individual_data <- correct_outputs$Individual_data %>%
    dplyr::mutate(PopID = dplyr::case_when(!is.na(.data$PopID) ~ "HOG",
                                           TRUE ~ .data$PopID))

  correct_outputs$Location_data <- correct_outputs$Location_data %>%
    dplyr::mutate(PopID = dplyr::case_when(!is.na(.data$PopID) ~ "HOG",
                                           TRUE ~ .data$PopID))

  ## Brood
  test_category_columns(correct_outputs, "Brood")

  ## Capture
  test_category_columns(correct_outputs, "Capture")

  ## Individual
  test_category_columns(correct_outputs, "Individual")

  ## Location
  test_category_columns(correct_outputs, "Location")

  ## Should be errors. These fail because there are unexpected categories.
  testthat::expect_failure(test_category_columns(incorrect_outputs, "Brood"))
  testthat::expect_failure(test_category_columns(incorrect_outputs, "Capture"))
  testthat::expect_failure(test_category_columns(incorrect_outputs, "Individual"))
  testthat::expect_failure(test_category_columns(incorrect_outputs, "Location"))


})
