testthat::skip_if(!exists("data_path"))

pipeline_output <- format_PIL(db = paste0(data_path, "/PIL_PilisVisegradMountains_Hungary"))

test_that("PIL outputs all files...", {

  expect_true(all("PIL" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("PIL" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("PIL" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("PIL" %in% pipeline_output$Location_data$PopID))
  expect_true(pipeline_output$protocol_version == "1.1.0")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only PIL data
  PIL_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "PIL")

  #Test 1: Brood where clutch type = first
  expect_equal(subset(PIL_data, BroodID == "2010_16_25_13")$Species, "PARMAJ")
  expect_equal(subset(PIL_data, BroodID == "2010_16_25_13")$ClutchType_calculated, "first")
  expect_equal(subset(PIL_data, BroodID == "2010_16_25_13")$LayDate_observed, as.Date("2010-04-13"))
  expect_equal(subset(PIL_data, BroodID == "2010_16_25_13")$ClutchSize_observed, 10L)
  expect_equal(subset(PIL_data, BroodID == "2010_16_25_13")$BroodSize_observed, 10L)
  expect_equal(subset(PIL_data, BroodID == "2010_16_25_13")$NumberFledged_observed, 10L)
  expect_equal(round(subset(PIL_data, BroodID == "2010_16_25_13")$AvgChickMass, 2), 18.27)
  expect_equal(round(subset(PIL_data, BroodID == "2010_16_25_13")$AvgTarsus, 2), 20.23)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(PIL_data, BroodID == "2000_7_90_40")$Species, "FICALB")
  expect_equal(subset(PIL_data, BroodID == "2000_7_90_40")$ClutchType_calculated, "replacement")
  expect_equal(subset(PIL_data, BroodID == "2000_7_90_40")$LayDate_observed, as.Date("2000-05-10"))
  expect_equal(subset(PIL_data, BroodID == "2000_7_90_40")$ClutchSize_observed, 5L)
  expect_equal(subset(PIL_data, BroodID == "2000_7_90_40")$BroodSize_observed, 5L)
  expect_equal(subset(PIL_data, BroodID == "2000_7_90_40")$NumberFledged_observed, 3L)
  #Measurements taken but not included in average because chick age was >16
  expect_equal(subset(PIL_data, BroodID == "2000_7_90_40")$AvgChickMass, NA_real_)
  expect_equal(subset(PIL_data, BroodID == "2000_7_90_40")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(PIL_data, BroodID == "2006_6_51_58")$Species, "PARMAJ")
  expect_equal(subset(PIL_data, BroodID == "2006_6_51_58")$ClutchType_calculated, "replacement")
  expect_equal(subset(PIL_data, BroodID == "2006_6_51_58")$LayDate_observed, as.Date("2006-05-28"))
  expect_equal(subset(PIL_data, BroodID == "2006_6_51_58")$ClutchSize_observed, 10L)
  expect_equal(subset(PIL_data, BroodID == "2006_6_51_58")$BroodSize_observed, NA_integer_)
  expect_equal(subset(PIL_data, BroodID == "2006_6_51_58")$NumberFledged_observed, 5L)
  expect_equal(subset(PIL_data, BroodID == "2006_6_51_58")$AvgChickMass, NA_real_)
  expect_equal(subset(PIL_data, BroodID == "2006_6_51_58")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(PIL_data, BroodID == "2017_2_243_45")$Species, "PARMAJ")
  expect_equal(subset(PIL_data, BroodID == "2017_2_243_45")$ClutchType_calculated, "second")
  expect_equal(subset(PIL_data, BroodID == "2017_2_243_45")$LayDate_observed, as.Date("2017-05-15"))
  expect_equal(subset(PIL_data, BroodID == "2017_2_243_45")$ClutchSize_observed, 7L)
  expect_equal(subset(PIL_data, BroodID == "2017_2_243_45")$BroodSize_observed, 7L)
  expect_equal(subset(PIL_data, BroodID == "2017_2_243_45")$NumberFledged_observed, 7L)
  expect_equal(subset(PIL_data, BroodID == "2017_2_243_45")$AvgChickMass, NA_real_)
  expect_equal(subset(PIL_data, BroodID == "2017_2_243_45")$AvgTarsus, NA_real_)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only PIL data
  PIL_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "PIL")

  #Test 1: First caught as adult
  expect_equal(subset(PIL_data, IndvID == "3E3832")$Sex_calculated, "F")
  expect_equal(subset(PIL_data, IndvID == "3E3832")$Species, "FICALB")
  expect_equal(subset(PIL_data, IndvID == "3E3832")$BroodIDLaid, NA_character_)
  expect_equal(subset(PIL_data, IndvID == "3E3832")$BroodIDFledged, NA_character_)
  expect_equal(subset(PIL_data, IndvID == "3E3832")$RingSeason, 2001L)
  expect_equal(subset(PIL_data, IndvID == "3E3832")$RingAge, "adult")

  #Test 2: Caught first as chick
  expect_equal(subset(PIL_data, IndvID == "1E6750")$Sex_calculated, "F")
  expect_equal(subset(PIL_data, IndvID == "1E6750")$Species, "FICALB")
  expect_equal(subset(PIL_data, IndvID == "1E6750")$BroodIDLaid, "1999_7_406_52")
  expect_equal(subset(PIL_data, IndvID == "1E6750")$BroodIDFledged, "1999_7_406_52")
  expect_equal(subset(PIL_data, IndvID == "1E6750")$RingSeason, 1999L)
  expect_equal(subset(PIL_data, IndvID == "1E6750")$RingAge, "chick")

})

test_that("Capture data returns an expected outcome...", {

  #Take a subset of only PIL data
  PIL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "PIL")

  #Test 1: Individual ringed as a chick
  expect_equal(nrow(subset(PIL_data, IndvID == "1E6750")), 6)
  expect_equal(subset(PIL_data, IndvID == "1E6750")$CaptureDate[1], as.Date("1999-06-15"))
  expect_equal(subset(PIL_data, IndvID == "1E6750")$CaptureDate[6], as.Date("2004-05-29"))
  expect_equal(subset(PIL_data, IndvID == "1E6750")$Age_observed[1], 1L)
  expect_equal(subset(PIL_data, IndvID == "1E6750")$Age_observed[6], NA_integer_)
  expect_equal(subset(PIL_data, IndvID == "1E6750")$Age_calculated[1], 1L)
  expect_equal(subset(PIL_data, IndvID == "1E6750")$Age_calculated[6], 13L)

  #Test 2: Individual caught only as adult
  expect_equal(nrow(subset(PIL_data, IndvID == "3E3832")), 9)
  expect_equal(subset(PIL_data, IndvID == "3E3832")$CaptureDate[1], as.Date("2001-04-25"))
  expect_equal(subset(PIL_data, IndvID == "3E3832")$CaptureDate[9], as.Date("2007-05-26"))
  expect_equal(subset(PIL_data, IndvID == "3E3832")$Age_observed[1], NA_integer_)
  expect_equal(subset(PIL_data, IndvID == "3E3832")$Age_observed[9], NA_integer_)
  expect_equal(subset(PIL_data, IndvID == "3E3832")$Age_calculated[1], 4L)
  expect_equal(subset(PIL_data, IndvID == "3E3832")$Age_calculated[9], 16L)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  PIL_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "PIL")

  #Test 1: Nestbox check
  expect_true(subset(PIL_data, LocationID == "7_49")$LocationType == "NB")
  expect_true(subset(PIL_data, LocationID == "7_49")$NestboxID == "7_49")
  expect_equal(subset(PIL_data, LocationID == "7_49")$StartSeason, 1982L)
  expect_equal(subset(PIL_data, LocationID == "7_49")$EndSeason, NA_integer_)
  expect_equal(subset(PIL_data, LocationID == "7_49")$PopID, "PIL")
  expect_equal(subset(PIL_data, LocationID == "7_49")$Latitude %>% setNames(nm = NULL), NA_real_)
  expect_equal(subset(PIL_data, LocationID == "7_49")$Longitude %>% setNames(nm = NULL), NA_real_)

})

## General tests

test_that("Expected columns are present", {

  ## Will fail if not all the expected columns are present

  ## Brood data: Test that all columns are present
  test_col_present(pipeline_output, "Brood", pipeline_output$protocol_version)

  ## Capture data: Test that all columns are present
  test_col_present(pipeline_output, "Capture", pipeline_output$protocol_version)

  ## Individual data: Test that all columns are present
  test_col_present(pipeline_output, "Individual", pipeline_output$protocol_version)

  ## Location data: Test that all columns are present
  test_col_present(pipeline_output, "Location", pipeline_output$protocol_version)

})

test_that("Column classes are as expected", {

  ## Will fail if columns that are shared by the output and the templates have different classes.

  ## Brood data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Brood", pipeline_output$protocol_version)

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture", pipeline_output$protocol_version)

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual", pipeline_output$protocol_version)

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location", pipeline_output$protocol_version)

})

test_that("ID columns match the expected format for the pipeline", {

  ## FemaleID format is as expected
  test_ID_format(pipeline_output, column = "FemaleID", format = "^[:alnum:]{2}[:digit:]{3,5}[:alpha:]{0,1}$")

  ## MaleID format is as expected
  test_ID_format(pipeline_output, column = "MaleID", format = "^[:alnum:]{2}[:digit:]{3,5}[:alpha:]{0,1}$")

  ## IndvID format in Capture data  is as expected
  test_ID_format(pipeline_output, column = "IndvID", table = "Capture", format = "^[:alnum:]{2}[:digit:]{3,5}[:alpha:]{0,1}$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, column = "IndvID", table = "Individual", format = "^[:alnum:]{2}[:digit:]{3,5}[:alpha:]{0,1}$")

})


test_that("Key columns only contain unique values", {

  ## BroodID has only unique values
  test_unique_values(pipeline_output, "BroodID")

  ## CaptureID has only unique values
  test_unique_values(pipeline_output, "CaptureID")

  ## PopID-IndvID has only unique values
  test_unique_values(pipeline_output, "IndvID")

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

})
