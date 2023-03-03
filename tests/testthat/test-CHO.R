testthat::skip_if(!exists("data_path"))

# Run pipeline including all optional variables
pipeline_output <- format_CHO(db = paste0(data_path, "/CHO_Choupal_Portugal"),
                              optional_variables = "all")

test_that("CHO outputs all files...", {

  expect_true("CHO" %in% pipeline_output$Brood_data$siteID)
  expect_true("CHO" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("CHO" %in% pipeline_output$Individual_data$siteID)
  expect_true("CHO" %in% pipeline_output$Measurement_data$siteID)
  expect_true("CHO" %in% pipeline_output$Location_data$siteID)
  #expect_true("CHO" %in% pipeline_output$Experiment_data$siteID) # Experiment_data is empty

  expect_true("CHO-1" %in% pipeline_output$Brood_data$studyID)
  expect_true("CHO-1" %in% pipeline_output$Capture_data$studyID)
  expect_true("CHO-1" %in% pipeline_output$Individual_data$studyID)
  expect_true("CHO-1" %in% pipeline_output$Measurement_data$studyID)
  expect_true("CHO-1" %in% pipeline_output$Location_data$studyID)

})

test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex for adults and chicks

  # Take a subset of CHO - Individual_data
  CHO_data <- dplyr::filter(pipeline_output$Individual_data, studyID == "CHO-1")

  # Test 1: Adult great tit female
  # Individual C029842 should be listed as a female great tit
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029842"))$calculatedSex, "F")
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029842"))$speciesID, "PARMAJ")
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029842"))$broodIDLaid, NA_character_)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029842"))$broodIDFledged, NA_character_)
  # Her tag year should be 2003 with a tagStage of 'adult'
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029842"))$tagYear, 2003)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029842"))$tagStage, "adult")

  # Test 2: Adult great tit male
  # Individual C029848 should be listed as a male great tit
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029848"))$calculatedSex, "M")
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029848"))$speciesID, "PARMAJ")
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029848"))$broodIDLaid, NA_character_)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029848"))$broodIDFledged, NA_character_)
  # His tag year should be 2003 with a tagStage of 'adult'
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029848"))$tagYear, 2003)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029848"))$tagStage, "adult")

  # Test 3: Subadult great tit female
  # Individual C044309 should be listed as a female great tit
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$calculatedSex, "F")
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$speciesID, "PARMAJ")
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$broodIDLaid, NA_character_)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$broodIDFledged, NA_character_)
  # Her tag year should be 2003 with a tagStage of 'subadult' (first year)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$tagYear, 2003)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$tagStage, "subadult")

  # Test 4: Subadult great tit male
  # Individual C029843 should be listed as a male great tit
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$calculatedSex, "M")
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$speciesID, "PARMAJ")
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$broodIDLaid, NA_character_)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$broodIDFledged, NA_character_)
  # His tag year should be 2003 with a tagStage of 'subadult'
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$tagYear, 2003)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$tagStage, "subadult")

  # Test 5: Caught as chick
  # Individual C048092 should be listed as a female great tit
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$calculatedSex, "F")
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$speciesID, "PARMAJ")
  # Should have broodIDLaid and broodIDFledged of 2005_81
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$broodIDLaid, "2005_081")
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$broodIDFledged, "2005_081")
  # Ring year should be 2005 with a ringStage of 'chick'
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$tagYear, 2005)
  expect_equal(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$tagStage, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of CHO - Brood_data
  CHO_data <- dplyr::filter(pipeline_output$Brood_data, studyID == "CHO-1")

  #Test 1: Brood clutch type = first
  #broodID 2005_021 should be PARMAJ
  expect_equal(subset(CHO_data, broodID == "2005_021")$speciesID, "PARMAJ")
  #broodID 2005_021 should have clutch type observed & calculated 'first'
  expect_equal(subset(CHO_data, broodID == "2005_021")$observedClutchType, "first")
  expect_equal(subset(CHO_data, broodID == "2005_021")$calculatedClutchType, "first")
  expect_equal(subset(CHO_data, broodID == "2005_021")$nestAttemptNumber, 1)
  #Laying date should be "2005-04-06"
  expect_equal(subset(CHO_data, broodID == "2005_021")$observedLayYear, 2005)
  expect_equal(subset(CHO_data, broodID == "2005_021")$observedLayMonth, 4)
  expect_equal(subset(CHO_data, broodID == "2005_021")$observedLayDay, 6)
  #Clutch size should be 6, BroodSize should be 6, NumberFledged should be 6
  expect_equal(subset(CHO_data, broodID == "2005_021")$observedClutchSize, 6)
  expect_equal(subset(CHO_data, broodID == "2005_021")$observedBroodSize, 6)
  expect_equal(subset(CHO_data, broodID == "2005_021")$observedNumberFledged, 6)

  #Test 2: Brood clutch type = second
  #broodID 2005_006 should be PARMAJ
  expect_equal(subset(CHO_data, broodID == "2005_006")$speciesID, "PARMAJ")
  #broodID 2005_006 should have clutch type observed 'second' (clutch tested above was successful)
  expect_equal(subset(CHO_data, broodID == "2005_006")$observedClutchType, "second")
  expect_equal(subset(CHO_data, broodID == "2005_006")$calculatedClutchType, "second")
  expect_equal(subset(CHO_data, broodID == "2005_006")$nestAttemptNumber, 2)
  #Laying date should be 2005-05-17
  expect_equal(subset(CHO_data, broodID == "2005_006")$observedLayYear, 2005)
  expect_equal(subset(CHO_data, broodID == "2005_006")$observedLayMonth, 5)
  expect_equal(subset(CHO_data, broodID == "2005_006")$observedLayDay, 17)
  #Clutch size should be 4, BroodSize should be 2, NumberFledged should be 2
  expect_equal(subset(CHO_data, broodID == "2005_006")$observedClutchSize, 4)
  expect_equal(subset(CHO_data, broodID == "2005_006")$observedBroodSize, 2)
  expect_equal(subset(CHO_data, broodID == "2005_006")$observedNumberFledged, 2)

  #Test 3: Great tit brood clutch type = replacement, where replacement is known (i.e. previous clutch was seen)
  #broodID 2003_004 should be PARMAJ
  expect_equal(subset(CHO_data, broodID == "2003_004")$speciesID, "PARMAJ")
  #broodID 2005_004 should have clutch type calculated 'replacement' (clutch 2015_BC_026_24_04 had no fledlings)
  expect_equal(subset(CHO_data, broodID == "2003_004")$calculatedClutchType, "replacement")
  #Laying date should be "2003-06-05"
  expect_equal(subset(CHO_data, broodID == "2003_004")$observedLayYear, 2003)
  expect_equal(subset(CHO_data, broodID == "2003_004")$observedLayMonth, 6)
  expect_equal(subset(CHO_data, broodID == "2003_004")$observedLayDay, 5)
  #Clutch size should be 4, BroodSize should be 2, NumberFledged should be 2
  expect_equal(subset(CHO_data, broodID == "2003_004")$observedClutchSize, 4)
  expect_equal(subset(CHO_data, broodID == "2003_004")$observedBroodSize, 2)
  expect_equal(subset(CHO_data, broodID == "2003_004")$observedNumberFledged, 2)

  #Test 4: Brood clutch type = replacement, where replacement calculated from cutoff
  #broodID 2003_012 should be PARMAJ
  expect_equal(subset(CHO_data, broodID == "2003_012")$speciesID, "PARMAJ")
  #broodID 2005_012 should have clutch type calculated 'replacement' (laying date is > cutoff)
  expect_equal(subset(CHO_data, broodID == "2003_012")$calculatedClutchType, "replacement")
  #Laying date should be "2003-06-01"
  expect_equal(subset(CHO_data, broodID == "2003_012")$observedLayYear, 2003)
  expect_equal(subset(CHO_data, broodID == "2003_012")$observedLayMonth, 6)
  expect_equal(subset(CHO_data, broodID == "2003_012")$observedLayDay, 1)
  #Clutch size should be 5, BroodSize should be 4, NumberFledged should be 4
  expect_equal(subset(CHO_data, broodID == "2003_012")$observedClutchSize, 5)
  expect_equal(subset(CHO_data, broodID == "2003_012")$observedBroodSize, 4)
  expect_equal(subset(CHO_data, broodID == "2003_012")$observedNumberFledged, 4)

})

test_that("Capture_data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females

  #Take a subset of CHO - Capture_data
  CHO_data <- dplyr::filter(pipeline_output$Capture_data, studyID == "CHO-1")

  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (5)
  expect_equal(nrow(subset(CHO_data, individualID == paste0("CHO_", "C044309"))), 5)
  #Test that the first capture of the female is as expected (2003-05-21)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$captureYear), 2003)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$captureDay), 21)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$releaseTagID), "C044309")
  #Test that the 5th capture of the female is as expected (2006-06-24)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$captureYear, 5), 2006)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$captureMonth, 5), 6)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$captureDay, 5), 24)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$captureTagID, 5), "C044309")
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$releaseTagID, 5), "C044309")
  #Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$exactAge), NA_integer_)
  #Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$minimumAge), 1)
  #Test that minimumAge is correct on 5th capture (4, because it's an adult caught 3 years after its first capture)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C044309"))$minimumAge, 5), 4)

  #Test 2: Male caught as adult
  #Test the male has the correct ID of capture records (2)
  expect_equal(nrow(subset(CHO_data, individualID == paste0("CHO_", "C029843"))), 2)
  #Test that the first capture of the male is as expected (2013-06-02)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$captureYear), 2003)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$captureMonth), 4)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$captureDay), 9)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$releaseTagID), "C029843")
  #Test that the 2nd capture of the male is as expcted (2003-10-17)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$captureYear, 2), 2003)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$captureMonth, 2), 10)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$captureDay, 2), 17)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$captureTagID, 2), "C029843")
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$releaseTagID, 2), "C029843")
  #Test that exactAge is as expected on first record (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$exactAge), NA_integer_)
  #Test that minimumAge is correct on first capture (1, because it's an adult not caught as chick)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$minimumAge), 1)
  #Test that minimumAge is correct on second capture (1, because it's the same year)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C029843"))$minimumAge, 2), 1)

  #Test 3: Caught first as chick
  #Test the male has the correct ID of capture records (4)
  expect_equal(nrow(subset(CHO_data, individualID == paste0("CHO_", "C048092"))), 4)
  #Test that the first capture of the male is as expected (2005-06-03)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$captureYear), 2005)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$captureDay), 3)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$releaseTagID), "C048092")
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$chickAge), 14)
  #Test that the 4th capture of the male is as expected (2006-07-01)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$captureYear, 4), 2006)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$captureMonth, 4), 7)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$captureDay, 4), 1)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$captureTagID, 4), "C048092")
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$releaseTagID, 4), "C048092")
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$chickAge, 4), NA_integer_)
  #Test that exactAge is as expected on first record (0, because it's caught as chick)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$exactAge), 0)
  #Test that exactAge is as expected on last record (1, because it's as 'first year' i.e. first year of breeding)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$exactAge, 4), 1)
  #Test that minimumAge is correct on first capture (0, because it's a chick)
  expect_equal(dplyr::first(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$minimumAge), 0)
  #Test that age calculated is correct on second capture (0 because it was caught as chick in this year)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$minimumAge, 2), 0)
  #Test that age calculated is correct on 4th capture (1, because it was caught as chick in the previous year)
  expect_equal(dplyr::nth(subset(CHO_data, individualID == paste0("CHO_", "C048092"))$minimumAge, 4), 1)

})

test_that("Measurement_data returns an expected outcome...", {

  # We want to run tests for individuals that had all all measurements taken, and individuals with some measurements missing

  # Take a subset of CHO - Measurement_data
  CHO_data <- dplyr::filter(pipeline_output$Measurement_data, studyID == "CHO-1")

  # Test 1: all three measurements
  # Test that individual C044404 had been taken three measurements of in its first capture
  expect_equal(nrow(subset(CHO_data, recordID == paste0("CHO_", "C044404", "_1"))), 3)
  # Test that its measurements are as expected (mass: 16.5, wing length: 72, tarsus: 19.3)
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C044404", "_1") & measurementType == "mass")$measurementValue, 16.5)
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C044404", "_1") & measurementType == "wing length")$measurementValue, 72)
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C044404", "_1") & measurementType == "tarsus")$measurementValue, 19.3)
  # Test that the measurements were taken on the correct date (2004-10-23)
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C044404", "_1"))$measurementDeterminedYear, rep(2004, 3))
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C044404", "_1"))$measurementDeterminedMonth, rep(10, 3))
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C044404", "_1"))$measurementDeterminedDay, rep(23, 3))

  # Test2: missing measurements
  # Test that individual C048001 had been taken two measurements of in its first capture
  expect_equal(nrow(subset(CHO_data, recordID == paste0("CHO_", "C048001", "_1"))), 2)
  # Test that its measurements are as expected (mass: 17, tarsus: 19.4)
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C048001", "_1") & measurementType == "mass")$measurementValue, 17)
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C048001", "_1") & measurementType == "tarsus")$measurementValue, 19.4)
  # Test that there is no wing length measurement
  expect_equal(nrow(subset(CHO_data, recordID == paste0("CHO_", "C048001", "_1") & measurementType == "wing length")), 0)
  # Test that the measurements were taken on the correct date (2004-10-23)
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C048001", "_1"))$measurementDeterminedYear, rep(2005, 2))
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C048001", "_1"))$measurementDeterminedMonth, rep(5, 2))
  expect_equal(subset(CHO_data, recordID == paste0("CHO_", "C048001", "_1"))$measurementDeterminedDay, rep(7, 2))

})

test_that("Location_data returns an expected outcome...", {

  # We want to run tests for both nesting and capturing (i.e. mist net) locations

  # Take a subset of CHO - Location_data
  CHO_data <- dplyr::filter(pipeline_output$Location_data, studyID == "CHO-1")

  # Test 1: Nest box check
  # Nestbox 7 should be type "nest"
  expect_equal(subset(CHO_data, locationID == "007")$locationType, "nest")

  # Test 2: Mist net check
  # There should be a capture (i.e., mist net) location called "MN1" (no exact mist net data provided)
  expect_equal(subset(CHO_data, locationID == "MN1")$locationType, "capture")

})


## General tests

test_that("Expected columns are present", {

  ## Will fail if not all the expected columns are present

  ## Brood data: Test that all columns are present
  test_col_present(pipeline_output, "Brood")

  ## Capture data: Test that all columns are present
  test_col_present(pipeline_output, "Capture")

  ## Individual data: Test that all columns are present
  test_col_present(pipeline_output, "Individual")

  ## Measurement data: Test that all columns are present
  test_col_present(pipeline_output, "Measurement")

  ## Location data: Test that all columns are present
  test_col_present(pipeline_output, "Location")

  ## Experiment data: Test that all columns are present
  test_col_present(pipeline_output, "Experiment")

})

test_that("Column classes are as expected", {

  ## Will fail if columns that are shared by the output and the templates have different classes.

  ## Brood data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Brood")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual")

  ## Measurement data: Test that all column classes are expected
  test_col_present(pipeline_output, "Measurement")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location")

  ## Experiment data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Experiment")

})

test_that("ID columns match the expected format for the pipeline", {

  ## femaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "femaleID", ID_format = "^CHO_C[:digit:]{6}$")

  ## maleID format is as expected
  test_ID_format(pipeline_output, ID_col = "maleID", ID_format = "^CHO_C[:digit:]{6}$")

  ## individualID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-individualID", ID_format = "^CHO_C[:digit:]{6}$")

  ## individualID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-individualID", ID_format = "^CHO_C[:digit:]{6}$")

})

test_that("Key columns only contain unique values", {

  ## broodID has only unique values
  test_unique_values(pipeline_output, "broodID") ## TODO: one broodID is duplicated, contact data owner

  ## captureID has only unique values
  test_unique_values(pipeline_output, "captureID")

  ## individualID has only unique values
  test_unique_values(pipeline_output, "individualID")

  ## measurementID has only unique values
  test_unique_values(pipeline_output, "measurementID")

  ## locationID has only unique values
  test_unique_values(pipeline_output, "locationID")

})

test_that("Key columns in each table do not have NAs", {

  ## Brood
  test_NA_columns(pipeline_output, "Brood")

  ## Capture
  test_NA_columns(pipeline_output, "Capture")

  ## Individual
  test_NA_columns(pipeline_output, "Individual")

  ## Measurement
  test_NA_columns(pipeline_output, "Measurement")

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

  ## Measurement
  test_category_columns(pipeline_output, "Measurement")

  ## Location
  test_category_columns(pipeline_output, "Location")

})
