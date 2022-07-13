testthat::skip_if(!exists("data_path"))

pipeline_output <- format_BAN(db = paste0(data_path, "/BAN_BandonValley_Ireland"),
                              optional_variables = "all")

test_that("BAN outputs all files...", {

  expect_true("BAN" %in% pipeline_output$Brood_data$siteID)
  expect_true("BAN" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("BAN" %in% pipeline_output$Individual_data$siteID)
  #expect_true("BAN" %in% pipeline_output$Measurement_data$siteID) # Measurement_data is empty
  expect_true("BAN" %in% pipeline_output$Location_data$siteID)
  #expect_true("BAN" %in% pipeline_output$Experiment_data$siteID) # Experiment_data is empty

})

test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex and each species for an adult and a chick
  # Currently, no chick data is available

  # Take a subset of only BAN data
  BAN_data <- dplyr::filter(pipeline_output$Individual_data, siteID == "BAN")

  # Test 1: Adult blue tit female
  # Individual A057192 should be listed as a female blue tit
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "A057192"))$calculatedSex, "F")
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "A057192"))$speciesID,
               species_codes[species_codes$speciesCode == 10002, ]$speciesID)
  # She should have no broodIDLaid or broodIDFledged because there is no chick info
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "A057192"))$broodIDLaid, NA_character_)
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "A057192"))$broodIDFledged, NA_character_)
  # Her ring year should be 2014 with a ringStage of 'adult'
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "A057192"))$ringYear, 2014)
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "A057192"))$ringStage, "adult")

  # Test 2: Adult great tit female
  # Individual D534573 should be listed as a female great tit
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$calculatedSex, "F")
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # She should have no broodIDLaid or broodIDFledged because there is no chick info
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$broodIDLaid, NA_character_)
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$broodIDFledged, NA_character_)
  # Her ring year should be 2011 with a ringStage of 'chick'
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$ringYear, 2013)
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$ringStage, "adult")

  # Test 3: Adult blue tit male
  # Individual D534700 should be listed as a male blue tit
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534700"))$calculatedSex, "M")
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534700"))$speciesID,
               species_codes[species_codes$speciesCode == 10002, ]$speciesID)
  # He should have no broodIDLaid or broodIDFledged because there is no chick info
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534700"))$broodIDLaid, NA_character_)
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534700"))$broodIDFledged, NA_character_)
  # His ring year should be 2014 with a ringStage of 'adult'
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534700"))$ringYear, 2014)
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534700"))$ringStage, "adult")

  # Test 4: Adult great tit male
  # Individual D534574 should be listed as a male great tit
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$calculatedSex, "M")
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # He should have no broodIDLaid or broodIDFledged because there is no chick info
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$broodIDLaid, NA_character_)
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$broodIDFledged, NA_character_)
  # His ring year should be 2011 with a ringStage of 'chick'
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$ringYear, 2013)
  expect_equal(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$ringStage, "adult")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of clutch type

  # Take a subset of only BAN data
  BAN_data <- dplyr::filter(pipeline_output$Brood_data, siteID == "BAN")

  # Test 1: Great tit brood clutch type = first
  # broodID 2017_BP_010_47 should be PARMAJ
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # broodID 2017_BP_010_47 should have clutch type observed & calculated 'first'
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$observedClutchType, "first")
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$calculatedClutchType, "first")
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$nestAttemptNumber, 1)
  # Laying date should be "2017-04-16"
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$observedLayYear, 2017)
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$observedLayMonth, 4)
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$observedLayDay, 16)
  # Clutch size should be 8, brood size should be NA, number fledged should be 4
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$observedClutchSize, 6)
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$observedBroodSize, NA_integer_)
  expect_equal(subset(BAN_data, broodID == "2017_BP_010_47")$observedNumberFledged, 4)

  # Test 2: Great tit brood clutch type = second
  # broodID 2017_BP_027_74 should be PARMAJ
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # broodID 2017_BP_027_74 should have clutch type observed & calculated 'second' (clutch tested above was successful)
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$observedClutchType, "second")
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$calculatedClutchType, "second")
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$nestAttemptNumber, 2)
  # Laying date should be "2017-05-13"
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$observedLayYear, 2017)
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$observedLayMonth, 5)
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$observedLayDay, 13)
  # Clutch size should be 7, brood size should be NA, number fledged should be 6
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$observedClutchSize, 7)
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$observedBroodSize, NA_integer_)
  expect_equal(subset(BAN_data, broodID == "2017_BP_027_74")$observedNumberFledged, 6)

  # Test 3: Great tit brood clutch type = replacement, where replacement is known (i.e. previous clutch was seen)
  # broodID 2015_BC_024_97 should be PARMAJ
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # broodID 2015_BC_024_97 should have clutch type observed 'first' & calculated 'replacement'
  # i.e., clutch 2015_BC_026_55 had no fledglings
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$observedClutchType, "first")
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$calculatedClutchType, "replacement")
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$nestAttemptNumber, 1)
  # Laying date should be "2015-06-05"
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$observedLayYear, 2015)
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$observedLayMonth, 6)
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$observedLayDay, 5)
  # Clutch size should be 5, brood size should be NA, number fledged should be 0
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$observedClutchSize, 5)
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$observedBroodSize, NA_integer_)
  expect_equal(subset(BAN_data, broodID == "2015_BC_024_97")$observedNumberFledged, 0)

  # Test 4: Great tit brood clutch type = replacement, where replacement calculated from cutoff
  # broodID 2014_KB_028_106 should be PARMAJ
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # broodID 2014_KB_028_106 should have clutch type observed 'first' & calculated 'replacement' (laying date is > cutoff)
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$observedClutchType, "first")
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$calculatedClutchType, "replacement")
  # Laying date should be "2014-06-14"
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$observedLayYear, 2014)
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$observedLayMonth, 6)
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$observedLayDay, 14)
  # Clutch size should be 4, brood size should be NA, number fledged should be 2
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$observedClutchSize, 4)
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$observedBroodSize, NA_integer_)
  expect_equal(subset(BAN_data, broodID == "2014_KB_028_106")$observedNumberFledged, 2)

})

test_that("Capture_data returns an expected outcome...", {

  # We want to run tests for captures as both chicks, males, and females
  # Currently we have no chick data, so we can only test adults

  # Take a subset of only BAN data
  BAN_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID == "BAN")

  # Test 1: Female caught as adult
  # Test the female has the correct number of capture records (6)
  expect_equal(nrow(subset(BAN_data, individualID == paste0("BAN_", "D534573"))), 6)
  #Test that the first capture of the female is as expected (2013-06-02)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$captureYear), 2013)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$captureDay), 2)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$releaseRingNumber), "D534573")
  #Test that the 5th capture of the female is as expcted (2017-05-12) (6th one is NA so no good to test)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$captureYear, 5), 2017)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$captureMonth, 5), 5)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$captureDay, 5), 12)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$captureRingNumber, 5), "D534573")
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$releaseRingNumber, 5), "D534573")
  # Test that exactAge is as expected (NA, because we just know it was caught as an adult)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$minimumAge), 1)
  #Test that minimumAge is correct on 5th capture (5, because it's an adult caught 4 years after its first capture)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534573"))$minimumAge, 5), 5)

  # Test 1: Male caught as adult
  # Test the male has the correct number of capture records (6)
  expect_equal(nrow(subset(BAN_data, individualID == paste0("BAN_", "D534574"))), 6)
  # Test that the first capture of the male is as expected (2013-06-02)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$captureYear), 2013)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$captureDay), 2)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$releaseRingNumber), "D534574")
  # Test that the 6th capture of the male is as expcted (2018-06-08)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$captureYear, 6), 2018)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$captureMonth, 6), 6)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$captureDay, 6), 8)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$captureRingNumber, 6), "D534574")
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$releaseRingNumber, 6), "D534574")
  # Test that exactAge is as expected (NA, because we just know it was caught as an adult, that's all)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$minimumAge), 1)
  # Test that minimumAge is correct on 6th capture (6, because it's an adult caught 5 years after its first capture)
  expect_equal(dplyr::nth(subset(BAN_data, individualID == paste0("BAN_", "D534574"))$minimumAge, 6), 6)

})

test_that("Location_data returns an expected outcome...", {

  # We want to run tests for nest boxes

  # Take a subset of only BAN data
  BAN_data <- dplyr::filter(pipeline_output$Location_data, siteID == "BAN")

  # Test 1: Nestbox check
  # Nestbox BP_001 should be type "nest", and put up in 2013
  expect_equal(subset(BAN_data, locationID == "BP_001")$locationType, "nest")
  expect_equal(subset(BAN_data, locationID == "BP_001")$startYear, 2013)

})

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
  test_ID_format(pipeline_output, ID_col = "femaleID", ID_format = "^BAN_[:alpha:]{1,3}[:digit:]{4,6}$")

  ## maleID format is as expected
  test_ID_format(pipeline_output, ID_col = "maleID", ID_format = "^BAN_[:alpha:]{1,3}[:digit:]{4,6}$")

  ## individualID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-individualID", ID_format = "^BAN_[:alpha:]{1,3}[:digit:]{4,6}$")

  ## individualID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-individualID", ID_format = "^BAN_[:alpha:]{1,3}[:digit:]{4,6}$")

})

test_that("Key columns only contain unique values", {

  ## broodID has only unique values
  test_unique_values(pipeline_output, "broodID")

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
  test_NA_columns(pipeline_output, "Capture") # TODO: check with data owner for captureMonth & captureDay

  ## Individual
  test_NA_columns(pipeline_output, "Individual") # TODO: check with data owner for ringMonth & ringDay

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

  ## Location
  test_category_columns(pipeline_output, "Location")

})
