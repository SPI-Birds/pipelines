testthat::skip_if(!exists("data_path"))

# Run pipeline including all optional variables
pipeline_output <- format_GLI(db = paste0(data_path, "/GLI_Glimmen_Netherlands"),
                              optional_variables = "all")

test_that("GLI outputs all files...", {

  expect_true("GLI" %in% pipeline_output$Brood_data$siteID)
  expect_true("GLI" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("GLI" %in% pipeline_output$Individual_data$siteID)
  #expect_true("GLI" %in% pipeline_output$Measurement_data$siteID) # Measurement_data is empty
  #expect_true("GLI" %in% pipeline_output$Location_data$siteID) # Location_data is empty
  #expect_true("GLI" %in% pipeline_output$Experiment_data$siteID) # Experiment_data is empty

})

test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex for adults

  # Take a subset of GLI - Individual_data
  GLI_data <- dplyr::filter(pipeline_output$Individual_data, siteID == "GLI")

  # Test 1: Adult female
  # Individual 12 should be listed as a female Eurasian jackdaw
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "12"))$calculatedSex, "F")
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "12"))$speciesID, "CORMON")
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "12"))$broodIDLaid, NA_character_)
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "12"))$broodIDFledged, NA_character_)
  # Her tag year should be 2000 with a tagStage of 'adult'
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "12"))$tagYear, 2000)
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "12"))$tagStage, "adult")

  # Test 2: Adult male
  # Individual 103 should be listed as a male Eurasian jackdaw
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "103"))$calculatedSex, "M")
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "103"))$speciesID, "CORMON")
  # Individual should have no broodIDLaid or broodIDFledged because caught as an adult
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "103"))$broodIDLaid, NA_character_)
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "103"))$broodIDFledged, NA_character_)
  # Tag year should be 1997 with a tagStage of 'adult'
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "103"))$tagYear, 1997)
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "103"))$tagStage, "adult")

  # Test 3: Adult (conflicted sex)
  # Individual 1058 should be listed as a Eurasian jackdaw of conflicted sex
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "1058"))$calculatedSex, "C")
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "1058"))$speciesID, "CORMON")
  # Individual should have no broodIDLaid or broodIDFledged because caught as an adult
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "1058"))$broodIDLaid, NA_character_)
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "1058"))$broodIDFledged, NA_character_)
  # Tag year should be 2010 with a tagStage of 'adult'
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "1058"))$tagYear, 2010)
  expect_equal(subset(GLI_data, individualID == paste0("GLI_", "1058"))$tagStage, "adult")

})

test_that("Brood data returns an expected outcome...", {

  # We want to run tests for all possible types of broods

  # Take a subset of GLI - Brood_data
  GLI_data <- dplyr::filter(pipeline_output$Brood_data, siteID == "GLI")

  # Test 1: all brood parameters recorded
  # broodID 10 should be CORMON
  expect_equal(subset(GLI_data, broodID == "10")$speciesID, "CORMON")
  # broodID 10 should have clutch type observed & calculated NA
  expect_equal(subset(GLI_data, broodID == "10")$observedClutchType, NA_character_)
  expect_equal(subset(GLI_data, broodID == "10")$calculatedClutchType, NA_character_)
  expect_equal(subset(GLI_data, broodID == "10")$nestAttemptNumber, 1)
  # Laying date should be "1996-04-17"
  expect_equal(subset(GLI_data, broodID == "10")$observedLayYear, 1996)
  expect_equal(subset(GLI_data, broodID == "10")$observedLayMonth, 4)
  expect_equal(subset(GLI_data, broodID == "10")$observedLayDay, 17)
  # Hatching date should be "1996-05-06"
  expect_equal(subset(GLI_data, broodID == "10")$observedHatchYear, 1996)
  expect_equal(subset(GLI_data, broodID == "10")$observedHatchMonth, 5)
  expect_equal(subset(GLI_data, broodID == "10")$observedHatchDay, 6)
  # Clutch size should be 5, brood size should be NA, fledgling number should be NA
  expect_equal(subset(GLI_data, broodID == "10")$observedClutchSize, 5)
  expect_equal(subset(GLI_data, broodID == "10")$observedBroodSize, NA_integer_)
  expect_equal(subset(GLI_data, broodID == "10")$observedNumberFledged, NA_integer_)

  # Test 2: no laying date, but clutch size and hatch date
  # broodID 248 should be CORMON
  expect_equal(subset(GLI_data, broodID == "248")$speciesID, "CORMON")
  # broodID 10 should have clutch type observed & calculated NA
  expect_equal(subset(GLI_data, broodID == "248")$observedClutchType, NA_character_)
  expect_equal(subset(GLI_data, broodID == "248")$calculatedClutchType, NA_character_)
  #expect_equal(subset(GLI_data, broodID == "248")$nestAttemptNumber, 2)
  # Laying date should be '2003-NA-NA'
  expect_equal(subset(GLI_data, broodID == "248")$observedLayYear, 2003)
  expect_equal(subset(GLI_data, broodID == "248")$observedLayMonth, NA_integer_)
  expect_equal(subset(GLI_data, broodID == "248")$observedLayDay, NA_integer_)
  # Hatching date should be "2003-05-19"
  expect_equal(subset(GLI_data, broodID == "248")$observedHatchYear, 2003)
  expect_equal(subset(GLI_data, broodID == "248")$observedHatchMonth, 5)
  expect_equal(subset(GLI_data, broodID == "248")$observedHatchDay, 19)
  # Clutch size should be 2, brood size should be NA, fledgling number should be NA
  expect_equal(subset(GLI_data, broodID == "248")$observedClutchSize, 2)
  expect_equal(subset(GLI_data, broodID == "248")$observedBroodSize, NA_integer_)
  expect_equal(subset(GLI_data, broodID == "248")$observedNumberFledged, NA_integer_)

})

test_that("Capture data returns an expected outcome...", {

  # We want to run tests for captures as both males and females

  # Take a subset of GLI - Capture_data
  GLI_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID == "GLI")

  # Test 1: Female caught as adult
  # Test the female has the correct number of capture records (9)
  expect_equal(nrow(subset(GLI_data, individualID == paste0("GLI_", "297"))), 9)
  # Test that the 1st capture of the female is as expected (2002)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "297"))$captureYear), 2002)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "297"))$captureMonth), NA_integer_)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "297"))$captureDay), NA_integer_)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "297"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "297"))$releaseTagID), NA_character_)
  # Test that the 9th capture of the female is as expected (2010)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "297"))$captureYear, 9), 2010)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "297"))$captureMonth, 9), NA_integer_)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "297"))$captureDay, 9), NA_integer_)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "297"))$captureTagID, 9), NA_character_)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "297"))$releaseTagID, 9), NA_character_)
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "297"))$exactAge), NA_integer_)
  # Test that minimumAge is as expected
  #expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "297"))$minimumAge), NA_integer_)

  # Test 2: Male caught as adult
  # Test the male has the correct number of capture records (4)
  expect_equal(nrow(subset(GLI_data, individualID == paste0("GLI_", "588"))), 4)
  # Test that the 1st capture of the male is as expected (2007)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "588"))$captureYear), 2007)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "588"))$captureMonth), NA_integer_)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "588"))$captureDay), NA_integer_)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "588"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "588"))$releaseTagID), NA_character_)
  # Test that the 4th capture of the male is as expected (2011)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "588"))$captureYear, 4), 2011)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "588"))$captureMonth, 4), NA_integer_)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "588"))$captureDay, 4), NA_integer_)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "588"))$captureTagID, 4), NA_character_)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "588"))$releaseTagID, 4), NA_character_)
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "588"))$exactAge), NA_integer_)
  # Test that minimumAge is as expected
  #expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "588"))$minimumAge), NA_integer_)

  # Test 3: Adults caught as female and as male
  # Test that the individual has the correct number of capture records (6)
  expect_equal(nrow(subset(GLI_data, individualID == paste0("GLI_", "1058"))), 6)
  # Test that the 1st capture is as male in 2010
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "1058"))$captureYear), 2010)
  expect_equal(dplyr::first(subset(GLI_data, individualID == paste0("GLI_", "1058"))$observedSex), "M")
  # Test that the 6th capture is as female in 2017
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "1058"))$captureYear, 6), 2017)
  expect_equal(dplyr::nth(subset(GLI_data, individualID == paste0("GLI_", "1058"))$observedSex, 6), "F")

})

# No Location tests
# No Measurement tests
# No Experiment tests
