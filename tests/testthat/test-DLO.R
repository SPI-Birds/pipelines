testthat::skip_if(!exists("data_path"))

# Run pipeline including all optional variables
pipeline_output <- format_DLO(db = paste0(data_path, "/DLO_DlouhaLoucka_Czechia"),
                              optional_variables = "all")

test_that("DLO outputs all files...", {

  expect_true("DLO" %in% pipeline_output$Brood_data$siteID)
  expect_true("DLO" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("DLO" %in% pipeline_output$Individual_data$siteID)
  expect_true("DLO" %in% pipeline_output$Measurement_data$siteID)
  expect_true("DLO" %in% pipeline_output$Location_data$siteID)
  #expect_true("DLO" %in% pipeline_output$Experiment_data$siteID) # Experiment_data is empty

})

test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex for (sub-)adults and chicks

  # Take a subset of DLO - Individual_data
  DLO_data <- dplyr::filter(pipeline_output$Individual_data, siteID == "DLO")

  # Test 1: Adult collared flycatcher female
  # Individual S304118 should be listed as a female collared flycatcher
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "S304118"))$calculatedSex, "F")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "S304118"))$speciesID, "FICALB")
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "S304118"))$broodIDLaid, NA_character_)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "S304118"))$broodIDFledged, NA_character_)
  # Her ring year should be 2005 with a ringStage of 'adult'
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "S304118"))$ringYear, 2005)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "S304118"))$ringStage, "adult")

  # Test 2: Subadult collared flycatcher male
  # Individual F70825 should be listed as a male collared flycatcher
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$calculatedSex, "M")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$speciesID, "FICALB")
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$broodIDLaid, NA_character_)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$broodIDFledged, NA_character_)
  # Her ring year should be 2005 with a ringStage of 'subadult'
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$ringYear, 2005)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$ringStage, "subadult")

  # Test 3: Caught as chick (unknown sex)
  # Individual N512178 should be listed as a great tit chick, with unknown sex
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$calculatedSex, NA_character_)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$speciesID, "PARMAJ")
  # broodIDLaid & broodIDFledged should be 2005_5_16_krmelec_108
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$broodIDLaid, "2005_5_16_krmelec_108")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$broodIDFledged, "2005_5_16_krmelec_108")
  # Their ring year should be 2005 with a ringStage of 'chick'
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$ringYear, 2005)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$ringStage, "chick")

  # Test 4: First caught as chick (but known sex because caught as breeder in subsequent years)
  # Individual J41882 is a collared flycatcher female
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$calculatedSex, "F")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$speciesID, "FICALB")
  # broodIDLaid & broodIDFledged should be 2017_5_6_olse_170
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$broodIDLaid, "2017_5_6_olse_170")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$broodIDFledged, "2017_5_6_olse_170")
  # Their ring year should be 2017 with a ringStage of 'chick'
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$ringYear, 2017)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$ringStage, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  # We want to run tests for all possible outcomes of clutch type

  # Take a subset of DLO - Brood_data
  DLO_data <- dplyr::filter(pipeline_output$Brood_data, siteID == "DLO")

  # Test 1: Brood clutch type = first
  # broodID 2005_4_15_domecky_9 should be SITEUR
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$speciesID, "SITEUR")
  # broodID 2005_4_15_domecky_9 should have clutch type observed & calculated 'first'
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$observedClutchType, "first")
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$calculatedClutchType, "first")
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$nestAttemptNumber, 1)
  # Laying date should be "2005-04-15"
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$observedLayYear, 2005)
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$observedLayMonth, 4)
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$observedLayDay, 15)
  # Clutch size should be 8, brood size should be 7, fledgling number should be 0 (predated)
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$observedClutchSize, 8)
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$observedBroodSize, 7)
  expect_equal(subset(DLO_data, broodID == "2005_4_15_domecky_9")$observedNumberFledged, 0)

  # Test 2: Brood clutch type = second
  # broodID 2005_5_30_krmelec_125 should be PARMAJ
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$speciesID, "PARMAJ")
  # broodID 2005_5_30_krmelec_125 should have clutch type observed & calculated 'second'
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$observedClutchType, "second")
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$calculatedClutchType, "second")
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$nestAttemptNumber, 1) # Nest attempt = 1, because male is unknown
  # Laying date should be "2005-05-30"
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$observedLayYear, 2005)
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$observedLayMonth, 5)
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$observedLayDay, 30)
  # Clutch size should be 8, brood size should be 7, fledgling number should be 6
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$observedClutchSize, 8)
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$observedBroodSize, 7)
  expect_equal(subset(DLO_data, broodID == "2005_5_30_krmelec_125")$observedNumberFledged, 6)

  # Test 3: Brood clutch type = replacement, where replacement is known (i.e. previous clutch was observed/recorded)
  # broodID 2007_5_14_krmelec_57 should be FICALB
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$speciesID, "FICALB")
  # broodID 2005_5_30_krmelec_125 should have clutch type observed & calculated 'second'
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$observedClutchType, "replacement")
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$calculatedClutchType, "replacement")
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$nestAttemptNumber, 1) # Nest attempt = 1, because male is unknown
  # Laying date should be "2007-05-14"
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$observedLayYear, 2007)
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$observedLayMonth, 5)
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$observedLayDay, 14)
  # Clutch size and brood size should be 5, fledgling number should be 0 (predated)
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$observedClutchSize, 5)
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$observedBroodSize, 5)
  expect_equal(subset(DLO_data, broodID == "2007_5_14_krmelec_57")$observedNumberFledged, 0)

  # Test 4: Brood clutch type = replacement, where replacement calculated from 30-day cutoff
  # broodID 2015_6_8_sutrak_16 should be PARMAJ
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$speciesID, "PARMAJ")
  # broodID 2015_6_8_sutrak_16 should have clutch type observed 'second' & calculated 'replacement'
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$observedClutchType, "second")
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$calculatedClutchType, "replacement")
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$nestAttemptNumber, 1) # Nest attempt = 1, because female is unknown
  # Laying date should be "2015-06-08"
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$observedLayYear, 2015)
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$observedLayMonth, 6)
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$observedLayDay, 8)
  # Clutch size, brood size, and fledgling number should be 8
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$observedClutchSize, 8)
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$observedBroodSize, 8)
  expect_equal(subset(DLO_data, broodID == "2015_6_8_sutrak_16")$observedNumberFledged, 8)

})

test_that("Capture_data returns an expected outcome...", {

  # We want to run tests for captures as both chicks, males, and females

  # Take a subset of DLO - Capture_data
  DLO_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID == "DLO")

  # Test 1: Female caught as adult
  # Test the female has the correct number of capture records (5)
  expect_equal(nrow(subset(DLO_data, individualID == paste0("DLO_", "F119613"))), 5)
  # Test that the 1st capture of the female is as expected (2011)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureYear), 2011)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureMonth), NA_integer_)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureDay), NA_integer_)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$releaseRingNumber), "F119613")
  # Test that the 5th capture of the female is as expected (2014-05-27)
  expect_equal(dplyr::last(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureYear), 2014)
  expect_equal(dplyr::last(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureMonth), 5)
  expect_equal(dplyr::last(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureDay), 27)
  expect_equal(dplyr::last(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureRingNumber), "F119613")
  expect_equal(dplyr::last(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$releaseRingNumber), "F119613")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$minimumAge), 1)
  # Test that minimumAge is correct on 5th capture (4, because it's an adult caught 3 years after its first capture)
  expect_equal(dplyr::last(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$minimumAge), 4)

})
