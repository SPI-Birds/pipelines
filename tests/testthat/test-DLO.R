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
  # Her tag year should be 2005 with a tagStage of 'adult'
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "S304118"))$tagYear, 2005)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "S304118"))$tagStage, "adult")

  # Test 2: Subadult collared flycatcher male
  # Individual F70825 should be listed as a male collared flycatcher
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$calculatedSex, "M")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$speciesID, "FICALB")
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$broodIDLaid, NA_character_)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$broodIDFledged, NA_character_)
  # Her tag year should be 2005 with a tagStage of 'subadult'
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$tagYear, 2005)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "F70825"))$tagStage, "subadult")

  # Test 3: Caught as chick (unknown sex)
  # Individual N512178 should be listed as a great tit chick, with unknown sex
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$calculatedSex, NA_character_)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$speciesID, "PARMAJ")
  # broodIDLaid & broodIDFledged should be 2005_5_16_krmelec_108
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$broodIDLaid, "2005_5_16_krmelec_108")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$broodIDFledged, "2005_5_16_krmelec_108")
  # Their tag year should be 2005 with a tagStage of 'chick'
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$tagYear, 2005)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "N512178"))$tagStage, "chick")

  # Test 4: First caught as chick (but known sex because caught as breeder in subsequent years)
  # Individual J41882 is a collared flycatcher female
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$calculatedSex, "F")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$speciesID, "FICALB")
  # broodIDLaid & broodIDFledged should be 2017_5_6_olse_170
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$broodIDLaid, "2017_5_6_olse_170")
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$broodIDFledged, "2017_5_6_olse_170")
  # Their tag year should be 2017 with a tagStage of 'chick'
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$tagYear, 2017)
  expect_equal(subset(DLO_data, individualID == paste0("DLO_", "J41882"))$tagStage, "chick")

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
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$releaseTagID), "F119613")
  # Test that the 5th capture of the female is as expected (2014-05-27)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureYear, 5), 2014)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureMonth, 5), 5)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureDay, 5), 27)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$captureTagID, 5), "F119613")
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$releaseTagID, 5), "F119613")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$exactAge), NA_integer_)
  # FIXME Age calculation: NA because capture dates are incomplete
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  #expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$minimumAge), 1)
  # Test that minimumAge is correct on 5th capture (4, because it's an adult caught 3 years after its first capture)
  #expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119613"))$minimumAge, 5), 4)

  # Test 2: Male caught as adult
  # Test the male has the correct number of capture records (3)
  expect_equal(nrow(subset(DLO_data, individualID == paste0("DLO_", "F119732"))), 3)
  # Test that the 1st capture of the male is as expected (2014-05-30)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$captureYear), 2014)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$captureDay), 30)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$releaseTagID), "F119732")
  # Test that the 3rd capture of the male is as expected (2017-06-01)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$captureYear, 3), 2017)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$captureMonth, 3), 6)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$captureDay, 3), 1)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$captureTagID, 3), "F119732")
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$releaseTagID, 3), "F119732")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$minimumAge), 1)
  # Test that minimumAge is correct on 3rd capture (4, because it's an adult caught 3 years after its first capture)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "F119732"))$minimumAge, 3), 4)

  # Test 3: Caught as chick
  # Test the chick has the correct number of capture records (4)
  expect_equal(nrow(subset(DLO_data, individualID == paste0("DLO_", "S695547"))), 4)
  # Test that the 1st capture of the chick is as expected (2015)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$captureYear), 2015)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$captureMonth), NA_integer_)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$captureDay), NA_integer_)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$releaseTagID), "S695547")
  # Test that the 4th capture of the chick is as expected (2019-05-30)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$captureYear, 4), 2019)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$captureMonth, 4), 5)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$captureDay, 4), 30)
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$captureTagID, 4), "S695547")
  expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$releaseTagID, 4), "S695547")
  # FIXME Age calculation: NA because capture dates are incomplete
  # Test that exactAge is correct on first capture (0, because it's caught as a chick)
  #expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$exactAge), 0)
  # Test that exactAge is correct on 4th capture (0, because it's caught as a chick 4 years later)
  #expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$exactAge, 4), 4)
  # Test that minimumAge is as expected
  #expect_equal(dplyr::first(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$minimumAge), 0)
  #expect_equal(dplyr::nth(subset(DLO_data, individualID == paste0("DLO_", "S695547"))$minimumAge, 4), 4)

})

test_that("Measurement_data returns an expected outcome...", {

  # We want to run tests for individuals that had all all measurements taken, and individuals with some measurements missing

  # Take a subset of DLO - Measurement_data
  DLO_data <- dplyr::filter(pipeline_output$Measurement_data, siteID == "DLO")

  # Test 1: all three measurements
  # Test that individual TB63242 had been taken three measurements of in its first capture
  expect_equal(nrow(subset(DLO_data, recordID == paste0("DLO_", "TB63242", "_1"))), 3)
  # Test that its measurements are as expected (mass: 16.5, wing length: 72, tarsus: 19.3)
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "TB63242", "_1") & measurementType == "mass")$measurementValue, 18.25)
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "TB63242", "_1") & measurementType == "wing length")$measurementValue, 74)
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "TB63242", "_1") & measurementType == "tarsus")$measurementValue, 22.78)
  # Test that the measurements were taken in the correct year (2005)
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "TB63242", "_1"))$measurementDeterminedYear, rep(2005, 3))

  # Test 2: missing measurements
  # Test that individual F119732 had been taken two measurements of in its second capture
  expect_equal(nrow(subset(DLO_data, recordID == paste0("DLO_", "F119732", "_2"))), 2)
  # Test that its measurements are as expected (mass: 16.5, wing length: 72)
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "F119732", "_2") & measurementType == "mass")$measurementValue, 13.3)
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "F119732", "_2") & measurementType == "wing length")$measurementValue, 86)
  # Test that there is no tarsus measurement
  expect_equal(nrow(subset(DLO_data, recordID == paste0("DLO_", "F119732", "_2") & measurementType == "tarsus")), 0)
  # Test that the measurements were taken on the correct date (2016-06-02)
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "F119732", "_2"))$measurementDeterminedYear, rep(2016, 2))
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "F119732", "_2"))$measurementDeterminedMonth, rep(6, 2))
  expect_equal(subset(DLO_data, recordID == paste0("DLO_", "F119732", "_2"))$measurementDeterminedDay, rep(2, 2))


})

test_that("Location_data returns an expected outcome...", {

  # We want to run tests for nesting locations

  # Take a subset of DLO - Location_data
  DLO_data <- dplyr::filter(pipeline_output$Location_data, siteID == "DLO")

  # Test 1: Nest box check
  # Nestbox 14 in polesi should be type "nest", and put up in 2005
  expect_equal(subset(DLO_data, locationID == "polesi_14")$locationType, "nest")
  expect_equal(subset(DLO_data, locationID == "polesi_14")$startYear, 2005L)

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
  test_ID_format(pipeline_output, ID_col = "femaleID", ID_format = "^DLO_[:upper:]{1,2}[:digit:]{4,6}$")

  ## maleID format is as expected
  test_ID_format(pipeline_output, ID_col = "maleID", ID_format = "^DLO_[:upper:]{1,2}[:digit:]{4,6}$")

  ## individualID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-individualID", ID_format = "^DLO_[:upper:]{1,2}[:digit:]{4,8}$")

  ## individualID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-individualID", ID_format = "^DLO_[:upper:]{1,2}[:digit:]{4,8}$")

})

test_that("Key columns only contain unique values", {

  ## broodID has only unique values
  test_unique_values(pipeline_output, "broodID") # TODO: 2 broodIDs are duplicated, contact data owner

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
