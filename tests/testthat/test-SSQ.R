testthat::skip_if(!exists("data_path"))

pipeline_output <- format_SSQ(db = paste0(data_path, "/SSQ_SantoStefanoQuisquina_Italy"),
                              optional_variables = "all")

test_that("SSQ outputs all files...", {

  expect_true("SSQ" %in% pipeline_output$Brood_data$siteID)
  expect_true("SSQ" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("SSQ" %in% pipeline_output$Individual_data$siteID)
  #expect_true("SSQ" %in% pipeline_output$Measurement_data$siteID) # Measurement_data is empty
  expect_true("SSQ" %in% pipeline_output$Location_data$siteID)
  #expect_true("SSQ" %in% pipeline_output$Experiment_data$siteID) # Experiment_data is empty

})

test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex and each species for an adult and a chick
  # Note: all individuals are listed as female (or sex unknown)

  # Take a subset of only SSQ data
  SSQ_data <- dplyr::filter(pipeline_output$Individual_data, siteID == "SSQ")

  # Test 1: Adult blue tit
  # Individual AS29440 should be listed as a female blue tit
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "AS29440"))$calculatedSex, "F")
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "AS29440"))$speciesID,
               species_codes[species_codes$speciesCode == 10002, ]$speciesID)
  # She should have no broodIDLaid or broodIDFledged
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "AS29440"))$broodIDLaid, NA_character_)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "AS29440"))$broodIDFledged, NA_character_)
  # Her ring season should be 2006 with a ringStage of 'adult'
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "AS29440"))$ringYear, 2006)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "AS29440"))$ringStage, "adult")

  # Test 2: Adult great tit
  # Individual LS30008 should be listed as a female great tit
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LS30008"))$calculatedSex, "F")
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LS30008"))$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # She should have a broodIDLaid and broodIDFledged of 2011_030_039
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LS30008"))$broodIDLaid, "2011_030_039")
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LS30008"))$broodIDFledged, "2011_030_039")
  # Her ring season should be 2011 with a ringStage of 'chick'
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LS30008"))$ringYear, 2011)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LS30008"))$ringStage, "chick")

  # Test 3: Chick great tit
  # Individual LA44309 should be listed as a great tit with unknown sex (NA)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LA44309"))$calculatedSex, NA_character_)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LA44309"))$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # Individual should have a broodIDLaid and broodIDFledged of 2006_002_051
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LA44309"))$broodIDLaid, "2006_002_051")
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LA44309"))$broodIDFledged, "2006_002_051")
  # Their ring season should be 2006 with a ringStage of 'chick'
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LA44309"))$ringYear, 2006)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "LA44309"))$ringStage, "chick")

  # Test 4: Chick blue tit
  # Individual 6A33818 should be listed as a blue tit with unknown sex (NA)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "6A33818"))$calculatedSex, NA_character_)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "6A33818"))$speciesID,
               species_codes[species_codes$speciesCode == 10002, ]$speciesID)
  # Individual should have a broodIDLaid and broodIDFledged of 2011_026_043
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "6A33818"))$broodIDLaid, "2011_026_043")
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "6A33818"))$broodIDFledged, "2011_026_043")
  # Their ring season should be 2011 with a ringStage of 'chick'
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "6A33818"))$ringYear, 2011)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "6A33818"))$ringStage, "chick")

  # Test 5: Individual with conflicted species
  # Individual 25A9699 should be listed as a female with conflicting species
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$calculatedSex, "F")
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$speciesID, "CCCCCC")
  # She should have no broodIDLaid or broodIDFledged
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$broodIDLaid, NA_character_)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$broodIDFledged, NA_character_)
  # Her ring season should be 2016 with a ringStage of 'adult'
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$ringYear, 2016)
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$ringStage, "adult")

})

test_that("Brood_data returns an expected outcome...", {

  # We want to run tests for great tit and blue tit broods
  # We want to manually check all possible outcomes of clutch type

  # Take a subset of only SSQ data
  SSQ_data <- dplyr::filter(pipeline_output$Brood_data, siteID == "SSQ")

  #Test 1: Blue tit brood clutch type = first
  # broodID 2010_005_046 should be CYACAE
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$speciesID,
               species_codes[species_codes$speciesCode == 10002, ]$speciesID)
  # broodID 2010_005_046 should have clutch type observed & calculated 'first'
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$observedClutchType, "first")
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$calculatedClutchType, "first")
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$nestAttemptNumber, 1)
  # Laying date should be "2010-04-15"
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$observedLayYear, 2010)
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$observedLayMonth, 4)
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$observedLayDay, 15)
  # Clutch size should be 8, brood size should be 0, number fledged should be 0
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$observedClutchSize, 8)
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$observedBroodSize, 0)
  expect_equal(subset(SSQ_data, broodID == "2010_005_046")$observedNumberFledged, 0)

  # Test 2: Blue tit brood clutch type = replacement
  # broodID 2010_006_061 should be CYACAE
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$speciesID,
               species_codes[species_codes$speciesCode == 10002, ]$speciesID)
  # broodID 2010_006_061 should have clutch type observed & calculated 'replacement'
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$observedClutchType, "replacement")
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$calculatedClutchType, "replacement")
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$nestAttemptNumber, 1)
  # Laying date should be "2010-04-30"
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$observedLayYear, 2010)
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$observedLayMonth, 4)
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$observedLayDay, 30)
  # Clutch size should be 8, brood size should be 7, number fledged should be 5
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$observedClutchSize, 8)
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$observedBroodSize, 7)
  expect_equal(subset(SSQ_data, broodID == "2010_006_061")$observedNumberFledged, 5)

  # Test 3: Great tit brood clutch type = first
  # broodID 2012_00X_044 should be PARMAJ
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # broodID 2010_005_046 should have clutch type observed & calculated 'first'
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$observedClutchType, "first")
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$calculatedClutchType, "first")
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$nestAttemptNumber, 1)
  # Laying date should be "2012-04-13"
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$observedLayYear, 2012)
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$observedLayMonth, 4)
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$observedLayDay, 13)
  # Clutch size should be 8, brood size should be 0, number fledged should be 0
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$observedClutchSize, 8)
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$observedBroodSize, 0)
  expect_equal(subset(SSQ_data, broodID == "2012_00X_044")$observedNumberFledged, 0)

  # Test 4: Great tit brood clutch type = replacement
  # broodID 2012_004_059 should be PARMAJ
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$speciesID,
               species_codes[species_codes$speciesCode == 10001, ]$speciesID)
  # broodID 2012_004_059 should have clutch type calculated 'replacement'
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$observedClutchType, "replacement")
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$calculatedClutchType, "replacement")
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$nestAttemptNumber, 1)
  # Laying date should be "2012-04-28"
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$observedLayYear, 2012)
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$observedLayMonth, 4)
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$observedLayDay, 28)
  # Clutch size should be 8, brood size should be 8, number fledged should be 8
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$observedClutchSize, 8)
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$observedBroodSize, 8)
  expect_equal(subset(SSQ_data, broodID == "2012_004_059")$observedNumberFledged, 8)

})

test_that("Capture_data returns an expected outcome...", {

  # We want to run tests for great tit and blue tit capture as both chicks and adults

  # Take a subset of only SSQ data
  SSQ_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID == "SSQ")

  # Test 1: Great tit caught as a chick
  # Test that the chick has the correct number of capture records (2)
  expect_equal(nrow(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))), 2)
  # Test that the 1st capture of the individual is as expected (2016-05-18, laying date + clutch size + 27)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$captureYear), 2016)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$captureDay), 18)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$releaseRingNumber), "LS05355")
  # Test that the 2nd capture of the individual is as expected (2015-05-07)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$captureYear, 2), 2017)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$captureMonth, 2), 5)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$captureDay, 2), 7)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$captureRingNumber, 2), "LS05355")
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$releaseRingNumber, 2), "LS05355")
  # Test that exactAge is correct on first capture (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$exactAge), 0)
  # Test that exactAge is correct on 2nd capture (1, because it's caught as a chick 1 year later)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$exactAge, 2), 1)
  # Test that minimumAge is as expected
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$minimumAge), 0)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "LS05355"))$minimumAge, 2), 1)

  # Test 2: Great tit caught as a adult
  # Test that the female has the correct number of capture records (3)
  expect_equal(nrow(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))), 3)
  # Test that the 1st capture of the individual is as expected (2009-05-09, laying date + clutch size)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$captureYear), 2009)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$captureDay), 9)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$releaseRingNumber), "AX42202")
  # Test that the 3rd capture of the individual is as expected (2011-05-05, laying date + clutch size)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$captureYear, 3), 2011)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$captureMonth, 3), 5)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$captureDay, 3), 5)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$captureRingNumber, 3), "AX42202")
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$releaseRingNumber, 3), "AX42202")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$minimumAge), 1)
  # Test that minimumAge is correct on 3rd capture (3, because it's an adult caught 2 years after its first capture)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AX42202"))$minimumAge, 3), 3)

  # Test 3: Blue tit caught as a chick
  # Test that the chick has the correct number of capture records (4)
  expect_equal(nrow(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))), 4)
  # Test that the 1st capture of the individual is as expected (2011-05-20, laying date + clutch size + 27)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$captureYear), 2011)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$captureDay), 20)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$releaseRingNumber), "6A33877")
  # Test that the 4th capture of the individual is as expected (2015-04-30, laying date + clutch size)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$captureYear, 4), 2015)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$captureMonth, 4), 4)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$captureDay, 4), 30)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$captureRingNumber, 4), "6A33877")
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$releaseRingNumber, 4), "6A33877")
  # Test that exactAge is correct on first capture (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$exactAge), 0)
  # Test that exactAge is correct on 4th capture (1, because it's caught as a chick 4 year later)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$exactAge, 4), 4)
  # Test that minimumAge is as expected
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$minimumAge), 0)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "6A33877"))$minimumAge, 4), 4)

  # Test 4: Blue tit caught as a adult
  # Test the individual has the correct number of capture records (4)
  expect_equal(nrow(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))), 4)
  # Test that the 1st capture of the individual is as expected (2009-04-26, laying date + clutch size)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$captureYear), 2009)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$captureMonth), 4)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$captureDay), 26)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$releaseRingNumber), "AT36437")
  # Test that the 4th capture of the individual is as expected (2012-04-21, laying date + clutch size)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$captureYear, 4), 2012)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$captureMonth, 4), 4)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$captureDay, 4), 21)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$captureRingNumber, 4), "AT36437")
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$releaseRingNumber, 4), "AT36437")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$minimumAge), 1)
  # Test that minimumAge is correct on 4th capture (4, because it's an adult caught 3 years after its first capture)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "AT36437"))$minimumAge, 4), 4)

  # Test 5: Individual classified as different species on 2 captures
  # Test the individual has the correct number of capture records (4)
  expect_equal(nrow(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))), 2)
  # Test that the 1st capture of the individual is as expected (2016-04-27, laying date + clutch size)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$captureYear), 2016)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$captureMonth), 4)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$captureDay), 27)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$releaseRingNumber), "25A9699")
  expect_equal(dplyr::first(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$releaseRingNumber), "25A9699")
  # Test that the 2nd capture of the individual is as expected (2017-04-25, laying date + clutch size)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$captureYear, 2), 2017)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$captureMonth, 2), 4)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$captureDay, 2), 25)
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$captureRingNumber, 2), "25A9699")
  expect_equal(dplyr::nth(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$releaseRingNumber, 2), "25A9699")
  # Test that the species of the 1st capture is PARMAJ, and of the 2nd capture CYACAE
  expect_equal(subset(SSQ_data, individualID == paste0("SSQ_", "25A9699"))$speciesID, c("PARMAJ", "CYACAE"))

})

test_that("Location_data returns an expected outcome...", {

  # We want to run tests for nest boxes

  # Take a subset of only SSQ data
  SSQ_data <- dplyr::filter(pipeline_output$Location_data, siteID == "SSQ")

  # Test 1: Nestbox check
  # Nest box "011" should be type "nest" and put up in 2006
  expect_equal(subset(SSQ_data, locationID == "011")$locationType, "nest")
  expect_equal(subset(SSQ_data, locationID == "011")$startYear, 2006)
  # Latitude and longitude should be 13.5458 and 37.6058
  expect_equal(subset(SSQ_data, locationID == "011")$decimalLatitude, 13.5458)
  expect_equal(subset(SSQ_data, locationID == "011")$decimalLongitude, 37.6058)

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
  test_ID_format(pipeline_output, ID_col = "femaleID", ID_format = "^SSQ_[:alnum:]{7}$")

  ## maleID format is as expected
  test_ID_format(pipeline_output, ID_col = "maleID", ID_format = "^SSQ_[:alnum:]{7}$")

  ## individualID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-individualID", ID_format = "^SSQ_[:alnum:]{7}$")

  ## individualID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-individualID", ID_format = "^SSQ_[:alnum:]{7}$")

})

test_that("Key columns only contain unique values", {

  ## broodID has only unique values
  test_unique_values(pipeline_output, "broodID") # TODO: Check with data owner: 5 duplicated broodIDs

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
