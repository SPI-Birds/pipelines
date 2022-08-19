testthat::skip_if(!exists("data_path"))

pipeline_output <- format_HAR(db = paste0(data_path, "/HAR_Harjavalta_Finland"),
                              optional_variables = "all")

test_that("HAR outputs all files...", {

  expect_true("HAR" %in% pipeline_output$Brood_data$siteID)
  expect_true("HAR" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("HAR" %in% pipeline_output$Individual_data$siteID)
  expect_true("HAR" %in% pipeline_output$Measurement_data$siteID)
  expect_true("HAR" %in% pipeline_output$Location_data$siteID)
  expect_true("HAR" %in% pipeline_output$Experiment_data$siteID)

})

test_that("Brood_data returns an expected outcome...", {

  # We want to run tests for all possible outcomes of ClutchType_calculated

  # Take a subset of only HAR data
  HAR_data <- dplyr::filter(pipeline_output$Brood_data, siteID == "HAR")

  # Test 1: Great tit brood, clutch type = first
  # broodID 2018_0101_1 should be PARMAJ
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == 10001])
  # broodID 2018_0101_1 should have clutch type observed & calculated 'first'
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$observedClutchType, "first")
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$calculatedClutchType, "first")
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$nestAttemptNumber, 1)
  # Laying date should be "2018-05-08"
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$observedLayYear, 2018)
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$observedLayMonth, 5)
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$observedLayDay, 8)
  # Clutch size should be 8, brood size and number fledged 4
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$observedClutchSize, 8)
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$observedBroodSize, 4)
  expect_equal(subset(HAR_data, broodID == "2018_0101_1")$observedNumberFledged, 4)

  # Test 2: Pied flycatcher brood, clutch type = replacement (because first is known to have failed)
  # broodID 1991_0842_1 should be FICHYP
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == 10003])
  # broodID 1991_0842_1 should have clutch type observed & calculated 'replacement'
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$observedClutchType, "replacement")
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$calculatedClutchType, "replacement")
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$nestAttemptNumber, 1)
  # Laying date should be "1991-06-10"
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$observedLayYear, 1991)
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$observedLayMonth, 6)
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$observedLayDay, 10)
  # Clutch size should be 7, brood size 7, and number fledged 5
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$observedClutchSize, 7)
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$observedBroodSize, 7)
  expect_equal(subset(HAR_data, broodID == "1991_0842_1")$observedNumberFledged, 5)

  # Test 3: Great tit brood, clutch type = replacement (past the 30-day cutoff)
  # broodID 2018_0159_1 should be PARMAJ
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == 10001])
  # broodID 2018_0159_1 should have clutch type observed 'second' & calculated 'replacement'
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$observedClutchType, "second")
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$calculatedClutchType, "replacement")
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$nestAttemptNumber, 1)
  # Laying date should be "2018-06-17"
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$observedLayYear, 2018)
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$observedLayMonth, 6)
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$observedLayDay, 17)
  # Clutch size should be 8, brood size 8, and number fledged 4
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$observedClutchSize, 8)
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$observedBroodSize, 8)
  expect_equal(subset(HAR_data, broodID == "2018_0159_1")$observedNumberFledged, 4)

  # Test 4: Blue tit brood, clutch type = second
  # broodID 1991_0113_1 should be CYACAE
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == 10002])
  # broodID 2018_0159_1 should have clutch type observed & calculated 'second'
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$observedClutchType, "second")
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$calculatedClutchType, "second")
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$nestAttemptNumber, 1)
  # Laying date should be "1991-06-17"
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$observedLayYear, 1991)
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$observedLayMonth, 6)
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$observedLayDay, 17)
  # Clutch size should be 10, brood size 10, and number fledged 5
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$observedClutchSize, 10)
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$observedBroodSize, 10)
  expect_equal(subset(HAR_data, broodID == "1991_0113_1")$observedNumberFledged, 5)

})

test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex for individuals caught as adults and chicks

  # Take a subset of only HAR data
  HAR_data <- dplyr::filter(pipeline_output$Individual_data, siteID == "HAR")

  # Test 1: Female never caught as chick
  # Individual V675839 should be listed as a great tit female
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$calculatedSex, "F")
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == 10001])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$broodIDLaid, NA_character_)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$broodIDFledged, NA_character_)
  # Her ring year should be 1991 with a ringStage of 'adult'
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$ringYear, 1991L)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$ringStage, "adult")

  # Test 2: Male never caught as chick
  # Individual HA32954 should be listed as a blue tit male
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HA32954"))$calculatedSex, "M")
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HA32954"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == 10002])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HA32954"))$broodIDLaid, NA_character_)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HA32954"))$broodIDFledged, NA_character_)
  # His ring year should be 1991 with a ringStage of 'adult'
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HA32954"))$ringYear, 2019)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HA32954"))$ringStage, "adult")

  # Test 3: First caught as chick (single record)
  # Individual HL010189 should be listed as a female pied flycatcher
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$calculatedSex, "F")
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == 10003])
  # She should have a broodIDLaid and broodIDFledged because this individual was caught as a chick
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$broodIDLaid, "2005_1617_1")
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$broodIDFledged, "2005_1617_1")
  # ringYear should be 2005 with a ringStage of 'chick'
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$ringYear, 2005L)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$ringStage, "chick")

  # Test 4: First caught as chick (multi-chick record)
  # Individual X127548 should be listed as a blue tit with unknown sex
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "X127548"))$calculatedSex, NA_character_)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "X127548"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == 10002])
  # Individual should have a broodIDLaid and broodIDFledged because they were caught as a chick
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "X127548"))$broodIDLaid, "1991_0102_1")
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "X127548"))$broodIDFledged, "1991_0102_1")
  # ringYear should be 1991 with a ringStage of 'chick'
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "X127548"))$ringYear, 1991L)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "X127548"))$ringStage, "chick")

})

test_that("Capture data returns an expected outcome...", {

  # Take a subset of only HAR data
  HAR_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID == "HAR")

  # Build tests for possible capture-nestling capture combos described in the help docs

  # Test 1: Capture records only as adult
  # Test for the expected number of captures (12)
  expect_equal(nrow(subset(HAR_data, individualID == paste0("HAR_", "X341048"))), 12)
  # Test that the 1st capture of the adult is as expected (1993-06-16)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$captureYear), 1993)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$captureDay), 16)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$releaseRingNumber), "X341048")
  # Test that the 12th capture of the adult is as expected (1996-06-28)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$captureYear, 12), 1996)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$captureMonth, 12), 6)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$captureDay, 12), 28)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$captureRingNumber, 12), "X341048")
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$releaseRingNumber, 12), "X341048")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on the 1st capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$minimumAge), 1)
  # Test that minimumAge is correct on the 12th capture (4, because it's an adult caught 3 years after its first capture)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X341048"))$minimumAge, 12), 4)

  # Test 2: Capture records as single chick
  # Test for the expected number of captures (5)
  expect_equal(nrow(subset(HAR_data, individualID == paste0("HAR_", "V871926"))), 5)
  # Test that the 1st capture of the chick is as expected (1990-07-04)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$captureYear), 1990)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$captureMonth), 7)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$captureDay), 4)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$releaseRingNumber), "V871926")
  # Test that the 5th capture of the adult is as expected (1992-07-02)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$captureYear, 5), 1992)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$captureMonth, 5), 7)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$captureDay, 5), 2)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$captureRingNumber, 5), "V871926")
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$releaseRingNumber, 5), "V871926")
  # Test that exactAge (and minimumAge) on the 1st capture is as expected (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$exactAge), 0)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$minimumAge), 0)
  # Test that exactAge (and minimumAge) on the 5th capture is as expected (2, because it's a chick caught 2 years after birth)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$exactAge, 5), 2)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "V871926"))$minimumAge, 5), 2)

  # Test 3: Chick captures with separate info in capture and nestling tables
  # Test for the expected number of captures (2)
  expect_equal(nrow(subset(HAR_data, individualID == paste0("HAR_", "X620030"))), 2)
  # Test that the 1st capture of the chick is as expected (1995-06-29)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$captureYear), 1995)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$captureDay), 29)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$releaseRingNumber), "X620030")
  # Test that the 2nd capture of the chick is as expected (1995-07-25)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$captureYear, 2), 1995)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$captureMonth, 2), 7)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$captureDay, 2), 25)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$captureRingNumber, 2), "X620030")
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$releaseRingNumber, 2), "X620030")
  # Test that exactAge (and minimumAge) on the 1st capture is as expected (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$exactAge), 0)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$minimumAge), 0)
  # Test that exactAge (and minimumAge) on the 2nd capture is as expected (0, because it's a chick caught in the year of birth)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$exactAge, 2), 0)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X620030"))$minimumAge, 2), 0)

  # Test 4: Individual in a multi-chick capture
  # Test for the expected number of captures (11)
  expect_equal(nrow(subset(HAR_data, individualID == paste0("HAR_", "X126587"))), 11)
  # Test that the 1st capture of the chick is as expected (1991-06-25)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$captureYear), 1991)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$captureDay), 25)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$releaseRingNumber), "X126587")
  # Test that the 11th capture of the chick is as expected (1996-06-26)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$captureYear, 11), 1996)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$captureMonth, 11), 6)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$captureDay, 11), 26)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$captureRingNumber, 11), "X126587")
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$releaseRingNumber, 11), "X126587")
  # Test that exactAge (and minimumAge) on the 1st capture is as expected (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$exactAge), 0)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$minimumAge), 0)
  # Test that exactAge (and minimumAge) on the 11th capture is as expected (5, because it's a chick caught 5 years after birth)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$exactAge, 11), 5)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X126587"))$minimumAge, 11), 5)

  # Test 5: Individual ringed as a chick (single record)
  # Test for the expected number of captures (3)
  expect_equal(nrow(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))), 3)
  # Test that the 1st capture of the chick is as expected (2005), no captureMonth and captureDay known
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$captureYear), 2005)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$captureMonth), NA_integer_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$captureDay), NA_integer_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$releaseRingNumber), "HL010189")
  # Test that the 3rd capture of the chick is as expected (2008-06-11)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$captureYear, 3), 2008)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$captureMonth, 3), 6)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$captureDay, 3), 11)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$captureRingNumber, 3), "HL010189")
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$releaseRingNumber, 3), "HL010189")
  # Test that exactAge (and minimumAge) on the 1st capture is as expected (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$exactAge), 0)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$minimumAge), 0)
  # Test that exactAge (and minimumAge) on the 3rd capture is as expected (3, because it's a chick caught 3 years after birth)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$exactAge, 3), 3)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "HL010189"))$minimumAge, 3), 3)

  # Test 6: Caught only as adult with 1 record
  # Test for the expected number of captures (1)
  expect_equal(nrow(subset(HAR_data, individualID == paste0("HAR_", "V675839"))), 1)
  # Test that the 1st capture of the adult is as expected (1991-05-19)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$captureYear), 1991)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$captureDay), 19)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$releaseRingNumber), "V675839")
  # Test that exactAge is as expected (NA, because it's ringed as an adult)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$exactAge, NA_integer_)
  # Test that minimumAge is as expected (1, because it's ringed as an adult)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "V675839"))$minimumAge, 1)

  # Test 7: Caught only as adult with multiple records
  # Test for the expected number of captures (9)
  expect_equal(nrow(subset(HAR_data, individualID == paste0("HAR_", "X103679"))), 9)
  # Test that the 1st capture of the adult is as expected (1992-06-03)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$captureYear), 1992)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$captureDay), 3)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$releaseRingNumber), "X103679")
  # Test that the 9th capture of the adult is as expected (1995-06-22)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$captureYear, 9), 1995)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$captureMonth, 9), 6)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$captureDay, 9), 22)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$captureRingNumber, 9), "X103679")
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$releaseRingNumber, 9), "X103679")
  # Test that exactAge on 1st and 9th capture are as expected (NA, because it's ringed as an adult)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$exactAge), NA_integer_)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$exactAge, 9), NA_integer_)
  # Test that minimumAge on 1st capture is as expected (1, because it's ringed as an adult)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$minimumAge), 1)
  # Test that minimumAge on 9th capture is as expected (4, because it's ringed as an adult and caught 3 years after ringing)
  expect_equal(dplyr::nth(subset(HAR_data, individualID == paste0("HAR_", "X103679"))$minimumAge, 9), 4)

  # Test 8: Caught with unknown age (FL)
  # Test for the expected number of captures (1)
  expect_equal(nrow(subset(HAR_data, individualID == paste0("HAR_", "X103582"))), 1)
  # Test that the 1st capture of the individual is as expected (1991-12-25)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103582"))$captureYear), 1991)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103582"))$captureMonth), 12)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103582"))$captureDay), 25)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103582"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(HAR_data, individualID == paste0("HAR_", "X103582"))$releaseRingNumber), "X103582")
  # Test that exactAge is as expected (NA, because it's ringed as an adult)
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "X103582"))$exactAge, NA_integer_)
  # Test that first and last age calculated is as expected
  expect_equal(subset(HAR_data, individualID == paste0("HAR_", "X103582"))$minimumAge, 1)

})

test_that("Measurement_data returns an expected outcome...", {

  # We want to run tests for individuals measured as adults, and individuals measured as chicks

  # Take a subset of HAR data
  HAR_data <- dplyr::filter(pipeline_output$Measurement_data, siteID == "HAR")

  # Test 1: Adult with 2 measurements
  # Test that individual HA32799 had been taken two measurements of in its first capture
  expect_equal(nrow(subset(HAR_data, recordID == paste0("HAR_", "HA32799", "_1"))), 2)
  # Test that its measurements are as expected (total wing length: 78, mass: 13.6)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "HA32799", "_1") &
                        measurementType == "total wing length")$measurementValue, 78)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "HA32799", "_1") &
                        measurementType == "mass")$measurementValue, 136/10)
  # Test that the measurements were taken on the correct date (2019-06-03)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "HA32799", "_1"))$measurementDeterminedYear, rep(2019, 2))
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "HA32799", "_1"))$measurementDeterminedMonth, rep(6, 2))
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "HA32799", "_1"))$measurementDeterminedDay, rep(3, 2))


  # Test 2: Adult with 0 measurements, should not be present
  expect_equal(nrow(subset(HAR_data, recordID == paste0("HAR_", "HC89464", "_1"))), 0)


  # Test 3: Chick with all measurements
  expect_equal(nrow(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2"))), 8)
  # Test that its total wing length is as expected (52)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2") &
                        measurementType == "total wing length")$measurementValue, 52)
  # Test that tarsus measures of both legs are as expected (20.7)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2") &
                        measurementType == "tarsus")$measurementValue, rep(20.7, 2))
  # Test that rectrix measures are as expected (24.5)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2") &
                        measurementType == "tail length")$measurementValue, rep(24.5, 2))
  # Test that p3 measures are as expected (33 for left, 35 for right)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2") &
                        measurementType == "p3" &
                        measurementMethod == "left")$measurementValue, 33)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2") &
                        measurementType == "p3" &
                        measurementMethod == "right")$measurementValue, 35)
  # Test that mass measurement is as expected (18.9)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2") &
                        measurementType == "mass")$measurementValue, 18.9)
  # Test that the measurements were taken on the correct date (1996-06-13)
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2"))$measurementDeterminedYear, rep(1996, 8))
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2"))$measurementDeterminedMonth, rep(6, 8))
  expect_equal(subset(HAR_data, recordID == paste0("HAR_", "X747248", "_2"))$measurementDeterminedDay, rep(13, 8))

  # Test 4: Chick with 0 measurements, should not be present
  expect_equal(nrow(subset(HAR_data, recordID == paste0("HAR_", "HA38030", "_1"))), 0)

})

test_that("Location_data returns an expected outcome...", {

  # We want to run tests for locations
  # Some locations (originally with the same ID) changed coordinates over time
  # Check that new IDs have been assigned

  # Take a subset of HAR data
  HAR_data <- dplyr::filter(pipeline_output$Location_data, siteID == "HAR")

  # Test 1: Nestbox, original location
  # Location listed as a nest box that has lat/long from separate file
  # Record has expected locationType
  expect_true(subset(HAR_data, locationID == paste("HAR", "00", "18_1", sep = "_"))$locationType == "nest")
  # startYear and endYear is as expected
  expect_equal(subset(HAR_data, locationID == paste("HAR", "00", "18_1", sep = "_"))$startYear, 1981L)
  expect_equal(subset(HAR_data, locationID == paste("HAR", "00", "18_1", sep = "_"))$endYear, 1983L)

  # Test 2: Nestbox, new location
  # Location listed as a nest box that has lat/long from separate file
  # Record has expected locationType
  expect_true(subset(HAR_data, locationID == paste("HAR", "00", "18_2", sep = "_"))$locationType == "nest")
  # startYear and endYear is as expected
  expect_equal(subset(HAR_data, locationID == paste("HAR", "00", "18_2", sep = "_"))$startYear, 1998L)
  expect_equal(subset(HAR_data, locationID == paste("HAR", "00", "18_2", sep = "_"))$endYear, 1998L)

  # Back-transform coordinates to test accuracy
  coords <- sf::st_transform(sf::st_as_sf(subset(HAR_data, locationID %in% c(paste("HAR", "00", "18_1", sep = "_"),
                                                                             paste("HAR", "00", "18_2", sep = "_"))),
                                          coords = c("decimalLongitude", "decimalLatitude"),
                                          crs = 4326),
                             crs = 2393) %>%
    sf::st_coordinates()

  # Check that latitude and longitude of the original location are as expected
  expect_equal(round(coords[1, 1]), 3222430)
  expect_equal(round(coords[1, 2]), 6739173)
  # Check that latitude and longitude of the new location are as expected
  expect_equal(round(coords[2, 1]), 3235105)
  expect_equal(round(coords[2, 2]), 6761105)

})

test_that("Experiment_data returns an expected outcome...", {

  # Take a subset of HAR data
  HAR_data <- dplyr::filter(pipeline_output$Experiment_data, siteID == "HAR")

  # Check experiment in 1994, with 3 treatment groups
  expect_equal(nrow(subset(HAR_data, experimentID == "1994_1")), 3)
  # Test that start date and end year are as expected
  expect_equal(subset(HAR_data, experimentID == "1994_1")$experimentStartYear, rep(1994, 3))
  expect_equal(subset(HAR_data, experimentID == "1994_1")$experimentStartMonth, rep(5, 3))
  expect_equal(subset(HAR_data, experimentID == "1994_1")$experimentStartDay, rep(29, 3))
  expect_equal(subset(HAR_data, experimentID == "1994_1")$experimentEndYear, rep(1994, 3))
  # Test that experiment is a calcium experiment and brood size manipulation
  expect_equal(subset(HAR_data, experimentID == "1994_1")$experimentType, rep("calcium experiment, brood size manipulation", 3))
  # As such, the experiment was conducted during the nestling period. Test that this is correct
  expect_equal(subset(HAR_data, experimentID == "1994_1")$experimentStage, rep("nestling", 3))
  # Test that treatment group 1 is the control
  expect_equal(subset(HAR_data, treatmentID == "1994_1_1")$treatmentDetails, "control")

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
  test_ID_format(pipeline_output, ID_col = "femaleID", ID_format = "^HAR_[:alpha:]{1,2}[:digit:]{5,6}$")

  ## maleID format is as expected
  test_ID_format(pipeline_output, ID_col = "maleID", ID_format = "^HAR_[:alpha:]{1,2}[:digit:]{5,6}$")

  ## individualID format in Capture data is as expected
  test_ID_format(pipeline_output, ID_col = "C-individualID", ID_format = "^HAR_[:alpha:]{1,2}[:digit:]{5,6}$")

  ## individualID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-individualID", ID_format = "^HAR_[:alpha:]{1,2}[:digit:]{5,6}$")

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

  ## treatmentID has only unique values
  test_unique_values(pipeline_output, "treatmentID")

})

test_that("Key columns in each table do not have NAs", {

  ## Brood
  test_NA_columns(pipeline_output, "Brood")

  ## Capture
  test_NA_columns(pipeline_output, "Capture") # TODO: Check with data owner for individualID, releaseRingNumber, speciesID, captureMonth, captureDay

  ## Individual
  test_NA_columns(pipeline_output, "Individual") # TODO: Check with data owner for ringMonth, ringDay

  ## Measurement
  test_NA_columns(pipeline_output, "Measurement")

  ## Location
  test_NA_columns(pipeline_output, "Location")

  ## Experiment
  test_NA_columns(pipeline_output, "Experiment") # TODO: Check with data owner for treatmentDetails

})

test_that("Categorical columns do not have unexpected values", {

  ## Brood
  test_category_columns(pipeline_output, "Brood")

  ## Capture
  test_category_columns(pipeline_output, "Capture") # TODO: Check with data owner for unknown species

  ## Individual
  test_category_columns(pipeline_output, "Individual")

  ## Measurement
  test_category_columns(pipeline_output, "Measurement")

  ## Location
  test_category_columns(pipeline_output, "Location")

  ## Experiment
  test_category_columns(pipeline_output, "Experiment")

})
