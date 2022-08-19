testthat::skip_if(!exists("data_path"))

# Run pipeline including all optional variables
pipeline_output <- format_WHZ(db = paste0(data_path, "/WHZ_Westerholz_Germany"),
                              optional_variables = "all")

test_that("WHZ outputs all files...", {

  expect_true("WHZ" %in% pipeline_output$Brood_data$siteID)
  expect_true("WHZ" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("WHZ" %in% pipeline_output$Individual_data$siteID)
  expect_true("WHZ" %in% pipeline_output$Measurement_data$siteID)
  expect_true("WHZ" %in% pipeline_output$Location_data$siteID)
  expect_true("WHZ" %in% pipeline_output$Experiment_data$siteID)

})


test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex for (sub-)adults and chicks.

  # Take a subset of WHZ - Individual_data
  WHZ_data <- dplyr::filter(pipeline_output$Individual_data, siteID == "WHZ")

  # Test 1: Adult blue tit female
  # Individual B1V5593 should be listed as a female blue tit
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1V5593"))$calculatedSex, "F")
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1V5593"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1V5593"))$broodIDLaid, NA_character_)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1V5593"))$broodIDFledged, NA_character_)
  # Her ring year should be 2007 with a ringStage of 'adult'
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1V5593"))$ringYear, 2007)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1V5593"))$ringStage, "adult")

  # Test 2: Adult blue tit male
  # Individual B2X3032 should be listed as a male blue tit
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3032"))$calculatedSex, "M")
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3032"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3032"))$broodIDLaid, NA_character_)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3032"))$broodIDFledged, NA_character_)
  # His ring year should be 2007 with a ringStage of 'adult'
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3032"))$ringYear, 2008)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3032"))$ringStage, "adult")

  # Test 3: Subadult blue tit female
  # Individual B1P5970 should be listed as a female blue tit
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1P5970"))$calculatedSex, "F")
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1P5970"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1P5970"))$broodIDLaid, NA_character_)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1P5970"))$broodIDFledged, NA_character_)
  # Her ring year should be 2007 with a ringStage of 'subadult'
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1P5970"))$ringYear, 2007)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B1P5970"))$ringStage, "subadult")

  # Test 4: Subadult blue tit male
  # Individual B2X3951 should be listed as a male blue tit
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3951"))$calculatedSex, "M")
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3951"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3951"))$broodIDLaid, NA_character_)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3951"))$broodIDFledged, NA_character_)
  # His ring year should be 2007 with a ringStage of 'subadult'
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3951"))$ringYear, 2009)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X3951"))$ringStage, "subadult")

  # Test 5: Blue tit chick (not caught later)
  # Individual B2X4460 should be listed as a blue tit with unknown sex
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X4460"))$calculatedSex, NA_character_)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X4460"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # broodIDLaid & broodIDFledged should be 2009_245_1
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X4460"))$broodIDLaid, "2009_245_1")
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X4460"))$broodIDFledged, "2009_245_1")
  # Ring year should be 2007 with a ringStage of 'chick'
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X4460"))$ringYear, 2009)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2X4460"))$ringStage, "chick")

  # Test 6: Blue tit chick (caught later and sex known)
  # Individual B2F5304 should be listed as a male blue tit
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2F5304"))$calculatedSex, "M")
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2F5304"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # broodIDLaid & broodIDFledged should be 2007_14_1
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2F5304"))$broodIDLaid, "2007_14_1")
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2F5304"))$broodIDFledged, "2007_14_1")
  # Ring year should be 2007 with a ringStage of 'chick'
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2F5304"))$ringYear, 2007)
  expect_equal(subset(WHZ_data, individualID == paste0("WHZ_", "B2F5304"))$ringStage, "chick")

})


test_that("Brood_data returns an expected outcome...", {

  # We want to run tests for all possible outcomes of clutch type.

  # Take a subset of WHZ - Brood_data
  WHZ_data <- dplyr::filter(pipeline_output$Brood_data, siteID == "WHZ")

  # Test 1: Blue tit brood clutch type = first
  # broodID 2007_48_1 should be CYACAE
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # broodID 2007_48_1 should have clutch type observed & calculated 'first'
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$observedClutchType, "first")
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$calculatedClutchType, "first")
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$nestAttemptNumber, 1)
  # Laying date should be "2007-04-10"
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$observedLayYear, 2007)
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$observedLayMonth, 4)
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$observedLayDay, 10)
  # Clutch size should be 12, brood size should be 10, fledgling number should be 10
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$observedClutchSize, 12)
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$observedBroodSize, 10)
  expect_equal(subset(WHZ_data, broodID == "2007_48_1")$observedNumberFledged, 10)

  # Test 2: Blue tit brood clutch type = replacement, where replacement is known (i.e. previous clutch was observed/recorded)
  # broodID 2015_113_2 should be CYACAE
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # broodID 2007_48_1 should have clutch type observed 'second' & calculated 'replacement' (because first was not successful)
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$observedClutchType, "second")
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$calculatedClutchType, "replacement")
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$nestAttemptNumber, 1)
  # Laying date should be "2015-04-30"
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$observedLayYear, 2015)
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$observedLayMonth, 4)
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$observedLayDay, 30)
  # Clutch size should be 8, brood size should be 6, fledgling number should be 0
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$observedClutchSize, 8)
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$observedBroodSize, 6)
  expect_equal(subset(WHZ_data, broodID == "2015_113_2")$observedNumberFledged, 0)

  # Test 3: Blue tit brood clutch type = replacement, where replacement calculated from 30-day cutoff
  # broodID 2013_62_1 should be CYACAE
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # broodID 2007_48_1 should have clutch type observed 'first' & calculated 'replacement'
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$observedClutchType, "first")
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$calculatedClutchType, "replacement")
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$nestAttemptNumber, 1)
  # Laying date should be "2015-05-13"
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$observedLayYear, 2017)
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$observedLayMonth, 5)
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$observedLayDay, 13)
  # Clutch size should be 7, brood size should be 7, fledgling number should be 6
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$observedClutchSize, 7)
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$observedBroodSize, 7)
  expect_equal(subset(WHZ_data, broodID == "2017_218_1")$observedNumberFledged, 6)

  # Test 4: Blue tit brood clutch type = second
  # broodID 2016_97_1 should be CYACAE
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10002"])
  # broodID 2016_97_1 should have clutch type observed 'first' & calculated 'second'
  # i.e., second clutch of female B4H0946 after a successful one that year
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$observedClutchType, "first")
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$calculatedClutchType, "second")
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$nestAttemptNumber, 2)
  # Laying date should be "2016-05-02"
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$observedLayYear, 2016)
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$observedLayMonth, 5)
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$observedLayDay, 2)
  # Clutch size should be 2, brood size should be 0, fledgling number should be 0
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$observedClutchSize, 2)
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$observedBroodSize, 0)
  expect_equal(subset(WHZ_data, broodID == "2016_97_1")$observedNumberFledged, 0)

})


test_that("Capture_data returns an expected outcome...", {

  # We want to run tests for captures for adults and chicks for both sexes.

  # Take a subset of WHZ - Capture_data
  WHZ_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID == "WHZ")

  # Test 1: Female caught as (sub)adult
  # Test that the female has the correct number of capture records (20)
  expect_equal(nrow(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))), 20)
  # Test that the 1st capture of the female is as expected (2015-11-09)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$captureYear), 2015)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$captureMonth), 11)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$captureDay), 09)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$releaseRingNumber), "B4H1755")
  # Test that the 20th capture of the female is as expected (2018-11-20)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$captureYear, 20), 2018)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$captureMonth, 20), 11)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$captureDay, 20), 20)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$captureRingNumber, 20), "B4H1755")
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$releaseRingNumber, 20), "B4H1755")
  # Test that exactAge is as expected (NA, because it's caught as a subadult)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's a subadult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$minimumAge), 1)
  # Test that minimumAge is correct on 20th capture (4, because it's a subadult caught 3 years after its first capture)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B4H1755"))$minimumAge, 20), 4)

  # Test 2: Male caught as (sub)adult
  # Test that the male has the correct number of capture records (5)
  expect_equal(nrow(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))), 5)
  # Test that the 1st capture of the female is as expected (2010-05-25)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$captureYear), 2010)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$captureDay), 25)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$releaseRingNumber), "B2V8479")
  # Test that the 5th capture of the male is as expected (2013-05-28)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$captureYear, 5), 2013)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$captureMonth, 5), 5)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$captureDay, 5), 28)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$captureRingNumber, 5), "B2V8479")
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$releaseRingNumber, 5), "B2V8479")
  # Test that exactAge is as expected (NA, because it's caught as a subadult)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's a subadult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$minimumAge), 1)
  # Test that minimumAge is correct on 5th capture (4, because it's a subadult caught 3 years after its first capture)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8479"))$minimumAge, 5), 4)

  # Test 3: Caught as chick
  # Test that the individual has the correct number of capture records (4)
  expect_equal(nrow(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))), 4)
  # Test that the 1st capture of the chick is as expected (2007-05-19)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$captureYear), 2007)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$captureDay), 19)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$captureRingNumber), NA_character_)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$releaseRingNumber), "B2V8308")
  # Test that the 4th capture of the chick is as expected (2010-05-17)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$captureYear, 4), 2010)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$captureMonth, 4), 5)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$captureDay, 4), 17)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$captureRingNumber, 4), "B2V8308")
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$releaseRingNumber, 4), "B2V8308")
  # Test that exactAge is correct on first capture (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$exactAge), 0)
  # Test that exactAge is correct on 4th capture (3, because it's caught as a chick, and 3 years later)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$exactAge, 4), 3)
  # Test that minimumAge is correct on first capture (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$minimumAge), 0)
  # Test that minimumAge is correct on 4th capture (3, because it's caught as a chick, and 3 years later)
  expect_equal(dplyr::nth(subset(WHZ_data, individualID == paste0("WHZ_", "B2V8308"))$minimumAge, 4), 3)

})


test_that("Measurement_data returns an expected outcome...", {

  # We want to run tests for individuals measured as adults, and individuals measured as chicks

  # Take a subset of WHZ - Measurement_data
  WHZ_data <- dplyr::filter(pipeline_output$Measurement_data, siteID == "WHZ")

  # Test 1: Adult with 3 measurements
  # Test that individual B1P5970 had been taken four measurements of in its first capture
  expect_equal(nrow(subset(WHZ_data, recordID == paste0("WHZ_", "B1P5970", "_1"))), 3)
  # Test that its measurements are as expected (tarsus: 16.55, total wing length: 66, mass: 11)
  expect_equal(subset(WHZ_data, recordID == paste0("WHZ_", "B1P5970", "_1") & measurementType == "tarsus")$measurementValue, 16.55)
  expect_equal(subset(WHZ_data, recordID == paste0("WHZ_", "B1P5970", "_1") & measurementType == "total wing length")$measurementValue, 66)
  expect_equal(subset(WHZ_data, recordID == paste0("WHZ_", "B1P5970", "_1") & measurementType == "mass")$measurementValue, 11)
  # Test that the measurements were taken in the correct year (2002)
  expect_equal(subset(WHZ_data, recordID == paste0("WHZ_", "B1P5970", "_1"))$measurementDeterminedYear, rep(2007, 3))

  # Test 2: Chick with 2 measurements
  # Test that individual B1P5970 had been taken two measurements of in its first capture
  expect_equal(nrow(subset(WHZ_data, recordID == paste0("WHZ_", "B2F5304", "_1"))), 2)
  # Test that its measurements are as expected (tarsus: 16.55, total wing length: 66, mass: 11)
  expect_equal(subset(WHZ_data, recordID == paste0("WHZ_", "B2F5304", "_1") & measurementType == "tarsus")$measurementValue, 16.8)
  expect_equal(subset(WHZ_data, recordID == paste0("WHZ_", "B2F5304", "_1") & measurementType == "mass")$measurementValue, 11)
  # Test that the measurements were taken in the correct year (2002)
  expect_equal(subset(WHZ_data, recordID == paste0("WHZ_", "B2F5304", "_1"))$measurementDeterminedYear, rep(2007, 2))

})


test_that("Location_data returns an expected outcome...", {

  # We want to run tests for nesting locations

  # Take a subset of WHZ - Location_data
  WHZ_data <- dplyr::filter(pipeline_output$Location_data, siteID == "WHZ")

  # Test 1: Nest box check
  # Nestbox 1 should be type "nest", and put up in 2007
  expect_equal(subset(WHZ_data, locationID == "WHZ_1")$locationType, "nest")
  expect_equal(subset(WHZ_data, locationID == "WHZ_1")$startYear, 2007)

  # Back-transform coordinates to test accuracy
  coords <- sf::st_transform(sf::st_as_sf(subset(WHZ_data, locationID == "WHZ_1"),
                                coords = c("decimalLongitude", "decimalLatitude"),
                                crs = sf::st_crs("+proj=longlat")),
                   sf::st_crs("+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +datum=potsdam +units=m +no_defs")) %>%
    sf::st_coordinates()

  # Test that the back-transformed coordinates match with the original crs
  expect_equal(round(coords[,1]), 4417157)
  expect_equal(round(coords[,2]), 5334225)

})


# test_that("Experiment_data returns an expected outcome...", {
#
#   # We want to run tests for experiments
#
#   # Take a subset of WHZ - Experiment_data
#   WHZ_data <- dplyr::filter(pipeline_output$Experiment_data, siteID == "WHZ")
#
# })

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
  test_ID_format(pipeline_output, ID_col = "femaleID", ID_format = "^WHZ_[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$")

  ## maleID format is as expected
  test_ID_format(pipeline_output, ID_col = "maleID", ID_format = "^WHZ_[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$")

  ## individualID format in Capture data is as expected
  test_ID_format(pipeline_output, ID_col = "C-individualID", ID_format = "^WHZ_[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$|^WHZ_X[:digit:]{8}$|^WHZ_X[:digit:]{2}_[:digit:]{3}[:upper:]{0,1}_[:digit:]{4}$")

  ## individualID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-individualID", ID_format = "^WHZ_[:upper:]{1}[:digit:]{1}[:upper:]{1}[:digit:]{4}$|^WHZ_X[:digit:]{8}$|^WHZ_X[:digit:]{2}_[:digit:]{3}[:upper:]{0,1}_[:digit:]{4}$")

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
  test_NA_columns(pipeline_output, "Capture") # TODO: Check with data owner for releaseRingNumber

  ## Individual
  test_NA_columns(pipeline_output, "Individual")

  ## Measurement
  test_NA_columns(pipeline_output, "Measurement")

  ## Location
  test_NA_columns(pipeline_output, "Location")

  ## Experiment
  #test_NA_columns(pipeline_output, "Experiment")
  # TODO: Check with data owner

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

  ## Experiment
  test_category_columns(pipeline_output, "Experiment")

})
