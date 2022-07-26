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
