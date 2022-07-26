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
