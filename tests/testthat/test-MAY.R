testthat::skip_if(!exists("data_path"))

# Run pipeline including all optional variables
pipeline_output <- format_MAY(db = paste0(data_path, "/MAY_Mayachino_Russia"),
                              optional_variables = "all")

test_that("MAY outputs all files...", {

  expect_true("MAY" %in% pipeline_output$Brood_data$siteID)
  expect_true("MAY" %in% pipeline_output$Capture_data$captureSiteID)
  expect_true("MAY" %in% pipeline_output$Individual_data$siteID)
  expect_true("MAY" %in% pipeline_output$Measurement_data$siteID)
  expect_true("MAY" %in% pipeline_output$Location_data$siteID)
  expect_true("MAY" %in% pipeline_output$Experiment_data$siteID)

})

test_that("Individual data returns an expected outcome...", {

  # We want to run a test for each sex for (sub-)adults and chicks.
  # It is important to distinguish between pied flycatchers and great tits,
  # as the primary data is stored in two separate files and formats.

  # Take a subset of MAY - Individual_data
  MAY_data <- dplyr::filter(pipeline_output$Individual_data, siteID == "MAY")

  # Test 1: Adult pied flycatcher female
  # Individual 593733 should be listed as a female pied flycatcher
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "593733"))$calculatedSex, "F")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "593733"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "593733"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "593733"))$broodIDFledged, NA_character_)
  # Her ring year should be 1979 with a ringStage of 'adult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "593733"))$ringYear, 1979)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "593733"))$ringStage, "adult")

  # Test 2: Adult pied flycatcher male
  # Individual 530660 should be listed as a male pied flycatcher
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$calculatedSex, "M")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$broodIDFledged, NA_character_)
  # His ring year should be 1981 with a ringStage of 'adult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$ringYear, 1981)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$ringStage, "adult")

  # Test 3: Subadult pied flycatcher female
  # Individual 531448 should be listed as a female pied flycatcher
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$calculatedSex, "F")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$broodIDFledged, NA_character_)
  # Her ring year should be 1981 with a ringStage of 'subadult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$ringYear, 1981)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$ringStage, "subadult")

  # Test 4: Subadult pied flycatcher male
  # Individual 712744 should be listed as a female pied flycatcher
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$calculatedSex, "M")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$broodIDFledged, NA_character_)
  # His ring year should be 1980 with a ringStage of 'subadult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$ringYear, 1980)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$ringStage, "subadult")

  # Test 5: Pied flycatcher chick
  # Individual 580222 should be listed as a pied flycatcher of unknown sex
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$calculatedSex, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # broodIDLaid & broodIDFledged should be 1982_B_59_163
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$broodIDLaid, "1982_B_59_163")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$broodIDFledged, "1982_B_59_163")
  # Ring year should be 1982 with a ringStage of 'chick'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$ringYear, 1982)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$ringStage, "chick")


  # Test 6: Adult great tit female
  # Individual XZ23672 should be listed as a female great tit
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$calculatedSex, "F")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$broodIDFledged, NA_character_)
  # Her ring year should be 2013 with a ringStage of 'adult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$ringYear, 2013)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$ringStage, "adult")

  # Test 7: Adult great tit male
  # Individual XY78791 should be listed as a male great tit
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$calculatedSex, "M")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$broodIDFledged, NA_character_)
  # His ring year should be 2007 with a ringStage of 'adult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$ringYear, 2007)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$ringStage, "adult")

  # Test 8: Subadult great tit female
  # Individual XB580013 should be listed as a female great tit
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$calculatedSex, "F")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$broodIDFledged, NA_character_)
  # Her ring year should be 1982 with a ringStage of 'subadult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$ringYear, 1982)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$ringStage, "subadult")

  # Test 9: Subadult great tit male
  # Individual XF69577 should be listed as a male great tit
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$calculatedSex, "M")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$broodIDFledged, NA_character_)
  # His ring year should be 2006 with a ringStage of 'subadult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$ringYear, 2006)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$ringStage, "subadult")

  # Test 10: Great tit chick
  # Individual 79330 should be listed as a great tit of unknown sex
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$calculatedSex, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # broodIDLaid & broodIDFledged should be 2008_B_45_303
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$broodIDLaid, "2008_B_45_303")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$broodIDFledged, "2008_B_45_303")
  # Ring year should be 2008 with a ringStage of 'chick'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$ringYear, 2008)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$ringStage, "chick")

})
