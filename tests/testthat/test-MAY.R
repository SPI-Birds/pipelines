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

test_that("Brood_data returns an expected outcome...", {

  # We want to run tests for all possible outcomes of clutch type

  # Take a subset of MAY - Brood_data
  MAY_data <- dplyr::filter(pipeline_output$Brood_data, siteID == "MAY")

  # Test 1: Pied flycatcher brood clutch type = first
  # broodID 1980_L_19_16 should be FICHYP
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # broodID 1980_L_19_16 should have clutch type observed NA & calculated 'first'
  # No clutch types were recorded for pied flycatchers in MAY
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$observedClutchType, NA_character_)
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$calculatedClutchType, "first")
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$nestAttemptNumber, 1)
  # Laying date should be "1980-06-03"
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$observedLayYear, 1980)
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$observedLayMonth, 6)
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$observedLayDay, 3)
  # Clutch size should be 6, brood size should be 4, fledgling number should be 4
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$observedClutchSize, 6)
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$observedBroodSize, 4)
  expect_equal(subset(MAY_data, broodID == "1980_L_19_16")$observedNumberFledged, 4)

  # Test 2: Pied flycatcher brood clutch type = replacement, where replacement is known (i.e. previous clutch was observed/recorded)
  # broodID 2014_W_4_3506 should be FICHYP
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # broodID 2014_W_4_3506 should have clutch type observed NA & calculated 'replacement'
  # No clutch types were recorded for pied flycatchers in MAY
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$observedClutchType, NA_character_)
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$calculatedClutchType, "replacement")
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$nestAttemptNumber, 1)
  # Laying date should be "2014-06-05"
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$observedLayYear, 2014)
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$observedLayMonth, 6)
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$observedLayDay, 5)
  # Clutch size should be 6, brood size should be 5, fledgling number should be 5
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$observedClutchSize, 6)
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$observedBroodSize, 5)
  expect_equal(subset(MAY_data, broodID == "2014_W_4_3506")$observedNumberFledged, 5)

  # Test 3: Pied flycatcher brood clutch type = replacement, where replacement calculated from 30-day cutoff
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # broodID 1988_M_34_1042 should have clutch type observed NA & calculated 'replacement'
  # No clutch types were recorded for pied flycatchers in MAY
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$observedClutchType, NA_character_)
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$calculatedClutchType, "replacement")
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$nestAttemptNumber, 1)
  # Laying date should be "1988-06-26"
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$observedLayYear, 1988)
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$observedLayMonth, 6)
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$observedLayDay, 26)
  # Clutch size should be 5, brood size should be 5, fledgling number should be 5
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$observedClutchSize, 5)
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$observedBroodSize, 5)
  expect_equal(subset(MAY_data, broodID == "1988_M_34_1042")$observedNumberFledged, 5)

  # Test 4: Great tit brood clutch type = first
  # broodID 1986_M_54_100 should be PARMAJ
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # broodID 1986_M_54_100 should have clutch type observed & calculated 'first'
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$observedClutchType, "first")
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$calculatedClutchType, "first")
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$nestAttemptNumber, 1)
  # Laying date should be "1986-05-01"
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$observedLayYear, 1986)
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$observedLayMonth, 5)
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$observedLayDay, 1)
  # Clutch size should be 13, brood size should be 12, fledgling number should be unknown
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$observedClutchSize, 13)
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$observedBroodSize, 12)
  expect_equal(subset(MAY_data, broodID == "1986_M_54_100")$observedNumberFledged, NA_integer_)

  # Test 5: Great tit brood clutch type = second
  # broodID 1999_M_6_197 should be PARMAJ
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # broodID 1999_M_6_197 should have clutch type observed & calculated 'second'
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$observedClutchType, "second")
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$calculatedClutchType, "second")
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$nestAttemptNumber, 2)
  # Laying date should be "1999-06-19"
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$observedLayYear, 1999)
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$observedLayMonth, 6)
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$observedLayDay, 19)
  # Clutch size should be 11, brood size should be 10, fledgling number should be 10
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$observedClutchSize, 11)
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$observedBroodSize, 10)
  expect_equal(subset(MAY_data, broodID == "1999_M_6_197")$observedNumberFledged, 10)

  # Test 6: Great tit brood clutch type = replacement, where replacement is known (i.e. previous clutch was observed/recorded)
  # broodID 1994_M_16_171 should be PARMAJ
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # broodID 1994_M_16_171 should have clutch type observed & calculated 'replacement'
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$observedClutchType, "replacement")
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$calculatedClutchType, "replacement")
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$nestAttemptNumber, 2)
  # Laying date should be "1994-06-04"
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$observedLayYear, 1994)
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$observedLayMonth, 6)
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$observedLayDay, 4)
  # Clutch size should be 11, brood size should be 11, fledgling number should be 0
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$observedClutchSize, 11)
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$observedBroodSize, 11)
  expect_equal(subset(MAY_data, broodID == "1994_M_16_171")$observedNumberFledged, 0)

  # Test 7: Great tit brood clutch type = replacement, where replacement calculated from 30-day cutoff
  # broodID 2007_W_8_292 should be PARMAJ
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # broodID 2007_W_8_292 should have clutch type observed 'second' & calculated 'replacement'
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$observedClutchType, "second")
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$calculatedClutchType, "replacement")
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$nestAttemptNumber, 1)
  # Laying date should be "2007-07-03"
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$observedLayYear, 2007)
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$observedLayMonth, 7)
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$observedLayDay, 3)
  # Clutch size should be 9, brood size should be 9, fledgling number should be 8
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$observedClutchSize, 9)
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$observedBroodSize, 9)
  expect_equal(subset(MAY_data, broodID == "2007_W_8_292")$observedNumberFledged, 8)

})
