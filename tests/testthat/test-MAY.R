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
  # Her tag year should be 1979 with a tagStage of 'adult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "593733"))$tagYear, 1979)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "593733"))$tagStage, "adult")

  # Test 2: Adult pied flycatcher male
  # Individual 530660 should be listed as a male pied flycatcher
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$calculatedSex, "M")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$broodIDFledged, NA_character_)
  # His tag year should be 1981 with a tagStage of 'adult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$tagYear, 1981)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "530660"))$tagStage, "adult")

  # Test 3: Subadult pied flycatcher female
  # Individual 531448 should be listed as a female pied flycatcher
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$calculatedSex, "F")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$broodIDFledged, NA_character_)
  # Her tag year should be 1981 with a tagStage of 'subadult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$tagYear, 1981)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "531448"))$tagStage, "subadult")

  # Test 4: Subadult pied flycatcher male
  # Individual 712744 should be listed as a female pied flycatcher
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$calculatedSex, "M")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$broodIDFledged, NA_character_)
  # His tag year should be 1980 with a tagStage of 'subadult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$tagYear, 1980)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "712744"))$tagStage, "subadult")

  # Test 5: Pied flycatcher chick
  # Individual 580222 should be listed as a pied flycatcher of unknown sex
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$calculatedSex, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10003"])
  # broodIDLaid & broodIDFledged should be 1982_B_59_163
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$broodIDLaid, "1982_B_59_163")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$broodIDFledged, "1982_B_59_163")
  # Ring year should be 1982 with a tagStage of 'chick'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$tagYear, 1982)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "580222"))$tagStage, "chick")


  # Test 6: Adult great tit female
  # Individual XZ23672 should be listed as a female great tit
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$calculatedSex, "F")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$broodIDFledged, NA_character_)
  # Her tag year should be 2013 with a tagStage of 'adult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$tagYear, 2013)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XZ23672"))$tagStage, "adult")

  # Test 7: Adult great tit male
  # Individual XY78791 should be listed as a male great tit
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$calculatedSex, "M")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as an adult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$broodIDFledged, NA_character_)
  # His tag year should be 2007 with a tagStage of 'adult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$tagYear, 2007)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XY78791"))$tagStage, "adult")

  # Test 8: Subadult great tit female
  # Individual XB580013 should be listed as a female great tit
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$calculatedSex, "F")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # She should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$broodIDFledged, NA_character_)
  # Her tag year should be 1982 with a tagStage of 'subadult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$tagYear, 1982)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XB580013"))$tagStage, "subadult")

  # Test 9: Subadult great tit male
  # Individual XF69577 should be listed as a male great tit
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$calculatedSex, "M")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # He should have no broodIDLaid or broodIDFledged because this individual was caught as a subadult
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$broodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$broodIDFledged, NA_character_)
  # His tag year should be 2006 with a tagStage of 'subadult'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$tagYear, 2006)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "XF69577"))$tagStage, "subadult")

  # Test 10: Great tit chick
  # Individual 79330 should be listed as a great tit of unknown sex
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$calculatedSex, NA_character_)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$speciesID,
               species_codes$speciesID[species_codes$speciesCode == "10001"])
  # broodIDLaid & broodIDFledged should be 2008_B_45_303
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$broodIDLaid, "2008_B_45_303")
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$broodIDFledged, "2008_B_45_303")
  # Ring year should be 2008 with a tagStage of 'chick'
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$tagYear, 2008)
  expect_equal(subset(MAY_data, individualID == paste0("MAY_", "79330"))$tagStage, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  # We want to run tests for all possible outcomes of clutch type.
  # It is important to distinguish between pied flycatchers and great tits,
  # as the primary data is stored in two separate files and formats.

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

test_that("Capture_data returns an expected outcome...", {

  # We want to run tests for captures as both chicks, males, and females.
  # It is important to distinguish between pied flycatchers and great tits,
  # as the primary data is stored in two separate files and formats.

  # Take a subset of MAY - Capture_data
  MAY_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID == "MAY")

  # Test 1: Pied flycatcher female caught as adult
  # Test that the female has the correct number of capture records (7)
  expect_equal(nrow(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))), 7)
  # Test that the 1st capture of the female is as expected (2011-05-28: lay date + clutch size)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$captureYear), 2013)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$captureDay), 28)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$releaseTagID), "XZ23067")
  # Test that the 7th capture of the female is as expected (2019-05-30: lay date + clutch size)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$captureYear, 7), 2019)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$captureMonth, 7), 5)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$captureDay, 7), 30)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$captureTagID, 7), "XZ23067")
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$releaseTagID, 7), "XZ23067")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$minimumAge), 1)
  # Test that minimumAge is correct on 7th capture (7, because it's an adult caught 6 years after its first capture)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23067"))$minimumAge, 7), 7)

  # Test 2: Pied flycatcher male caught as adult
  # Test that the male has the correct number of capture records (7)
  expect_equal(nrow(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))), 7)
  # Test that the 1st capture of the female is as expected (2011-06-19: lay date + clutch size)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$captureYear), 2013)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$captureDay), 19)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$releaseTagID), "XZ23778")
  # Test that the 7th capture of the male is as expected (2016-06-06: lay date + clutch size)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$captureYear, 7), 2016)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$captureMonth, 7), 6)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$captureDay, 7), 6)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$captureTagID, 7), "XZ23778")
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$releaseTagID, 7), "XZ23778")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$minimumAge), 1)
  # Test that minimumAge is correct on 7th capture (3, because it's an adult caught just under 3 years after its first capture)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XZ23778"))$minimumAge, 7), 3)

  # Test 3: Pied flycatcher caught as chick
  # Test that the individual has the correct number of capture records (6)
  expect_equal(nrow(subset(MAY_data, individualID == paste0("MAY_", "580817"))), 6)
  # Test that the 1st capture of the female is as expected (1982-06-11: lay date + clutch size + ...)
  # Ring numbers should be NA, because the ID is an incomplete number (i.e., letters missing)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "580817"))$captureYear), 1982)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "580817"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "580817"))$captureDay), 11)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "580817"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "580817"))$releaseTagID), NA_character_)
  # Test that the 6th capture of the individual is as expected (1987-06-15: lay date + clutch size + ...)
  # Ring numbers should be NA, because the ID is an incomplete number (i.e., letters missing)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "580817"))$captureYear, 6), 1987)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "580817"))$captureMonth, 6), 6)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "580817"))$captureDay, 6), 15)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "580817"))$captureTagID, 6), NA_character_)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "580817"))$releaseTagID, 6), NA_character_)
  # Test that exactAge is correct on first capture (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "580817"))$exactAge), 0)
  # Test that exactAge is correct on 6th capture (5, because it's caught as a chick, and 5 years later)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "580817"))$exactAge, 6), 5)
  # Test that minimumAge is correct on first capture (0, because it's caught as a chick)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "580817"))$minimumAge), 0)
  # Test that minimumAge is correct on 6th capture (5, because it's caught as a chick, and 5 years later)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "580817"))$minimumAge, 6), 5)

  # Test 4: Great tit female caught as adult
  # Test that the female has the correct number of capture records (6)
  expect_equal(nrow(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))), 6)
  # Test that the 1st capture of the female is as expected (1982-05-19: lay date + clutch size)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$captureYear), 1982)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$captureMonth), 5)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$captureDay), 19)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$releaseTagID), "XB580014")
  # Test that the 6th capture of the female is as expected (1984-07-17: lay date + clutch size)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$captureYear, 6), 1984)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$captureMonth, 6), 7)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$captureDay, 6), 17)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$captureTagID, 6), "XB580014")
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$releaseTagID, 6), "XB580014")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$minimumAge), 1)
  # Test that minimumAge is correct on 6th capture (3, because it's an adult caught 2 years after its first capture)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XB580014"))$minimumAge, 6), 3)

  # Test 5: Great tit male caught as adult
  # Test that the male has the correct number of capture records (6)
  expect_equal(nrow(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))), 6)
  # Test that the 1st capture of the female is as expected (1980-06-05: lay date + clutch size)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$captureYear), 1980)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$captureMonth), 6)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$captureDay), 5)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$captureTagID), NA_character_)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$releaseTagID), "XA712924")
  # Test that the 6th capture of the male is as expected (1983-06-28: lay date + clutch size)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$captureYear, 6), 1983)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$captureMonth, 6), 6)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$captureDay, 6), 28)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$captureTagID, 6), "XA712924")
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$releaseTagID, 6), "XA712924")
  # Test that exactAge is as expected (NA, because it's caught as an adult)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$exactAge), NA_integer_)
  # Test that minimumAge is correct on first capture (1, because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(dplyr::first(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$minimumAge), 1)
  # Test that minimumAge is correct on 6th capture (4, because it's an adult caught 3 years after its first capture)
  expect_equal(dplyr::nth(subset(MAY_data, individualID == paste0("MAY_", "XA712924"))$minimumAge, 6), 4)

  # Test 6: Great tit caught as chick

  # NB: Issue with IDs in great tit data. Chicks all start with numbers; parents mostly start with letters.
  # Verify with data owner, and fill test later.

})

test_that("Measurement_data returns an expected outcome...", {

  # We want to run tests for individuals that had all all measurements taken, and individuals with some measurements missing

  # Take a subset of MAY - Measurement_data
  MAY_data <- dplyr::filter(pipeline_output$Measurement_data, siteID == "MAY")

  # Test 1: Pied flycatcher, all measurements
  # Test that individual XK46551 had been taken four measurements of in its first capture
  expect_equal(nrow(subset(MAY_data, recordID == paste0("MAY_", "XK46551", "_1"))), 4)
  # Test that its measurements are as expected (drost: 3.5, wing length: 81, tarsus: 18.2, molt: 1)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46551", "_1") & measurementType == "plumage colour")$measurementValue, 3.5)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46551", "_1") & measurementType == "wing length")$measurementValue, 81)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46551", "_1") & measurementType == "tarsus")$measurementValue, 18.2)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46551", "_1") & measurementType == "molt")$measurementValue, 1)
  # Test that the measurements were taken in the correct year (2002)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46551", "_1"))$measurementDeterminedYear, rep(2002, 4))

  # Test 2: Pied flycatcher, some measurements
  # Test that individual XK46355 had been taken three measurements of in its first capture
  expect_equal(nrow(subset(MAY_data, recordID == paste0("MAY_", "XK46355", "_1"))), 3)
  # Test that its measurements are as expected (drost: 5, wing length: 81, molt: 0)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46355", "_1") & measurementType == "plumage colour")$measurementValue, 5)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46355", "_1") & measurementType == "wing length")$measurementValue, 81)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46355", "_1") & measurementType == "molt")$measurementValue, 0)
  # Test that there is no tarsus measurement
  expect_equal(nrow(subset(MAY_data, recordID == paste0("MAY_", "XK46355", "_1") & measurementType == "tarsus")), 0)
  # Test that the measurements were taken in the correct year (2001)
  expect_equal(subset(MAY_data, recordID == paste0("MAY_", "XK46355", "_1"))$measurementDeterminedYear, rep(2001, 3))

  # NB: no measurements were taken of great tits

})

test_that("Location_data returns an expected outcome...", {

  # We want to run tests for nesting locations

  # Take a subset of MAY - Location_data
  MAY_data <- dplyr::filter(pipeline_output$Location_data, siteID == "MAY")

  # Test 1: Nest box check
  # Nestbox 34 in M should be type "nest", and put up in 1979
  expect_equal(subset(MAY_data, locationID == "M_34")$locationType, "nest")
  expect_equal(subset(MAY_data, locationID == "M_34")$startYear, 1979)

})

# test_that("Experiment_data returns an expected outcome...", {
#
#   # We want to run tests for experiments
#
#   # Take a subset of MAY - Location_data
#   MAY_data <- dplyr::filter(pipeline_output$Experiment_data, siteID == "MAY")
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
  test_ID_format(pipeline_output, ID_col = "femaleID", ID_format = "^MAY_[:upper:]{0,2}[:digit:]{5,6}$")

  ## maleID format is as expected
  test_ID_format(pipeline_output, ID_col = "maleID", ID_format = "^MAY_[:upper:]{0,2}[:digit:]{5,6}$")

  ## individualID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-individualID", ID_format = "^MAY_[:upper:]{0,2}[:digit:]{5,6}$")

  ## individualID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-individualID", ID_format = "^MAY_[:upper:]{0,2}[:digit:]{5,6}$")

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
  test_NA_columns(pipeline_output, "Capture") # TODO: Check with data owner for releaseTagID

  ## Individual
  test_NA_columns(pipeline_output, "Individual")

  ## Measurement
  test_NA_columns(pipeline_output, "Measurement") # TODO: Check with data owner for measurementDeterminedYear, measurementUnit

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
