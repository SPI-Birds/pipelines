testthat::skip_if(!exists("data_path"))
pipeline_output <- format_STR(db = paste0(data_path, "/Strasbourg"),
                              optional_variables = "all")


test_that("STR outputs all files...", {

  expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Brood_data$siteID))
  expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Capture_data$captureSiteID))
  expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Individual_data$siteID))
  expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Measurement_data$siteID))
  expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Location_data$siteID))
  expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Experiment_data$siteID))

})



test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of calculatedClutchType
  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Brood_data, siteID %in% c("STR", "ROB", "WAN"))

  #Test 1: Brood where clutch type = first
  expect_equal(subset(STR_data, broodID == "2019_cnrs_19_NB_1")$speciesID, "PARMAJ")
  expect_equal(subset(STR_data, broodID == "2019_cnrs_19_NB_1")$calculatedClutchType, "first")
  expect_equal(subset(STR_data, broodID == "2019_cnrs_19_NB_1")$observedLayYear, 2019)
  expect_equal(subset(STR_data, broodID == "2019_cnrs_19_NB_1")$observedClutchSize, 10)
  expect_equal(subset(STR_data, broodID == "2019_cnrs_19_NB_1")$observedBroodSize, 10)
  expect_equal(subset(STR_data, broodID == "2019_cnrs_19_NB_1")$observedNumberFledged, 3)
  expect_equal(subset(STR_data, broodID == "2019_cnrs_19_NB_1")$breedingSeason, "2019")
  expect_equal(subset(STR_data, broodID == "2019_cnrs_19_NB_1")$nestAttemptNumber, 1)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(STR_data, broodID == "2022_robe_5_NB_2")$speciesID, "PARMAJ")
  expect_equal(subset(STR_data, broodID == "2022_robe_5_NB_2")$calculatedClutchType, "replacement")
  expect_equal(subset(STR_data, broodID == "2022_robe_5_NB_2")$observedLayYear, 2022)
  expect_equal(subset(STR_data, broodID == "2022_robe_5_NB_2")$observedClutchSize, 10)
  expect_equal(subset(STR_data, broodID == "2022_robe_5_NB_2")$observedBroodSize, 10)
  expect_equal(subset(STR_data, broodID == "2022_robe_5_NB_2")$observedNumberFledged, 5)
  expect_equal(subset(STR_data, broodID == "2022_robe_5_NB_2")$breedingSeason, "2022")
  expect_equal(subset(STR_data, broodID == "2022_robe_5_NB_2")$nestAttemptNumber, 1)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(STR_data, broodID == "2017_want_112_NB_1")$speciesID, "PARMAJ")
  expect_equal(subset(STR_data, broodID == "2017_want_112_NB_1")$calculatedClutchType, "replacement")
  expect_equal(subset(STR_data, broodID == "2017_want_112_NB_1")$observedLayYear, 2017)
  expect_equal(subset(STR_data, broodID == "2017_want_112_NB_1")$observedClutchSize, NA_integer_)
  expect_equal(subset(STR_data, broodID == "2017_want_112_NB_1")$observedBroodSize, 7)
  expect_equal(subset(STR_data, broodID == "2017_want_112_NB_1")$observedNumberFledged, 6)
  expect_equal(subset(STR_data, broodID == "2017_want_112_NB_1")$breedingSeason, "2017")
  expect_equal(subset(STR_data, broodID == "2017_want_112_NB_1")$nestAttemptNumber, 1)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(STR_data, broodID == "2021_cita_65_NB_2")$speciesID, "PARMAJ")
  expect_equal(subset(STR_data, broodID == "2021_cita_65_NB_2")$calculatedClutchType, "second")
  expect_equal(subset(STR_data, broodID == "2021_cita_65_NB_2")$observedLayYear, 2021)
  expect_equal(subset(STR_data, broodID == "2021_cita_65_NB_2")$observedClutchSize, 6)
  expect_equal(subset(STR_data, broodID == "2021_cita_65_NB_2")$observedBroodSize, NA_integer_)
  expect_equal(subset(STR_data, broodID == "2021_cita_65_NB_2")$observedNumberFledged, NA_integer_)
  expect_equal(subset(STR_data, broodID == "2021_cita_65_NB_2")$breedingSeason, "2021")
  expect_equal(subset(STR_data, broodID == "2021_cita_65_NB_2")$nestAttemptNumber, 2)

  #Test 5: FemaleIDs only contain numbers (or V/O at the first character) and all are 6-8 characters long (until 2022, only chicks with V+6digits ring number, but males
  #and females breeders recruited can have one in 2023 onwards)
  expect_true(all(nchar(STR_data$femaleID[!is.na(STR_data$femaleID)]) %in% c(6,7,8) &
                    stringr::str_detect(STR_data$femaleID[!is.na(STR_data$femaleID)], "^(V|[0-9])+[:digit:]+$")))

  #Test 8: MaleIDs only contain numbers (or V/O at the first character) and all are 6-8 characters long
  expect_true(all(nchar(STR_data$maleID[!is.na(STR_data$maleID)]) %in% c(6,7,8)
                  & stringr::str_detect(STR_data$maleID[!is.na(STR_data$maleID)], "^(V|O|[0-9])+[:digit:]+$")))

})


test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Individual_data, siteID %in% c("WAN", "STR", "ROB"))

  #Test 1: Male caught first as breeder (unidentified age)
  #Individual 7971223 should be listed as a male great tit
  expect_equal(subset(STR_data, individualID == "7971223")$calculatedSex, "M")
  expect_equal(subset(STR_data, individualID == "7971223")$speciesID, "PARMAJ")
  #They should have no broodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(STR_data, individualID == "7971223")$broodIDLaid, NA_character_)
  expect_equal(subset(STR_data, individualID == "7971223")$broodIDFledged, NA_character_)
  #His tag year should be 2016 with a tagStage of 'NA' (age was not identified when first tagged)
  expect_equal(subset(STR_data, individualID == "7971223")$tagYear, 2016)
  expect_equal(subset(STR_data, individualID == "7971223")$tagStage, NA_character_)

  #Test 2: Female caught first as breeder (unidentified age)
  #Individual 7971215 should be listed as a female great tit
  expect_equal(subset(STR_data, individualID == "7971215")$calculatedSex, "F")
  expect_equal(subset(STR_data, individualID == "7971215")$speciesID, "PARMAJ")
  #She should have no broodIDLaid or Fledged because this individual was first caught as a breeder
  expect_equal(subset(STR_data, individualID == "7971215")$broodIDLaid, NA_character_)
  expect_equal(subset(STR_data, individualID == "7971215")$broodIDFledged, NA_character_)
  #Her tag year should be 2016 with a tagStage of 'NA' (unidentified age when captured)
  expect_equal(subset(STR_data, individualID == "7971215")$tagYear, 2016)
  expect_equal(subset(STR_data, individualID == "7971215")$tagStage, NA_character_)

  #Test 3: Caught as chick
  #Individual 7971346 should be listed as a [conflicted sex] great tit
  expect_equal(subset(STR_data, individualID == "7971346")$calculatedSex, "C")
  expect_equal(subset(STR_data, individualID == "7971346")$speciesID, "PARMAJ")
  #Check that broodIDLaid/Fledged are as expected
  #This individual was not cross-fostered, so they should be the same
  expect_equal(subset(STR_data, individualID == "7971346")$broodIDLaid, "2017_robe_11_NB_1")
  expect_equal(subset(STR_data, individualID == "7971346")$broodIDFledged, "2017_robe_11_NB_1")
  #tag Year should be 2017 with a tagstage of 'chick'
  expect_equal(subset(STR_data, individualID == "7971346")$tagYear, 2017)
  expect_equal(subset(STR_data, individualID == "7971346")$tagStage, "chick")

  #Test 4: individualIDs only contain numbers (or V as the first character) and all are 6-8 characters long
  expect_true(all(nchar(STR_data$individualID[!is.na(STR_data$individualID)]) %in% c(6,7,8) &
                    stringr::str_detect(STR_data$individualID[!is.na(STR_data$individualID)], "^(V|[0-9])+[:digit:]+$")))

})



test_that("Capture data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID %in% c("STR", "WAN", "ROB"))

  #Test 1: Individual ringed as a chick
  #Test the male has the correct number of capture records
  expect_equal(nrow(subset(STR_data, individualID == "8211502")), 6)
  #Test that the first capture is as expected (2017-06-09)
  expect_equal(subset(STR_data, individualID == "8211502")$captureYear[1], 2017)
  expect_equal(subset(STR_data, individualID == "8211502")$captureMonth[1], 6)
  expect_equal(subset(STR_data, individualID == "8211502")$captureDay[1], 9)

  #Test that the 6th capture of the male is as expected (2022-05-06)
  expect_equal(subset(STR_data, individualID == "8211502")$captureYear[6], 2022)
  expect_equal(subset(STR_data, individualID == "8211502")$captureMonth[6], 5)
  expect_equal(subset(STR_data, individualID == "8211502")$captureDay[6], 6)

  #Test that exact and minimum age calculated are correct on first capture and last capture
  expect_equal(subset(STR_data, individualID == "8211502")$minimumAge[1], 0L)
  expect_equal(subset(STR_data, individualID == "8211502")$minimumAge[6], 4L)
  expect_equal(subset(STR_data, individualID == "8211502")$exactAge[1], 0L)
  expect_equal(subset(STR_data, individualID == "8211502")$exactAge[6], 4L)

  #Test 2: Female caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(STR_data, individualID == "8211824")), 5)
  #Test that the first capture is as expected (2018-05-08)
  expect_equal(subset(STR_data, individualID == "8211824")$captureYear[1], 2018)
  expect_equal(subset(STR_data, individualID == "8211824")$captureMonth[1], 5)
  expect_equal(subset(STR_data, individualID == "8211824")$captureDay[1], 8)
  #Test that on first capture, captureTagID is NA and releaseTagID is 8811824
  expect_equal(subset(STR_data, individualID == "8211824")$captureTagID[1], NA_character_)
  expect_equal(subset(STR_data, individualID == "8211824")$releaseTagID[1], "8211824")
  #Test that the 5th capture is as expected (2021-06-11)
  expect_equal(subset(STR_data, individualID == "8211824")$captureYear[5], 2021)
  expect_equal(subset(STR_data, individualID == "8211824")$captureMonth[5], 6)
  expect_equal(subset(STR_data, individualID == "8211824")$captureDay[5], 11)
  #Test that on 5th capture, captureTagID and releaseTagID are 8211824
  expect_equal(subset(STR_data, individualID == "8211824")$captureTagID[5],"8211824")
  expect_equal(subset(STR_data, individualID == "8211824")$releaseTagID[5], "8211824")

  #Test that first and last minmum age calculated is as expected/test that first and last exact age is NA (not tagged as chick)
  expect_equal(subset(STR_data, individualID == "8211824")$minimumAge[1], 1L)
  expect_equal(subset(STR_data, individualID == "8211824")$minimumAge[5], 4L)
  expect_equal(subset(STR_data, individualID == "8211824")$exactAge[1], NA_integer_)
  expect_equal(subset(STR_data, individualID == "8211824")$exactAge[5], NA_integer_)

  #Test 3: Male caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(STR_data, individualID == "7971525")), 4)
  #Test that the first capture date is as expected (2018-05-18)
  expect_equal(subset(STR_data, individualID == "7971525")$captureYear[1], 2018)
  expect_equal(subset(STR_data, individualID == "7971525")$captureMonth[1], 5)
  expect_equal(subset(STR_data, individualID == "7971525")$captureDay[1], 18)
  #Test that the first capture is not associated with a captureTagID but with a releaseTagID
  expect_equal(subset(STR_data, individualID == "7971525")$captureTagID[1], NA_character_)
  expect_equal(subset(STR_data, individualID == "7971525")$releaseTagID[1], "7971525")
  #Test that the 4th capture is as expected (2022-05-24)
  expect_equal(subset(STR_data, individualID == "7971525")$captureYear[4], 2022)
  expect_equal(subset(STR_data, individualID == "7971525")$captureMonth[4], 5)
  expect_equal(subset(STR_data, individualID == "7971525")$captureDay[4], 24)
  #Test that the last capture is associated with a captureTagID and a releaseTagID
  expect_equal(subset(STR_data, individualID == "7971525")$captureTagID[4], "7971525")
  expect_equal(subset(STR_data, individualID == "7971525")$releaseTagID[4], "7971525")

  #Test that first and last exact age  is set to NA and minimul age is as expected
  expect_equal(subset(STR_data, individualID == "7971525")$minimumAge[1], 1L)
  expect_equal(subset(STR_data, individualID == "7971525")$minimumAge[4], 5L)
  expect_equal(subset(STR_data, individualID == "7971525")$exactAge[4], NA_integer_)
  expect_equal(subset(STR_data, individualID == "7971525")$exactAge[4], NA_integer_)


  #Test 4: individualIDs are all properly formatted
  expect_true(all(nchar(STR_data$individualID) %in% c(6,7,8) & stringr::str_detect(STR_data$individualID, "^(V|[0-9])+[:digit:]+$")))

})


test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Location_data, siteID %in% c("STR", "WAN", "ROB"))

  #Test 1: Nestbox check
  #Location listed as a nest box that has lat/long from separate file
  #Record has expected LocationType
  expect_true(subset(STR_data, locationID == "cita_69_NB_1")$locationType == "nest")
  #habitatID as expected
  expect_true(subset(STR_data, locationID == "cita_69_NB_1")$habitatID == "J1")
  #Expect Start and EndYear is as expected
  expect_equal(subset(STR_data, locationID == "cita_69_NB_1")$startYear, 2018L)
  expect_equal(subset(STR_data, locationID == "cita_69_NB_1")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(STR_data, locationID == "cita_69_NB_1")$siteID, "STR")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(STR_data, locationID == "cita_69_NB_1")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  expect_equal(round(subset(STR_data, locationID == "cita_69_NB_1")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.78)

  #Test 2: Nestbox check
  #Location with no lat/long info
  #Record has expected LocationType
  expect_true(subset(STR_data, locationID == "obse_11a_NB_1")$locationType == "nest")
  #habitatID is set to J1 (zone corresponds to "downtown", so urban habitat)
  expect_equal(subset(STR_data, locationID == "obse_11a_NB_1")$habitatID, "J1")
  #Expect Start and End Year set respectively to 2014 and 2020
  expect_equal(subset(STR_data, locationID == "obse_11a_NB_1")$startYear, 2014L)
  expect_equal(subset(STR_data, locationID == "obse_11a_NB_1")$endYear, 2020L)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(STR_data, locationID == "obse_11a_NB_1")$siteID, "STR")
  #Check that latitude and longitude are as expected
  expect_equal(subset(STR_data, locationID == "obse_11a_NB_1")$decimalLatitude %>% setNames(nm = NULL), NA_real_)
  expect_equal(subset(STR_data, locationID == "obse_11a_NB_1")$decimalLongitude %>% setNames(nm = NULL), NA_real_)

  #Test 3: Mistnet check (location outside main study area)
  #LocationType is as expected
  expect_true(subset(STR_data, locationID == "want_MN_1")$locationType == "capture")
  #LocationDetails is as expected (specifiy "Mist net")
  expect_true(subset(STR_data, locationID == "want_MN_1")$locationDetails == "Mist net")
  #habitatID should correspond to forest habitat (G1) since "want" site corresponds to a forest site (within WAN siteID)
  expect_equal(subset(STR_data, locationID == "want_MN_1")$habitatID, "G1")
  #Expect Start and EndYear is as expected
  expect_equal(subset(STR_data, locationID == "want_MN_1")$startYear, 2019L)
  expect_equal(subset(STR_data, locationID == "want_MN_1")$endYear, 2019L)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(STR_data, locationID == "want_MN_1")$siteID, "WAN")

  #Test 4: Nestbox check: case when nest box type was changed
  #LocationType is as expected for both occurrences
  expect_true(subset(STR_data, locationID == "espl_21_NB_1")$locationType == "nest")
  expect_true(subset(STR_data, locationID == "espl_21_NB_2")$locationType == "nest")
  #LocationDetails is as expected for both occurrences
  expect_true(subset(STR_data, locationID == "espl_21_NB_1")$locationDetails == "Wooden nesting box")
  expect_equal(subset(STR_data, locationID == "espl_21_NB_2")$locationDetails, NA_character_)
  #habitatID as expected for both occurrences
  expect_equal(subset(STR_data, locationID == "espl_21_NB_1")$habitatID, "J1")
  expect_equal(subset(STR_data, locationID == "espl_21_NB_2")$habitatID, "J1")
  #Expect Start and EndYear is as expected for both occurrences
  expect_equal(subset(STR_data, locationID == "espl_21_NB_1")$startYear, 2014L)
  expect_equal(subset(STR_data, locationID == "espl_21_NB_1")$endYear, 2019L)
  expect_equal(subset(STR_data, locationID == "espl_21_NB_2")$startYear, 2020L)
  expect_equal(subset(STR_data, locationID == "espl_21_NB_2")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(STR_data, locationID == "espl_21_NB_1")$siteID, "STR")
  expect_equal(subset(STR_data, locationID == "espl_21_NB_2")$siteID, "STR")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(STR_data, locationID == "espl_21_NB_1")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  expect_equal(round(subset(STR_data, locationID == "espl_21_NB_1")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.77)
  expect_equal(round(subset(STR_data, locationID == "espl_21_NB_2")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  expect_equal(round(subset(STR_data, locationID == "espl_21_NB_2")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.77)

  #Test 5: Nestbox check: case when there was a gap in monitoring the nest box
  #LocationType is as expected for both occurrences
  expect_true(subset(STR_data, locationID == "cita_32_NB_1")$locationType == "nest")
  expect_true(subset(STR_data, locationID == "cita_32_NB_2")$locationType == "nest")
  #LocationDetails is as expected for both occurrences
  expect_true(subset(STR_data, locationID == "cita_32_NB_1")$locationDetails == "Schwegler nesting box")
  expect_true(subset(STR_data, locationID == "cita_32_NB_2")$locationDetails == "Schwegler nesting box")
  #habitatID as expected for both occurrences
  expect_equal(subset(STR_data, locationID == "cita_32_NB_1")$habitatID, "J1")
  expect_equal(subset(STR_data, locationID == "cita_32_NB_2")$habitatID, "J1")
  #Expect Start and EndYear is as expected for both occurrences
  expect_equal(subset(STR_data, locationID == "cita_32_NB_1")$startYear, 2017L)
  expect_equal(subset(STR_data, locationID == "cita_32_NB_1")$endYear, 2018L)
  expect_equal(subset(STR_data, locationID == "cita_32_NB_2")$startYear, 2022L)
  expect_equal(subset(STR_data, locationID == "cita_32_NB_2")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(STR_data, locationID == "cita_32_NB_1")$siteID, "STR")
  expect_equal(subset(STR_data, locationID == "cita_32_NB_2")$siteID, "STR")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(STR_data, locationID == "cita_32_NB_1")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  expect_equal(round(subset(STR_data, locationID == "cita_32_NB_1")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.77)
  expect_equal(round(subset(STR_data, locationID == "cita_32_NB_2")$decimalLatitude, 2) %>% setNames(nm = NULL), 48.58)
  expect_equal(round(subset(STR_data, locationID == "cita_32_NB_2")$decimalLongitude, 2) %>% setNames(nm = NULL), 7.77)

})

test_that("Measurement_data returns an expected outcome...", {
  #We want to run test for measurements
  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Measurement_data, siteID %in% c("STR", "WAN", "ROB"))

  #Test 1: several measurements for individual 7971665
  #tarsus should be 19.30 mm measured with the alternative method recorded by obs_1
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[1], 19.30)
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[1], "mm")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[1], "alternative")
  expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[1], "obs_1")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[1], "tarsus")
  #wing length should be 76.0 mm recorded by obs_1
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[2], 76.0)
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[2], "mm")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[2], "flattened, maximum chord from ESF guidelines")
  expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[2], "obs_1")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[2], "wing length")

  #mass should be 16.40 g recorded by obs_1
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[5], 16.4)
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[5], "g")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[5], NA_character_)
  expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[5], "obs_1")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[5], "mass")

  #head-beak length should be 30 mm recorded by obs_1
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[3], 30)
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[3], "mm")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[3], "length from the back of the head to the tip of the beak")
  expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[3], "obs_1")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[3], "head beak length")

  #fat score should be 1 recorded by obs_1
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[4], 1)
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[4], "no unit")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[4], "fat score from ESF guidelines")
  expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[4], "obs_1")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[4], "fat score")

  #handling docility should be 0 recorded by obs_1
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementValue[6], 0)
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementUnit[6], "no unit")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementMethod[6], "behavioral score (0 to 3) of docility in hand")
  expect_equal(subset(STR_data, recordID == "7971665_1")$recordedBy[6], "obs_1")
  expect_equal(subset(STR_data, recordID == "7971665_1")$measurementType[6], "handling docility")


  #Test 2: several measurements for individual (chick) V017121
  #tarsus should be 19.7 mm measured with the alternative method recorded by obs_9
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[1], 19.7)
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[1], "mm")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[1], "alternative")
  expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[1], "obs_9")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[1], "tarsus")
  #wing length should be 49.0 mm recorded by obs_9
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[2], 49.0)
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[2], "mm")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[2], "flattened, maximum chord from ESF guidelines")
  expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[2], "obs_9")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[2], "wing length")
  #mass should be 16.2g recorded by obs_9
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[5], 16.2)
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[5], "g")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[5], NA_character_)
  expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[5], "obs_9")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[5], "mass")

  #head-beak length should be 25.5 mm recorded by obs_9
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[3], 25.5)
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[3], "mm")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[3], "length from the back of the head to the tip of the beak")
  expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[3], "obs_9")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[3], "head beak length")

  #fat score should be 3.0 recorded by obs_9
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementValue[4], 3.0)
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementUnit[4], "no unit")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementMethod[4], "fat score from ESF guidelines")
  expect_equal(subset(STR_data, recordID == "V017121_1")$recordedBy[4], "obs_9")
  expect_equal(subset(STR_data, recordID == "V017121_1")$measurementType[4], "fat score")

})

test_that("Experiment_data returns an expected outcome...", {
  #We want to run test for measurements
  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Experiment_data, siteID %in% c("STR", "WAN", "ROB"))

  #Test 1: translocation experiment on brood
  #In 2019,  a sampling (no information on stage) with experimentID br_5
  expect_equal(subset(STR_data, treatmentID == "2019_ROB_br_5")$siteID, "ROB")
  expect_equal(subset(STR_data, treatmentID == "2019_ROB_br_5")$experimentID, "br_5")
  expect_equal(subset(STR_data, treatmentID == "2019_ROB_br_5")$experimentType, "sampling")
  expect_equal(subset(STR_data, treatmentID == "2019_ROB_br_5")$treatmentStartYear, 2019)
  expect_equal(subset(STR_data, treatmentID == "2019_ROB_br_5")$treatmentEndYear, 2019)
  expect_equal(subset(STR_data, treatmentID == "2019_ROB_br_5")$treatmentStage, NA_character_)


})


## General tests (for pipelines formatted to standard protocol version 2.0)


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

}) #test passed


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

  ## Experiment
  test_NA_columns(pipeline_output, "Experiment")

}) #Test passed


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

}) #Test passed

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

}) #Test passed




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

}) #Test passed

