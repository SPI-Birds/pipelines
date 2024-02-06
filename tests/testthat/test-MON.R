testthat::skip_if(!exists("data_path"))

pipeline_output <- format_MON(db = paste0(data_path, "/Montpellier"),
                              optional_variables = "all")

test_that("MON outputs all files...", {

  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Brood_data$siteID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Capture_data$captureSiteID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Individual_data$siteID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Measurement_data$siteID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Location_data$siteID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Experiment_data$siteID))

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of calculatedClutchType
  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Brood_data, siteID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Brood where clutch type = first
  expect_equal(subset(MON_data, broodID == "1984_pir_35_NB_1")$speciesID, "CYACAE")
  expect_equal(subset(MON_data, broodID == "1984_pir_35_NB_1")$calculatedClutchType, "first")
  expect_equal(subset(MON_data, broodID == "1984_pir_35_NB_1")$observedLayYear, 1984)
  expect_equal(subset(MON_data, broodID == "1984_pir_35_NB_1")$observedClutchSize, 5)
  expect_equal(subset(MON_data, broodID == "1984_pir_35_NB_1")$observedBroodSize, 4)
  expect_equal(subset(MON_data, broodID == "1984_pir_35_NB_1")$observedNumberFledged, 0)
  expect_equal(subset(MON_data, broodID == "1984_pir_35_NB_1")$breedingSeason, "1984")
  expect_equal(subset(MON_data, broodID == "1984_pir_35_NB_1")$nestAttemptNumber, 1)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(MON_data, broodID == "1980_pir_36_NB_1")$speciesID, "CYACAE")
  expect_equal(subset(MON_data, broodID == "1980_pir_36_NB_1")$calculatedClutchType, "replacement")
  expect_equal(subset(MON_data, broodID == "1980_pir_36_NB_1")$observedLayYear, 1980)
  expect_equal(subset(MON_data, broodID == "1980_pir_36_NB_1")$observedClutchSize, 5)
  expect_equal(subset(MON_data, broodID == "1980_pir_36_NB_1")$observedBroodSize, 5)
  expect_equal(subset(MON_data, broodID == "1980_pir_36_NB_1")$observedNumberFledged, 5)
  expect_equal(subset(MON_data, broodID == "1980_pir_36_NB_1")$breedingSeason, "1980")
  expect_equal(subset(MON_data, broodID == "1980_pir_36_NB_1")$nestAttemptNumber, 1)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(MON_data, broodID == "1984_pir_47_NB_1")$speciesID, "CYACAE")
  expect_equal(subset(MON_data, broodID == "1984_pir_47_NB_1")$calculatedClutchType, "replacement")
  expect_equal(subset(MON_data, broodID == "1984_pir_47_NB_1")$observedLayYear, 1984)
  expect_equal(subset(MON_data, broodID == "1984_pir_47_NB_1")$observedClutchSize, 7)
  expect_equal(subset(MON_data, broodID == "1984_pir_47_NB_1")$observedBroodSize, 7)
  expect_equal(subset(MON_data, broodID == "1984_pir_47_NB_1")$observedNumberFledged, 7)
  expect_equal(subset(MON_data, broodID == "1984_pir_47_NB_1")$breedingSeason, "1984")
  expect_equal(subset(MON_data, broodID == "1984_pir_47_NB_1")$nestAttemptNumber, 1)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(MON_data, broodID == "1992_rou_90_NB_2")$speciesID, "PARMAJ")
  expect_equal(subset(MON_data, broodID == "1992_rou_90_NB_2")$calculatedClutchType, "second")
  expect_equal(subset(MON_data, broodID == "1992_rou_90_NB_2")$observedLayYear, 1992)
  expect_equal(subset(MON_data, broodID == "1992_rou_90_NB_2")$observedClutchSize, 8)
  expect_equal(subset(MON_data, broodID == "1992_rou_90_NB_2")$observedBroodSize, 2)
  expect_equal(subset(MON_data, broodID == "1992_rou_90_NB_2")$observedNumberFledged, 1)
  expect_equal(subset(MON_data, broodID == "1992_rou_90_NB_2")$breedingSeason, "1992")
  expect_equal(subset(MON_data, broodID == "1992_rou_90_NB_2")$nestAttemptNumber, 1)

  #Test 5: BroodID has FemaleID that does not match proper ID format
  expect_equal(subset(MON_data, broodID == "2016_gram_39_NB_1")$femaleID, NA_character_)

  #Test 6: BroodID has MaleID that does not match proper ID format
  expect_equal(subset(MON_data, broodID == "2012_font_15_NB_1")$maleID, NA_character_)

  #Test 7: FemaleIDs only contain numbers (or V/O at the first character) and all are 6-8 characters long
  expect_true(all(nchar(MON_data$femaleID[!is.na(MON_data$femaleID)]) %in% c(6,7,8) &
                    stringr::str_detect(MON_data$femaleID[!is.na(MON_data$femaleID)], "^(V|O|[0-9])+[:digit:]+$")))

  #Test 8: MaleIDs only contain numbers (or V/O at the first character) and all are 6-8 characters long
  expect_true(all(nchar(MON_data$maleID[!is.na(MON_data$maleID)]) %in% c(6,7,8)
                  & stringr::str_detect(MON_data$maleID[!is.na(MON_data$maleID)], "^(V|O|[0-9])+[:digit:]+$")))

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Individual_data, siteID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Male caught first as breeder (unidentified age)
  #Individual 2221101 should be listed as a male coal tit
  expect_equal(subset(MON_data, individualID == "2221101")$calculatedSex, "M")
  expect_equal(subset(MON_data, individualID == "2221101")$speciesID, "PERATE")
  #They should have no broodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(MON_data, individualID == "2221101")$broodIDLaid, NA_character_)
  expect_equal(subset(MON_data, individualID == "2221101")$broodIDFledged, NA_character_)
  #Her tag year should be 1991 with a tagStage of 'NA' (age was not identified when first tagged)
  expect_equal(subset(MON_data, individualID == "2221101")$tagYear, 1991)
  expect_equal(subset(MON_data, individualID == "2221101")$tagStage, NA_character_)

  #Test 2: Female caught first as breeder (unidentified age)
  #Individual 2546616 should be listed as a female blue tit
  expect_equal(subset(MON_data, individualID == "2546616")$calculatedSex, "F")
  expect_equal(subset(MON_data, individualID == "2546616")$speciesID, "CYACAE")
  #She should have no broodIDLaid or Fledged because this individual was first caught as a breeder
  expect_equal(subset(MON_data, individualID == "2546616")$broodIDLaid, NA_character_)
  expect_equal(subset(MON_data, individualID == "2546616")$broodIDFledged, NA_character_)
  #Her tag year should be 1981 with a tagStage of 'NA' (unidentified age when captured)
  expect_equal(subset(MON_data, individualID == "2546616")$tagYear, 1981)
  expect_equal(subset(MON_data, individualID == "2546616")$tagStage, NA_character_)

  #Test 3: Caught as chick not cross-fostered
  #Individual 8189538 should be listed as a female great tit
  expect_equal(subset(MON_data, individualID == "8189538")$calculatedSex, "F")
  expect_equal(subset(MON_data, individualID == "8189538")$speciesID, "PARMAJ")
  #Check that broodIDLaid/Fledged are as expected
  #This individual was not cross-fostered, so they should be the same
  expect_equal(subset(MON_data, individualID == "8189538")$broodIDLaid, "2017_rou_28_NB_1")
  expect_equal(subset(MON_data, individualID == "8189538")$broodIDFledged, "2017_rou_28_NB_1")
  #tag Year should be 2017 with a tagstage of 'chick'
  expect_equal(subset(MON_data, individualID == "8189538")$tagYear, 2017)
  expect_equal(subset(MON_data, individualID == "8189538")$tagStage, "chick")

  #Test 4: Caught as chick and cross-fostered in same population
  #Check sex and species are as expected
  #Sex should be NA because it was never caught as an adult
  expect_equal(subset(MON_data, individualID == "2221172")$calculatedSex, NA_character_)
  expect_equal(subset(MON_data, individualID == "2221172")$speciesID, "CYACAE")
  #Check that broodIDLaid/Fledged are as expected
  expect_equal(subset(MON_data, individualID == "2221172")$broodIDLaid, "1991_pir_103_NB_1")
  expect_equal(subset(MON_data, individualID == "2221172")$broodIDFledged, "1991_pir_89_NB_1")
  #tag year and stage are as expected
  expect_equal(subset(MON_data, individualID == "2221172")$tagYear, 1991)
  expect_equal(subset(MON_data, individualID == "2221172")$tagStage, "chick")

  #Test 5: Caught as chick and cross-fostered to different population
  #Check sex and species are as expected (sex as NA because never seen as breeder)
  expect_equal(subset(MON_data, individualID == "7207569")$calculatedSex, NA_character_)
  expect_equal(subset(MON_data, individualID == "7207569")$speciesID, "CYACAE")
  #Check that broodIDLaid/Fledged are as expected
  expect_equal(subset(MON_data, individualID == "7207569")$broodIDLaid, "2014_fil_11_NB_1")
  expect_equal(subset(MON_data, individualID == "7207569")$broodIDFledged, "2014_ava_13_NB_1")
  #tag year and stage are as expected
  expect_equal(subset(MON_data, individualID == "7207569")$tagYear, 2014)
  expect_equal(subset(MON_data, individualID == "7207569")$tagStage, "chick")

  #Test 6: individualIDs only contain numbers (or V/O as the first character) and all are 6-8 characters long
  expect_true(all(nchar(MON_data$individualID[!is.na(MON_data$individualID)]) %in% c(6,7,8) &
                    stringr::str_detect(MON_data$individualID[!is.na(MON_data$individualID)], "^(V|O|[0-9])+[:digit:]+$")))

})

test_that("Capture data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Capture_data, captureSiteID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Individual ringed as a chick
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(MON_data, individualID == "2709339")), 6)
  #Test that the first capture is as expected
  #This should be NA because no date was recorded for first capture.
  expect_equal(subset(MON_data, individualID == "2709339")$captureYear[1], 1982)
  expect_equal(subset(MON_data, individualID == "2709339")$captureMonth[1], NA_integer_)
  expect_equal(subset(MON_data, individualID == "2709339")$captureDay[1], NA_integer_)

  #Test that the 6th capture of the female is as expected (1987-06-08)
  expect_equal(subset(MON_data, individualID == "2709339")$captureYear[6], 1987)
  expect_equal(subset(MON_data, individualID == "2709339")$captureMonth[6], 6)
  expect_equal(subset(MON_data, individualID == "2709339")$captureDay[6], 8)

  #Test that exact and minimum age are not defined as there is no date associated with the day the chick was tagged
  expect_equal(subset(MON_data, individualID == "2709339")$minimumAge[1], NA_integer_)
  expect_equal(subset(MON_data, individualID == "2709339")$minimumAge[6], NA_integer_)
  expect_equal(subset(MON_data, individualID == "2709339")$exactAge[1], NA_integer_)
  expect_equal(subset(MON_data, individualID == "2709339")$exactAge[6], NA_integer_)

  #Test 2: Female caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, individualID == "3672638")), 16)
  #Test that the first capture is as expected
  expect_equal(subset(MON_data, individualID == "3672638")$captureYear[1], 1992)
  expect_equal(subset(MON_data, individualID == "3672638")$captureMonth[1], 1)
  expect_equal(subset(MON_data, individualID == "3672638")$captureDay[1], 20)
  #Test that on first capture, captureTagID is NA and releaseTagID is 3672638
  expect_equal(subset(MON_data, individualID == "3672638")$captureTagID[1], NA_character_)
  expect_equal(subset(MON_data, individualID == "3672638")$releaseTagID[1], "3672638")
  #Test that the 16th capture is as expected
  expect_equal(subset(MON_data, individualID == "3672638")$captureYear[16], 1997)
  expect_equal(subset(MON_data, individualID == "3672638")$captureMonth[16], 1)
  expect_equal(subset(MON_data, individualID == "3672638")$captureDay[16], 28)
  #Test that on 16th capture, captureTagID and releaseTagID are 3672638
  expect_equal(subset(MON_data, individualID == "3672638")$captureTagID[16],"3672638")
  expect_equal(subset(MON_data, individualID == "3672638")$releaseTagID[16], "3672638")

  #Test that first and last minmum age calculated is as expected/test that first and last exact age is NA (not tagged as chick)
  expect_equal(subset(MON_data, individualID == "3672638")$minimumAge[1], 1L)
  expect_equal(subset(MON_data, individualID == "3672638")$minimumAge[16], 6L)
  expect_equal(subset(MON_data, individualID == "3672638")$exactAge[1], NA_integer_)
  expect_equal(subset(MON_data, individualID == "3672638")$exactAge[16], NA_integer_)

  #Test 3: Male caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, individualID == "4208517")), 15)
  #Test that the first capture date is as expected
  expect_equal(subset(MON_data, individualID == "4208517")$captureYear[1], 1993)
  expect_equal(subset(MON_data, individualID == "4208517")$captureMonth[1], 5)
  expect_equal(subset(MON_data, individualID == "4208517")$captureDay[1], 14)
  #Test that the first capture is not associated with a captureTagID but with a releaseTagID
  expect_equal(subset(MON_data, individualID == "4208517")$captureTagID[1], NA_character_)
  expect_equal(subset(MON_data, individualID == "4208517")$releaseTagID[1], "4208517")
  #Test that the 15th capture is as expected
  expect_equal(subset(MON_data, individualID == "4208517")$captureYear[15], 1998)
  expect_equal(subset(MON_data, individualID == "4208517")$captureMonth[15], 1)
  expect_equal(subset(MON_data, individualID == "4208517")$captureDay[15], 27)
  #Test that the last capture is associated with a captureTagID and a releaseTagID
  expect_equal(subset(MON_data, individualID == "4208517")$captureTagID[15], "4208517")
  expect_equal(subset(MON_data, individualID == "4208517")$releaseTagID[15], "4208517")

  #Test that first and last exact age  is set to NA and minimul age is as expected
  expect_equal(subset(MON_data, individualID == "4208517")$minimumAge[1], 1L)
  expect_equal(subset(MON_data, individualID == "4208517")$minimumAge[15], 5L)
  expect_equal(subset(MON_data, individualID == "4208517")$exactAge[1], NA_integer_)
  expect_equal(subset(MON_data, individualID == "4208517")$exactAge[15], NA_integer_)


  #Test 4: Adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, individualID == "4486371")), 8)
  #Test that the first capture is as expected
  expect_equal(subset(MON_data, individualID == "4486371")$captureYear[1], 2002)
  expect_equal(subset(MON_data, individualID == "4486371")$captureMonth[1], 4)
  expect_equal(subset(MON_data, individualID == "4486371")$captureDay[1], 26)
  #Test that the last capture is associated with a captureTagID and a releaseTagID
  expect_equal(subset(MON_data, individualID == "4486371")$captureTagID[1], NA_character_)
  expect_equal(subset(MON_data, individualID == "4486371")$releaseTagID[1], "4486371")
  #Test that the 8th capture is as expected
  expect_equal(subset(MON_data, individualID == "4486371")$captureYear[8], 2007)
  expect_equal(subset(MON_data, individualID == "4486371")$captureMonth[8], 4)
  expect_equal(subset(MON_data, individualID == "4486371")$captureDay[8], 30)
  #Test that the last capture is associated with a captureTagID and a releaseTagID
  expect_equal(subset(MON_data, individualID == "4486371")$captureTagID[8], "4486371")
  expect_equal(subset(MON_data, individualID == "4486371")$releaseTagID[8], "4486371")
  #Test that first and last minimum age is as expected and exact age is set to NA
  expect_equal(subset(MON_data, individualID == "4486371")$minimumAge[1], 0L)
  expect_equal(subset(MON_data, individualID == "4486371")$minimumAge[8], 5L)
  expect_equal(subset(MON_data, individualID == "4486371")$exactAge[1], 0L)
  expect_equal(subset(MON_data, individualID == "4486371")$exactAge[8], 5L)
  #Test that capture and release are different
  expect_equal(subset(MON_data, individualID == "4486371")$capturePlotID[7], "mur")
  expect_equal(subset(MON_data, individualID == "4486371")$releasePlotID[7], "aul")

  #Test 5: Adult first tagged as a chick with a "V" ring number
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, individualID == "V021562")), 3)
  #Test that the first capture is as expected
  expect_equal(subset(MON_data, individualID == "V021562")$captureYear[1], 2022)
  expect_equal(subset(MON_data, individualID == "V021562")$captureMonth[1], 6)
  expect_equal(subset(MON_data, individualID == "V021562")$captureDay[1], 3)
  #Test that the last capture is associated with a captureTagID and a releaseTagID
  expect_equal(subset(MON_data, individualID == "V021562")$captureTagID[1], NA_character_)
  expect_equal(subset(MON_data, individualID == "V021562")$releaseTagID[1], "V021562")
  #Test that the 8th capture is as expected
  expect_equal(subset(MON_data, individualID == "V021562")$captureYear[3], 2023)
  expect_equal(subset(MON_data, individualID == "V021562")$captureMonth[3], 7)
  expect_equal(subset(MON_data, individualID == "V021562")$captureDay[3], 6)
  #Test that the last capture is associated with a captureTagID and a releaseTagID
  expect_equal(subset(MON_data, individualID == "V021562")$captureTagID[3], "V021562")
  expect_equal(subset(MON_data, individualID == "V021562")$releaseTagID[3], "V021562")
  #Test that first and last minimum age is as expected and exact age is set to NA
  expect_equal(subset(MON_data, individualID == "V021562")$minimumAge[1], 0L)
  expect_equal(subset(MON_data, individualID == "V021562")$minimumAge[3], 1L)
  expect_equal(subset(MON_data, individualID == "V021562")$exactAge[1], 0L)
  expect_equal(subset(MON_data, individualID == "V021562")$exactAge[3], 1L)
  #Test that capture and release are different
  expect_equal(subset(MON_data, individualID == "V021562")$capturePlotID[3], "gram")
  expect_equal(subset(MON_data, individualID == "V021562")$releasePlotID[3], "gram")

  #Test 5: individualIDs are all properly formatted
  expect_true(all(nchar(MON_data$individualID) %in% c(6,7,8) & stringr::str_detect(MON_data$individualID, "^(V|O|[0-9])+[:digit:]+$")))

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Location_data, siteID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Nestbox check
  #Location listed as a nest box that has lat/long from separate file
  #Record has expected LocationType
  expect_true(subset(MON_data, locationID == "rou_314_NB")$locationType == "nest")
  #habitatID as expected
  expect_true(subset(MON_data, locationID == "rou_314_NB")$habitatID == "G1.7111")
  #Expect Start and EndYear is as expected
  expect_equal(subset(MON_data, locationID == "rou_314_NB")$startYear, 2002L)
  expect_equal(subset(MON_data, locationID == "rou_314_NB")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(MON_data, locationID == "rou_314_NB")$siteID, "ROU")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(MON_data, locationID == "rou_314_NB")$decimalLatitude, 2) %>% setNames(nm = NULL), 43.67)
  expect_equal(round(subset(MON_data, locationID == "rou_314_NB")$decimalLongitude, 2) %>% setNames(nm = NULL), 3.67)

  #Test 2: Nestbox check
  #Location with no lat/long info
  #Record has expected LocationType
  expect_true(subset(MON_data, locationID == "mau_1_NB")$locationType == "nest")
  #habitatID is not set
  expect_equal(subset(MON_data, locationID == "mau_1_NB")$habitatID, NA_character_)
  #Expect Start and End Year is not set
  expect_equal(subset(MON_data, locationID == "mau_1_NB")$startYear, NA_integer_)
  expect_equal(subset(MON_data, locationID == "mau_1_NB")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(MON_data, locationID == "mau_1_NB")$siteID, "MIS")
  #Check that latitude and longitude are as expected
  expect_equal(subset(MON_data, locationID == "mau_1_NB")$decimalLatitude %>% setNames(nm = NULL), NA_real_)
  expect_equal(subset(MON_data, locationID == "mau_1_NB")$decimalLongitude %>% setNames(nm = NULL), NA_real_)

  #Test 3: Mistnet check (location outside main study area)
  #LocationType is as expected
  expect_true(subset(MON_data, locationID == "hs_1_MN")$locationType == "capture")
  #Expect no habitatID
  expect_equal(subset(MON_data, locationID == "hs_1_MN")$habitatID, NA_character_)
  #Expect Start and EndYear is as expected
  expect_equal(subset(MON_data, locationID == "hs_1_MN")$startYear, 2009L)
  expect_equal(subset(MON_data, locationID == "hs_1_MN")$endYear, 2009L)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(MON_data, locationID == "hs_1_MN")$siteID, "MIS")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(MON_data, locationID == "hs_1_MN")$decimalLatitude, 2) %>% setNames(nm = NULL), 41.44)
  expect_equal(round(subset(MON_data, locationID == "hs_1_MN")$decimalLongitude, 2) %>% setNames(nm = NULL), 9.19)

  #Test 4: Mistnet check (caught at a nest box)
  #LocationType is as expected
  expect_true(subset(MON_data, locationID == "pir_140_NB")$locationType == "nest")
  #habitatID as expected
  expect_equal(subset(MON_data, locationID == "pir_140_NB")$habitatID, "G2.1215")
  #Expect Start and EndYear is as expected
  expect_equal(subset(MON_data, locationID == "pir_140_NB")$startYear, 1976L)
  expect_equal(subset(MON_data, locationID == "pir_140_NB")$endYear, NA_integer_)
  #Check that LocationID is in the expected siteID
  expect_equal(subset(MON_data, locationID == "pir_140_NB")$siteID, "PIR")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(MON_data, locationID == "pir_140_NB")$decimalLatitude, 1) %>% setNames(nm = NULL), 42.40)
  expect_equal(round(subset(MON_data, locationID == "pir_140_NB")$decimalLongitude, 2) %>% setNames(nm = NULL), 8.75)

})


test_that("Measurement_data returns an expected outcome...", {
  #We want to run test for measurements
  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Measurement_data, siteID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: several measurements for individual 3034847
  #tarsus should be 14.45 mm measured with the alternative method recorded by LK
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementValue[1], 14.45)
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementUnit[1], "mm")
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementMethod[1], "alternative")
  expect_equal(subset(MON_data, recordID == "3034847_2")$recordedBy[1], "LK")
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementType[1], "tarsus")
  #wing length should be 61.50 mm recorded by LK
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementValue[2], 61.5)
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementUnit[2], "mm")
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementMethod[2], NA_character_)
  expect_equal(subset(MON_data, recordID == "3034847_2")$recordedBy[2], "LK")
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementType[2], "wing length")

  #mass should be 8.80 g recorded by LK
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementValue[3], 8.8)
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementUnit[3], "g")
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementMethod[3], NA_character_)
  expect_equal(subset(MON_data, recordID == "3034847_2")$recordedBy[3], "LK")
  expect_equal(subset(MON_data, recordID == "3034847_2")$measurementType[3], "mass")

  #Test 2: several measurements for individual 8803886
  #tarsus should be 19.86 mm measured with the alternative method recorded by BC
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementValue[1], 19.86)
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementUnit[1], "mm")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementMethod[1], "alternative")
  expect_equal(subset(MON_data, recordID == "8803886_1")$recordedBy[1], "BC")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementType[1], "tarsus")

  #wing length should be 76.50 mm recorded by BC
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementValue[2], 76.50)
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementUnit[2], "mm")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementMethod[2], NA_character_)
  expect_equal(subset(MON_data, recordID == "8803886_1")$recordedBy[2], "BC")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementType[2], "wing length")

  #beak length should be 8.64 mm recorded by BC
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementValue[3], 8.64)
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementUnit[3], "mm")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementMethod[3], "beak length from nostril to tip of beak")
  expect_equal(subset(MON_data, recordID == "8803886_1")$recordedBy[3], "BC")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementType[3], "beak length")

  #tail length should be 68.50 mm recorded by BC
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementValue[4], 68.50)
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementUnit[4], "mm")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementMethod[4], NA_character_)
  expect_equal(subset(MON_data, recordID == "8803886_1")$recordedBy[4], "BC")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementType[4], "tail length")

  #mass should be 17.93 g recorded by BC
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementValue[5], 17.93)
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementUnit[5], "g")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementMethod[5], NA_character_)
  expect_equal(subset(MON_data, recordID == "8803886_1")$recordedBy[5], "BC")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementType[5], "mass")

  #behavorial test of handling docility should be 3.00 recorded by AFA
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementValue[6], 3.00)
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementUnit[6], "no unit")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementMethod[6], "behavioral score (0 to 3) of docility in hand")
  expect_equal(subset(MON_data, recordID == "8803886_1")$recordedBy[6], "AFA")
  expect_equal(subset(MON_data, recordID == "8803886_1")$measurementType[6], "handling docility")


  #Test 3: several measurements for individual (chick) V021380
  #tarsus should be 19.91 mm measured with the alternative method recorded by AL
  expect_equal(subset(MON_data, recordID == "V021380_1")$measurementValue[1], 19.91)
  expect_equal(subset(MON_data, recordID == "V021380_1")$measurementUnit[1], "mm")
  expect_equal(subset(MON_data, recordID == "V021380_1")$measurementMethod[1], "alternative")
  expect_equal(subset(MON_data, recordID == "V021380_1")$recordedBy[1], "AL")
  expect_equal(subset(MON_data, recordID == "V021380_1")$measurementType[1], "tarsus")

  #mass should be 15.50 g recorded by aL
  expect_equal(subset(MON_data, recordID == "V021380_1")$measurementValue[2], 15.50)
  expect_equal(subset(MON_data, recordID == "V021380_1")$measurementUnit[2], "g")
  expect_equal(subset(MON_data, recordID == "V021380_1")$measurementMethod[2], NA_character_)
  expect_equal(subset(MON_data, recordID == "V021380_1")$recordedBy[2], "AL")
  expect_equal(subset(MON_data, recordID == "V021380_1")$measurementType[2], "mass")

})


test_that("Experiment_data returns an expected outcome...", {
  #We want to run test for measurements
  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Experiment_data, siteID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: translocation experiment on brood
  #In 2001,  a translocation at brood stage with experimentID br_5
  expect_equal(subset(MON_data, treatmentID == "2001_PIR_br_5")$siteID, "PIR")
  expect_equal(subset(MON_data, treatmentID == "2001_PIR_br_5")$experimentID, "br_5")
  expect_equal(subset(MON_data, treatmentID == "2001_PIR_br_5")$experimentType, "Translocation")
  expect_equal(subset(MON_data, treatmentID == "2001_PIR_br_5")$treatmentStartYear, 2001)
  expect_equal(subset(MON_data, treatmentID == "2001_PIR_br_5")$treatmentEndYear, 2001)
  expect_equal(subset(MON_data, treatmentID == "2001_PIR_br_5")$treatmentStage, "Brood")

  #Test 2: experiment of plugging nestboxes in Montpellier
  #In 2023,  an experiment in two sites in Montpellier increasing competition for nestboxes and delaying laying dates. Start of experiment is expected
  #to be 22/2/2023 and end on 11/4/2023, with "AG" in charge of the experiment
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$siteID, "MON")
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$experimentID, "br_7")
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$experimentType, "Plugged_nestbox")
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$treatmentStartYear, 2023)
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$treatmentStartMonth, 2)
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$treatmentStartDay, 22)
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$treatmentEndYear, 2023)
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$treatmentEndMonth, 4)
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$treatmentEndDay, 11)
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$treatmentStage, "Pre-laying")
  expect_equal(subset(MON_data, treatmentID == "2023_MON_br_7")$recordedBy, "AG")

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

}) #tests passed


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

})
