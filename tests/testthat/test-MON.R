testthat::skip_if(!exists("data_path"))

pipeline_output <- format_MON(db = paste0(data_path, "/MON_Montpellier_France"))

test_that("MON outputs all files...", {

  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Location_data$PopID))
  expect_true(pipeline_output$protocol_version == "1.0.0")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Brood where clutch type = first
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$Species, "CYACAE")
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$ClutchType_calculated, "first")
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$LayDate, as.Date("1984-05-02"))
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$ClutchSize, 5)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$BroodSize, 4)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$NumberFledged, 0)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$Species, "CYACAE")
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$ClutchType_calculated, "replacement")
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$LayDate, as.Date("1980-05-30"))
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$ClutchSize, 5)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$BroodSize, 5)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$NumberFledged, 5)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$Species, "CYACAE")
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$ClutchType_calculated, "replacement")
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$LayDate, as.Date("1984-06-10"))
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$ClutchSize, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$BroodSize, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$NumberFledged, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$Species, "PARMAJ")
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$ClutchType_calculated, "second")
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$LayDate, as.Date("1992-05-20"))
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$ClutchSize, 8)
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$BroodSize, 2)
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$NumberFledged, 1)
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$AvgTarsus, NA_real_)

  #Test 5: BroodID has FemaleID that does not match proper ID format
  expect_equal(subset(MON_data, BroodID == "2016_gram_39_1")$FemaleID, NA_character_)

  #Test 6: BroodID has MaleID that does not match proper ID format
  expect_equal(subset(MON_data, BroodID == "2012_font_15_1")$MaleID, NA_character_)

  #Test 7: FemaleIDs only contain numbers and all are 6-8 characters long
  expect_true(all(nchar(MON_data$FemaleID[!is.na(MON_data$FemaleID)]) %in% c(6,7,8) &
                    stringr::str_detect(MON_data$FemaleID[!is.na(MON_data$FemaleID)], "^[:digit:]+$")))

  #Test 8: MaleIDs only contain numbers and all are 6-8 characters long
  expect_true(all(nchar(MON_data$MaleID[!is.na(MON_data$MaleID)]) %in% c(6,7,8)
                  & stringr::str_detect(MON_data$MaleID[!is.na(MON_data$MaleID)], "^[:digit:]+$")))

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Male caught first as adult
  #Individual C044309 should be listed as a male coal tit
  expect_equal(subset(MON_data, IndvID == "2221101")$Sex, "M")
  expect_equal(subset(MON_data, IndvID == "2221101")$Species, "PERATE")
  #They should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(MON_data, IndvID == "2221101")$BroodIDLaid, NA_character_)
  expect_equal(subset(MON_data, IndvID == "2221101")$BroodIDFledged, NA_character_)
  #Her ring season should be 1991 with a RingAge of 'adult'
  expect_equal(subset(MON_data, IndvID == "2221101")$RingSeason, 1991)
  expect_equal(subset(MON_data, IndvID == "2221101")$RingAge, "adult")

  #Test 2: Female caught first as adult
  #Individual 2546616 should be listed as a female blue tit
  expect_equal(subset(MON_data, IndvID == "2546616")$Sex, "F")
  expect_equal(subset(MON_data, IndvID == "2546616")$Species, "CYACAE")
  #She should have no BroodIDLaid or Fledged because this individual was caught as an adult
  expect_equal(subset(MON_data, IndvID == "2546616")$BroodIDLaid, NA_character_)
  expect_equal(subset(MON_data, IndvID == "2546616")$BroodIDFledged, NA_character_)
  #Her ring season should be 2003 with a RingAge of 'chick'
  expect_equal(subset(MON_data, IndvID == "2546616")$RingSeason, 1981)
  expect_equal(subset(MON_data, IndvID == "2546616")$RingAge, "adult")

  #Test 3: Caught as chick not cross-fostered
  #Individual 8189538 should be listed as a female great tit
  expect_equal(subset(MON_data, IndvID == "8189538")$Sex, "F")
  expect_equal(subset(MON_data, IndvID == "8189538")$Species, "PARMAJ")
  #Check that BroodIDLaid/Fledged are as expected
  #This individual was not cross-fostered, so they should be the same
  expect_equal(subset(MON_data, IndvID == "8189538")$BroodIDLaid, "2017_rou_28_2")
  expect_equal(subset(MON_data, IndvID == "8189538")$BroodIDFledged, "2017_rou_28_2")
  #Ring season should be 2017 with a RingAge of 'chick'
  expect_equal(subset(MON_data, IndvID == "8189538")$RingSeason, 2017)
  expect_equal(subset(MON_data, IndvID == "8189538")$RingAge, "chick")

  #Test 4: Caught as chick and cross-fostered in same population
  #Check sex and species are as expected
  #Sex should be NA because it was never caught as an adult
  expect_equal(subset(MON_data, IndvID == "2221172")$Sex, NA_character_)
  expect_equal(subset(MON_data, IndvID == "2221172")$Species, "CYACAE")
  #Check that BroodIDLaid/Fledged are as expected
  expect_equal(subset(MON_data, IndvID == "2221172")$BroodIDLaid, "1991_pir_103_1")
  expect_equal(subset(MON_data, IndvID == "2221172")$BroodIDFledged, "1991_pir_89_1")
  #RingSeason and Age are as expected
  expect_equal(subset(MON_data, IndvID == "2221172")$RingSeason, 1991)
  expect_equal(subset(MON_data, IndvID == "2221172")$RingAge, "chick")

  #Test 5: Caught as chick and cross-fostered to different population
  #Check sex and species are as expected
  expect_equal(subset(MON_data, IndvID == "7207569")$Sex, "M")
  expect_equal(subset(MON_data, IndvID == "7207569")$Species, "CYACAE")
  #Check that BroodIDLaid/Fledged are as expected
  expect_equal(subset(MON_data, IndvID == "7207569")$BroodIDLaid, "2014_fil_11_1")
  expect_equal(subset(MON_data, IndvID == "7207569")$BroodIDFledged, "2014_ava_13_1")
  #RingSeason and Age are as expected
  expect_equal(subset(MON_data, IndvID == "7207569")$RingSeason, 2014)
  expect_equal(subset(MON_data, IndvID == "7207569")$RingAge, "chick")

  #Test 6: IndvIDs only contain numbers and all are 6-8 characters long
  expect_true(all(nchar(MON_data$IndvID[!is.na(MON_data$IndvID)]) %in% c(6,7,8) &
                    stringr::str_detect(MON_data$IndvID[!is.na(MON_data$IndvID)], "^[:digit:]+$")))

})

test_that("Capture data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Individual ringed as a chick
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(MON_data, IndvID == "2709339")), 6)
  #Test that the first capture is as expected
  #This should be NA because no date was recorded for first capture.
  expect_equal(subset(MON_data, IndvID == "2709339")$CaptureDate[1], as.Date(NA))
  #Test that the 6th capture of the female is as expcted (1987-06-08)
  expect_equal(subset(MON_data, IndvID == "2709339")$CaptureDate[6], as.Date("1987-06-08"))
  #Test that age observed is as expected on first capture
  #Test that age observed is as expected on 6th capture
  expect_equal(subset(MON_data, IndvID == "2709339")$Age_observed[1], 1L)
  expect_equal(subset(MON_data, IndvID == "2709339")$Age_observed[6], 13L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(MON_data, IndvID == "2709339")$Age_calculated[1], 1L)
  expect_equal(subset(MON_data, IndvID == "2709339")$Age_calculated[6], 13L)

  #Test 2: Female caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, IndvID == "3672638")), 16)
  #Test that the first capture is as expected
  expect_equal(subset(MON_data, IndvID == "3672638")$CaptureDate[1], as.Date("1992-01-20"))
  #Test that the 16th capture is as expected
  expect_equal(subset(MON_data, IndvID == "3672638")$CaptureDate[16], as.Date("1997-01-28"))
  #Test that first and last age observed is as expected
  expect_equal(subset(MON_data, IndvID == "3672638")$Age_observed[1], 5L)
  expect_equal(subset(MON_data, IndvID == "3672638")$Age_observed[16], 15L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(MON_data, IndvID == "3672638")$Age_calculated[1], 4L)
  expect_equal(subset(MON_data, IndvID == "3672638")$Age_calculated[16], 14L)

  #Test 3: Male caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, IndvID == "4208517")), 15)
  #Test that the first capture is as expected
  expect_equal(subset(MON_data, IndvID == "4208517")$CaptureDate[1], as.Date("1993-05-14"))
  #Test that the 15th capture is as expected
  expect_equal(subset(MON_data, IndvID == "4208517")$CaptureDate[15], as.Date("1998-01-27"))
  #Test that first and last age observed is as expected
  expect_equal(subset(MON_data, IndvID == "4208517")$Age_observed[1], 6L)
  expect_equal(subset(MON_data, IndvID == "4208517")$Age_observed[15], 16L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(MON_data, IndvID == "4208517")$Age_calculated[1], 4L)
  expect_equal(subset(MON_data, IndvID == "4208517")$Age_calculated[15], 14L)

  #Test 4: Adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, IndvID == "4486371")), 8)
  #Test that the first capture is as expected
  expect_equal(subset(MON_data, IndvID == "4486371")$CaptureDate[1], as.Date("2002-04-26"))
  #Test that the 8th capture is as expected
  expect_equal(subset(MON_data, IndvID == "4486371")$CaptureDate[8], as.Date("2007-04-30"))
  #Test that first and last age observed is as expected
  expect_equal(subset(MON_data, IndvID == "4486371")$Age_observed[1], 1L)
  expect_equal(subset(MON_data, IndvID == "4486371")$Age_observed[8], 13L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(MON_data, IndvID == "4486371")$Age_calculated[1], 1L)
  expect_equal(subset(MON_data, IndvID == "4486371")$Age_calculated[8], 13L)
  #Test that capture and release are different
  expect_equal(subset(MON_data, IndvID == "4486371")$CapturePlot[7], "mur")
  expect_equal(subset(MON_data, IndvID == "4486371")$ReleasePlot[7], "aul")

  #Test 5: IndvIDs are all properly formatted
  expect_true(all(nchar(MON_data$IndvID) %in% c(6,7,8) & stringr::str_detect(MON_data$IndvID, "^[:digit:]+$")))

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  MON_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Nestbox check
  #Location listed as a nest box that has lat/long from separate file
  #Record has expected LocationType
  expect_true(subset(MON_data, LocationID == "rou_314_NB")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(MON_data, LocationID == "rou_314_NB")$NestboxID == "rou_314_NB")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(MON_data, LocationID == "rou_314_NB")$StartSeason, 2002L)
  expect_equal(subset(MON_data, LocationID == "rou_314_NB")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(MON_data, LocationID == "rou_314_NB")$PopID, "ROU")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(MON_data, LocationID == "rou_314_NB")$Latitude, 2) %>% setNames(nm = NULL), 43.67)
  expect_equal(round(subset(MON_data, LocationID == "rou_314_NB")$Longitude, 2) %>% setNames(nm = NULL), 3.67)

  #Test 2: Nestbox check
  #Location with no lat/long info
  #Record has expected LocationType
  expect_true(subset(MON_data, LocationID == "mau_1_NB")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(MON_data, LocationID == "mau_1_NB")$NestboxID == "mau_1_NB")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(MON_data, LocationID == "mau_1_NB")$StartSeason, 1987L)
  expect_equal(subset(MON_data, LocationID == "mau_1_NB")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(MON_data, LocationID == "mau_1_NB")$PopID, "MIS")
  #Check that latitude and longitude are as expected
  expect_equal(subset(MON_data, LocationID == "mau_1_NB")$Latitude %>% setNames(nm = NULL), NA_real_)
  expect_equal(subset(MON_data, LocationID == "mau_1_NB")$Longitude %>% setNames(nm = NULL), NA_real_)

  #Test 3: Mistnet check (location outside main study area)
  #LocationType is as expected
  expect_true(subset(MON_data, LocationID == "hs_1")$LocationType == "MN")
  #Expect no NestboxID
  expect_equal(subset(MON_data, LocationID == "hs_1")$NestboxID, NA_character_)
  #Expect Start and EndSeason is as expected
  expect_equal(subset(MON_data, LocationID == "hs_1")$StartSeason, 2009L)
  expect_equal(subset(MON_data, LocationID == "hs_1")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(MON_data, LocationID == "hs_1")$PopID, "MIS")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(MON_data, LocationID == "hs_1")$Latitude, 2) %>% setNames(nm = NULL), 41.44)
  expect_equal(round(subset(MON_data, LocationID == "hs_1")$Longitude, 2) %>% setNames(nm = NULL), 9.19)

  #Test 4: Mistnet check (caught at a nest box)
  #LocationType is as expected
  expect_true(subset(MON_data, LocationID == "pir_14_MN")$LocationType == "MN")
  #Expect no NestboxID
  expect_equal(subset(MON_data, LocationID == "pir_14_MN")$NestboxID, NA_character_)
  #Expect Start and EndSeason is as expected
  expect_equal(subset(MON_data, LocationID == "pir_14_MN")$StartSeason, 1985L)
  expect_equal(subset(MON_data, LocationID == "pir_14_MN")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(MON_data, LocationID == "pir_14_MN")$PopID, "PIR")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(MON_data, LocationID == "pir_14_MN")$Latitude, 2) %>% setNames(nm = NULL), 42.38)
  expect_equal(round(subset(MON_data, LocationID == "pir_14_MN")$Longitude, 2) %>% setNames(nm = NULL), 8.75)

})
