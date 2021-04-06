context("Run data quality check on East Dartmoor pipeline output")

test_that("PFN outputs all files...", {

  expect_true(all("EDM" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("EDM" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("EDM" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("EDM" %in% pipeline_output$Location_data$PopID))

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "EDM")

  #Test 1: Brood where clutch type = first (CltCd = 1 in raw)
  expect_equal(subset(PFN_data, BroodID == "6219")$Species, "CYACAE")
  expect_equal(subset(PFN_data, BroodID == "6219")$ClutchType_calculated, "first") #
  expect_equal(subset(PFN_data, BroodID == "6219")$LayDate_observed, as.Date("2016-03-31") + 35)
  expect_equal(subset(PFN_data, BroodID == "6219")$ClutchSize_observed, 8L)
  expect_equal(subset(PFN_data, BroodID == "6219")$BroodSize_observed, NA_integer_)
  expect_equal(subset(PFN_data, BroodID == "6219")$NumberFledged_observed, 3L)
  expect_equal(subset(PFN_data, BroodID == "6219")$AvgChickMass, mean(c(10.88, 10.81, 12.19)))
  expect_equal(subset(PFN_data, BroodID == "6219")$AvgTarsus, mean(c(16.4, 15.7, 17.3)))

  #Test 2: Brood where clutch type = replacement (because first is known to have failed, CltCd = 2 in raw)
  expect_equal(subset(PFN_data, BroodID == "6077")$Species, "CYACAE")
  expect_equal(subset(PFN_data, BroodID == "6077")$ClutchType_calculated, "replacement")
  expect_equal(subset(PFN_data, BroodID == "6077")$LayDate_observed, as.Date("2015-03-31") + 45)
  expect_equal(subset(PFN_data, BroodID == "6077")$ClutchSize_observed, 10L)
  expect_equal(subset(PFN_data, BroodID == "6077")$BroodSize_observed, NA_integer_)
  expect_equal(subset(PFN_data, BroodID == "6077")$NumberFledged_observed, 2L)
  expect_equal(subset(PFN_data, BroodID == "6077")$AvgChickMass, mean(c(8.61, 9.8)))
  expect_equal(subset(PFN_data, BroodID == "6077")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(PFN_data, BroodID == "2729")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "2729")$ClutchType_calculated, "replacement")
  expect_equal(subset(PFN_data, BroodID == "2729")$LayDate_observed, as.Date("1994-03-31") + 74)
  expect_equal(subset(PFN_data, BroodID == "2729")$ClutchSize_observed, 4L)
  expect_equal(subset(PFN_data, BroodID == "2729")$BroodSize_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "2729")$NumberFledged_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "2729")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "2729")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second (CltCd = 3 in raw)
  expect_equal(subset(PFN_data, BroodID == "4653")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "4653")$ClutchType_calculated, "second")
  expect_equal(subset(PFN_data, BroodID == "4653")$LayDate_observed, as.Date("2007-03-31") + 68)
  expect_equal(subset(PFN_data, BroodID == "4653")$ClutchSize_observed, 4L)
  expect_equal(subset(PFN_data, BroodID == "4653")$BroodSize_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "4653")$NumberFledged_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "4653")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "4653")$AvgTarsus, NA_real_)

  #Test 5: Brood where clutch type cannot be calculated (because female ID and LayDate are NA)
  expect_equal(subset(PFN_data, BroodID == "3952")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "3952")$ClutchType_calculated, NA_character_)
  expect_equal(subset(PFN_data, BroodID == "3952")$LayDate_observed, as.Date(NA))
  expect_equal(subset(PFN_data, BroodID == "3952")$ClutchSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "3952")$BroodSize_observed, 7L)
  expect_equal(subset(PFN_data, BroodID == "3952")$NumberFledged_observed, 0L)
  expect_equal(subset(PFN_data, BroodID == "3952")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "3952")$AvgTarsus, NA_real_)
})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "EDM")

  #Test 1: First as adult
  expect_equal(subset(PFN_data, IndvID == "B80960")$Sex_calculated, "F")
  expect_equal(subset(PFN_data, IndvID == "B80960")$Species, "FICHYP")
  #They should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(PFN_data, IndvID == "B80960")$BroodIDLaid, NA_character_)
  expect_equal(subset(PFN_data, IndvID == "B80960")$BroodIDFledged, NA_character_)
  #Ring age and season are as expected
  expect_equal(subset(PFN_data, IndvID == "B80960")$RingSeason, 1955L)
  expect_equal(subset(PFN_data, IndvID == "B80960")$RingAge, "adult")

  #Test 2: Caught first as chick
  expect_equal(subset(PFN_data, IndvID == "K185273")$Sex_calculated, NA_character_)
  expect_equal(subset(PFN_data, IndvID == "K185273")$Species, "FICHYP")
  #Check that BroodIDLaid/Fledged are as expected
  expect_equal(subset(PFN_data, IndvID == "K185273")$BroodIDLaid, "3176")
  expect_equal(subset(PFN_data, IndvID == "K185273")$BroodIDFledged, "3176")
  #Ring season is as expected
  expect_equal(subset(PFN_data, IndvID == "K185273")$RingSeason, 1997)
  expect_equal(subset(PFN_data, IndvID == "K185273")$RingAge, "chick")

})

test_that("Capture data returns an expected outcome...", {

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "EDM")

  #Test 1: Individual ringed as a chick (no chick measurement info)
  #Test that there are the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "Y369483")), 14)
  #Test that the first and last capture are as expected
  expect_equal(subset(PFN_data, IndvID == "Y369483")$CaptureDate[1], as.Date("2012-06-11"))
  expect_equal(subset(PFN_data, IndvID == "Y369483")$CaptureDate[14], as.Date("2019-06-03"))
  #Test that age observed is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_observed[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_observed[14], 16L)
  #Test that age calculated is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_calculated[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_calculated[14], 17L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Mass[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Mass[14], 12.2)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Tarsus[14], 17.1)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$WingLength[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$WingLength[14], NA_real_)

  #Test 2: Individual ringed as a chick (chick measurement info from brood data)
  #Test that there are the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "Z286392")), 3)
  #Test that the first and last capture are as expected
  expect_equal(subset(PFN_data, IndvID == "Z286392")$CaptureDate[1], as.Date("2015-05-29"))
  expect_equal(subset(PFN_data, IndvID == "Z286392")$CaptureDate[3], as.Date("2017-05-28"))
  #Test that age observed is as expected on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Age_observed[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Age_observed[3], 7L)
  #Test that age calculated is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Age_calculated[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Age_calculated[3], 7L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Mass[1], 13.73)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Mass[3], 12.5)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Tarsus[1], 16.4)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$Tarsus[3], 18.1)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "Z286392")$WingLength[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "Z286392")$WingLength[3], 81)

  # Test 3: individual only caught as an adult (and recovered dead)
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "V143745")), 4)
  #Test that the first and last captures are as expected
  expect_equal(subset(PFN_data, IndvID == "V143745")$CaptureDate[1], as.Date("2014-03-05"))
  expect_equal(subset(PFN_data, IndvID == "V143745")$CaptureDate[4], as.Date("2016-04-21"))
  #Test that first and last age observed is as expected
  expect_equal(subset(PFN_data, IndvID == "V143745")$Age_observed[1], 6L)
  expect_equal(subset(PFN_data, IndvID == "V143745")$Age_observed[4], 4L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(PFN_data, IndvID == "V143745")$Age_calculated[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "V143745")$Age_calculated[4], 8L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "V143745")$Mass[1], 17.9)
  expect_equal(subset(PFN_data, IndvID == "V143745")$Mass[4], NA_real_)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "V143745")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "V143745")$Tarsus[4], NA_real_)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "V143745")$WingLength[1], 73)
  expect_equal(subset(PFN_data, IndvID == "V143745")$WingLength[4], NA_real_)
  #Test that the capture event is correctly characterized
  expect_equal(subset(PFN_data, IndvID == "V143745")$CaptureAlive[1], TRUE)
  expect_equal(subset(PFN_data, IndvID == "V143745")$ReleaseAlive[1], TRUE)
  expect_equal(subset(PFN_data, IndvID == "V143745")$CaptureAlive[4], FALSE)
  expect_equal(subset(PFN_data, IndvID == "V143745")$ReleaseAlive[4], FALSE)

  #Test 4: A re-ringed bird
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "D140987")), 3+7)
  #Test that the first and last captures are as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$CaptureDate[1], as.Date("2013-06-13"))
  expect_equal(subset(PFN_data, IndvID == "D140987")$CaptureDate[3+7], as.Date("2020-06-04"))
  #Test that first and last age observed is as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_observed[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_observed[3+7], 18L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_calculated[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_calculated[3+7], 18L)
  #Test that mass is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "D140987")$Mass[1], 11.8)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Mass[3+7], 12.7)
  #Test that tarsus is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "D140987")$Tarsus[1], NA_real_)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Tarsus[3+7], 17.1)
  #Test that wing length is correct on first and last capture
  expect_equal(subset(PFN_data, IndvID == "D140987")$WingLength[1], 77)
  expect_equal(subset(PFN_data, IndvID == "D140987")$WingLength[3+7], 79)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "EDM")

  #Test 1: Nestbox check
  expect_true(subset(PFN_data, LocationID == "HIDE9")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(PFN_data, LocationID == "HIDE9")$NestboxID == "HIDE9")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$StartSeason, 1987L)
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$EndSeason, 2019L)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$PopID, "EDM")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(PFN_data, LocationID == "HIDE9")$Latitude, 2) %>% setNames(nm = NULL), 278419)
  expect_equal(round(subset(PFN_data, LocationID == "HIDE9")$Longitude, 2) %>% setNames(nm = NULL), 78643)
  #Check that habitat type is correct
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$HabitatType, "deciduous")

})
