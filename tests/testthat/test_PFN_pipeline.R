context("Run data quality check on East Dartmoor pipeline output")

test_that("PFN outputs all files...", {

  expect_true(all("PFN" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("PFN" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("PFN" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("PFN" %in% pipeline_output$Location_data$PopID))

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "PFN")

  #Test 1: Brood where clutch type = first (CltCd = 1 in raw)
  expect_equal(subset(PFN_data, BroodID == "6219")$Species, "CYACAE")
  expect_equal(subset(PFN_data, BroodID == "6219")$ClutchType_calculated, "first") #
  expect_equal(subset(PFN_data, BroodID == "6219")$LayDate, as.Date("2016-03-31") + 35)
  expect_equal(subset(PFN_data, BroodID == "6219")$ClutchSize, 8L)
  expect_equal(subset(PFN_data, BroodID == "6219")$BroodSize, NA_integer_)
  expect_equal(subset(PFN_data, BroodID == "6219")$NumberFledged, 3L)
  expect_equal(subset(PFN_data, BroodID == "6219")$AvgChickMass, mean(c(10.88, 10.81, 12.19)))
  expect_equal(subset(PFN_data, BroodID == "6219")$AvgTarsus, mean(c(16.4, 15.7, 17.3)))

  #Test 2: Brood where clutch type = replacement (because first is known to have failed, CltCd = 2 in raw)
  expect_equal(subset(PFN_data, BroodID == "6077")$Species, "CYACAE")
  expect_equal(subset(PFN_data, BroodID == "6077")$ClutchType_calculated, "replacement")
  expect_equal(subset(PFN_data, BroodID == "6077")$LayDate, as.Date("2015-03-31") + 45)
  expect_equal(subset(PFN_data, BroodID == "6077")$ClutchSize, 10L)
  expect_equal(subset(PFN_data, BroodID == "6077")$BroodSize, NA_integer_)
  expect_equal(subset(PFN_data, BroodID == "6077")$NumberFledged, 2L)
  expect_equal(subset(PFN_data, BroodID == "6077")$AvgChickMass, mean(c(8.61, 9.8)))
  expect_equal(subset(PFN_data, BroodID == "6077")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(PFN_data, BroodID == "2729")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "2729")$ClutchType_calculated, "replacement")
  expect_equal(subset(PFN_data, BroodID == "2729")$LayDate, as.Date("1994-03-31") + 74)
  expect_equal(subset(PFN_data, BroodID == "2729")$ClutchSize, 4L)
  expect_equal(subset(PFN_data, BroodID == "2729")$BroodSize, 0L)
  expect_equal(subset(PFN_data, BroodID == "2729")$NumberFledged, 0L)
  expect_equal(subset(PFN_data, BroodID == "2729")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "2729")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second (CltCd = 3 in raw)
  expect_equal(subset(PFN_data, BroodID == "4653")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "4653")$ClutchType_calculated, "second")
  expect_equal(subset(PFN_data, BroodID == "4653")$LayDate, as.Date("2007-03-31") + 68)
  expect_equal(subset(PFN_data, BroodID == "4653")$ClutchSize, 4L)
  expect_equal(subset(PFN_data, BroodID == "4653")$BroodSize, 0L)
  expect_equal(subset(PFN_data, BroodID == "4653")$NumberFledged, 0L)
  expect_equal(subset(PFN_data, BroodID == "4653")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "4653")$AvgTarsus, NA_real_)

  #Test 5: Brood where clutch type cannot be calculated (because female ID and LayDate are NA)
  expect_equal(subset(PFN_data, BroodID == "3952")$Species, "FICHYP")
  expect_equal(subset(PFN_data, BroodID == "3952")$ClutchType_calculated, NA_character_)
  expect_equal(subset(PFN_data, BroodID == "3952")$LayDate, as.Date(NA))
  expect_equal(subset(PFN_data, BroodID == "3952")$ClutchSize, 7L)
  expect_equal(subset(PFN_data, BroodID == "3952")$BroodSize, 7L)
  expect_equal(subset(PFN_data, BroodID == "3952")$NumberFledged, 0L)
  expect_equal(subset(PFN_data, BroodID == "3952")$AvgChickMass, NA_real_)
  expect_equal(subset(PFN_data, BroodID == "3952")$AvgTarsus, NA_real_)
})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only PFN data
  PFN_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "PFN")

  #Test 1: First as adult
  expect_equal(subset(PFN_data, IndvID == "B80960")$Sex, "F")
  expect_equal(subset(PFN_data, IndvID == "B80960")$Species, "FICHYP")
  #They should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(PFN_data, IndvID == "B80960")$BroodIDLaid, NA_character_)
  expect_equal(subset(PFN_data, IndvID == "B80960")$BroodIDFledged, NA_character_)
  #Ring age and season are as expected
  expect_equal(subset(PFN_data, IndvID == "B80960")$RingSeason, 1955L)
  expect_equal(subset(PFN_data, IndvID == "B80960")$RingAge, "adult")

  #Test 2: Caught first as chick
  expect_equal(subset(PFN_data, IndvID == "K185273")$Sex, NA_character_)
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
  PFN_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "PFN")

  #Test 1: Individual ringed as a chick
  #Test there are the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "Y369483")), 7)
  #Test that the first and 7th capture are as expected
  expect_equal(subset(PFN_data, IndvID == "Y369483")$CaptureDate[1], as.Date("2012-05-16"))
  expect_equal(subset(PFN_data, IndvID == "Y369483")$CaptureDate[7], as.Date("2018-06-07"))
  #Test that age observed is as expected on first capture
  #Test that age observed is as expected on 7th capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_observed[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_observed[7], 4L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_calculated[1], 1L)
  expect_equal(subset(PFN_data, IndvID == "Y369483")$Age_calculated[7], 15L)

  #Test 2: Individual caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(PFN_data, IndvID == "D140987")), 6)
  #Test that the first capture is as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$CaptureDate[1], as.Date("2013-06-13"))
  #Test that the 6th capture is as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$CaptureDate[6], as.Date("2018-06-02"))
  #Test that first and last age observed is as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_observed[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_observed[6], 4L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_calculated[1], 4L)
  expect_equal(subset(PFN_data, IndvID == "D140987")$Age_calculated[6], 14L)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  PFN_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "PFN")

  #Test 1: Nestbox check
  expect_true(subset(PFN_data, LocationID == "HIDE9")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(PFN_data, LocationID == "HIDE9")$NestboxID == "HIDE9")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$StartSeason, 1989L)
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$EndSeason, 2015L)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(PFN_data, LocationID == "HIDE9")$PopID, "PFN")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(PFN_data, LocationID == "HIDE9")$Latitude, 2), NA_real_)
  expect_equal(round(subset(PFN_data, LocationID == "HIDE9")$Longitude, 2), NA_real_)

})
