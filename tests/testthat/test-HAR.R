testthat::skip_if(!exists("data_path"))

pipeline_output <- format_HAR(db = paste0(data_path, "/HAR_Harjavalta_Finland"))

test_that("HAR outputs all files...", {

  expect_true("HAR" %in% pipeline_output$Brood_data$PopID)
  expect_true("HAR" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("HAR" %in% pipeline_output$Individual_data$PopID)
  expect_true("HAR" %in% pipeline_output$Location_data$PopID)
  expect_true(pipeline_output$protocol_version == "1.0.0")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only HAR data
  HAR_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "HAR")

  #Test 1: Brood where clutch type = first
  expect_equal(subset(HAR_data, BroodID == "2018_0101_1")$Species, "PARMAJ")
  expect_equal(subset(HAR_data, BroodID == "2018_0101_1")$ClutchType_calculated, "first")
  expect_equal(subset(HAR_data, BroodID == "2018_0101_1")$LayDate, as.Date("2018-05-08"))
  expect_equal(subset(HAR_data, BroodID == "2018_0101_1")$ClutchSize, 8)
  expect_equal(subset(HAR_data, BroodID == "2018_0101_1")$BroodSize, 4)
  expect_equal(subset(HAR_data, BroodID == "2018_0101_1")$NumberFledged, 4)
  #Although there were fledged chicks, there are no measurement records in Nestling/Ringings data
  expect_equal(subset(HAR_data, BroodID == "2018_0101_1")$AvgChickMass, NA_real_)
  expect_equal(subset(HAR_data, BroodID == "2018_0101_1")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(HAR_data, BroodID == "1991_0842_1")$Species, "FICHYP")
  expect_equal(subset(HAR_data, BroodID == "1991_0842_1")$ClutchType_calculated, "replacement")
  expect_equal(subset(HAR_data, BroodID == "1991_0842_1")$LayDate, as.Date("1991-06-10"))
  expect_equal(subset(HAR_data, BroodID == "1991_0842_1")$ClutchSize, 7)
  expect_equal(subset(HAR_data, BroodID == "1991_0842_1")$BroodSize, 7)
  expect_equal(subset(HAR_data, BroodID == "1991_0842_1")$NumberFledged, 5)
  #NA because no chicks were measured between 14-16 days old
  expect_equal(subset(HAR_data, BroodID == "1991_0842_1")$AvgChickMass, NA_real_)
  expect_equal(subset(HAR_data, BroodID == "1991_0842_1")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(HAR_data, BroodID == "2018_0159_1")$Species, "PARMAJ")
  expect_equal(subset(HAR_data, BroodID == "2018_0159_1")$ClutchType_calculated, "replacement")
  expect_equal(subset(HAR_data, BroodID == "2018_0159_1")$LayDate, as.Date("2018-06-17"))
  expect_equal(subset(HAR_data, BroodID == "2018_0159_1")$ClutchSize, 8)
  expect_equal(subset(HAR_data, BroodID == "2018_0159_1")$BroodSize, 8)
  expect_equal(subset(HAR_data, BroodID == "2018_0159_1")$NumberFledged, 4)
  expect_equal(subset(HAR_data, BroodID == "2018_0159_1")$AvgChickMass, NA_real_)
  expect_equal(subset(HAR_data, BroodID == "2018_0159_1")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(HAR_data, BroodID == "1991_0113_1")$Species, "CYACAE")
  expect_equal(subset(HAR_data, BroodID == "1991_0113_1")$ClutchType_calculated, "second")
  expect_equal(subset(HAR_data, BroodID == "1991_0113_1")$LayDate, as.Date("1991-06-17"))
  expect_equal(subset(HAR_data, BroodID == "1991_0113_1")$ClutchSize, 10)
  expect_equal(subset(HAR_data, BroodID == "1991_0113_1")$BroodSize, 10)
  expect_equal(subset(HAR_data, BroodID == "1991_0113_1")$NumberFledged, 5)
  expect_equal(round(subset(HAR_data, BroodID == "1991_0113_1")$AvgChickMass, 2), 8.72)
  expect_equal(subset(HAR_data, BroodID == "1991_0113_1")$AvgTarsus, NA_real_)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only HAR data
  HAR_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "HAR")

  #Test 1: Never caught as chick
  #Individual V-675839 should be listed as a great tit female
  expect_equal(subset(HAR_data, IndvID == "V-675839")$Sex, "F")
  expect_equal(subset(HAR_data, IndvID == "V-675839")$Species, "PARMAJ")
  #They should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(HAR_data, IndvID == "V-675839")$BroodIDLaid, NA_character_)
  expect_equal(subset(HAR_data, IndvID == "V-675839")$BroodIDFledged, NA_character_)
  #Her ring season should be 1991 with a RingAge of 'adult'
  expect_equal(subset(HAR_data, IndvID == "V-675839")$RingSeason, 1991L)
  expect_equal(subset(HAR_data, IndvID == "V-675839")$RingAge, "adult")

  #Test 2: First caught as chick (individual record)
  #Individual HL-010189 should be listed as a female flycatcher
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$Sex, "F")
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$Species, "FICHYP")
  #She should have no BroodIDLaid or Fledged because this individual was caught as an adult
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$BroodIDLaid, "2005_1617_1")
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$BroodIDFledged, "2005_1617_1")
  #Her ring season should be 2003 with a RingAge of 'chick'
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$RingSeason, 2005L)
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$RingAge, "chick")

  #Test 3: First caught as chick (multi-chick record)
  expect_equal(subset(HAR_data, IndvID == "X-127548")$Sex, NA_character_)
  expect_equal(subset(HAR_data, IndvID == "X-127548")$Species, "CYACAE")
  #She should have no BroodIDLaid or Fledged because this individual was caught as an adult
  expect_equal(subset(HAR_data, IndvID == "X-127548")$BroodIDLaid, "1991_0102_1")
  expect_equal(subset(HAR_data, IndvID == "X-127548")$BroodIDFledged, "1991_0102_1")
  #Her ring season should be 2003 with a RingAge of 'chick'
  expect_equal(subset(HAR_data, IndvID == "X-127548")$RingSeason, 1991L)
  expect_equal(subset(HAR_data, IndvID == "X-127548")$RingAge, "chick")

})

test_that("Capture data returns an expected outcome...", {

  #Take a subset of only HAR data
  HAR_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "HAR")

  #Build tests for possible capture-nestling capture combos described in the help docs

  #Test 1: Capture records only as adult
  #Test for the expected number of captures
  expect_equal(nrow(subset(HAR_data, IndvID == "X-341048")), 12)
  #Test that the first capture is as expected
  expect_equal(subset(HAR_data, IndvID == "X-341048")$CaptureDate[1], as.Date("1993-6-16"))
  expect_equal(subset(HAR_data, IndvID == "X-341048")$CaptureDate[12], as.Date("1996-6-28"))
  #Test that age observed is as expected on first and 16th capture
  expect_equal(subset(HAR_data, IndvID == "X-341048")$Age_observed[1], 5L)
  expect_equal(subset(HAR_data, IndvID == "X-341048")$Age_observed[12], 4L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(HAR_data, IndvID == "X-341048")$Age_calculated[1], 4L)
  expect_equal(subset(HAR_data, IndvID == "X-341048")$Age_calculated[12], 10L)

  #Test 2: Capture records as single chick
  #Test for the expected number of captures
  expect_equal(nrow(subset(HAR_data, IndvID == "V-871926")), 5)
  #Test that the first and 5th capture is as expected
  expect_equal(subset(HAR_data, IndvID == "V-871926")$CaptureDate[1], as.Date("1990-7-4"))
  expect_equal(subset(HAR_data, IndvID == "V-871926")$CaptureDate[5], as.Date("1992-7-2"))
  #Test that age observed is as expected on first and 16th capture
  expect_equal(subset(HAR_data, IndvID == "V-871926")$Age_observed[1], 1L)
  expect_equal(subset(HAR_data, IndvID == "V-871926")$Age_observed[5], 6L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(HAR_data, IndvID == "V-871926")$Age_calculated[1], 1L)
  expect_equal(subset(HAR_data, IndvID == "V-871926")$Age_calculated[5], 7L)

  #Test 3: Chick captures with separate info in capture and nestling tables
  #Test for the expected number of captures
  expect_equal(nrow(subset(HAR_data, IndvID == "X-620030")), 2)
  #Test that the first and 5th capture is as expected
  expect_equal(subset(HAR_data, IndvID == "X-620030")$CaptureDate[1], as.Date("1995-06-29"))
  expect_equal(subset(HAR_data, IndvID == "X-620030")$CaptureDate[2], as.Date("1995-07-25"))
  #Test that age observed is as expected on first and 16th capture
  expect_equal(subset(HAR_data, IndvID == "X-620030")$Age_observed[1], 1L)
  expect_equal(subset(HAR_data, IndvID == "X-620030")$Age_observed[2], 1L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(HAR_data, IndvID == "X-620030")$Age_calculated[1], 1L)
  expect_equal(subset(HAR_data, IndvID == "X-620030")$Age_calculated[2], 1L)

  #Test 4: Individual in a multi-chick capture
  #Test for the expected number of captures
  expect_equal(nrow(subset(HAR_data, IndvID == "X-126587")), 11)
  #Test that the first capture is as expected
  expect_equal(subset(HAR_data, IndvID == "X-126587")$CaptureDate[1], as.Date("1991-06-25"))
  expect_equal(subset(HAR_data, IndvID == "X-126587")$CaptureDate[11], as.Date("1996-06-26"))
  #Test that age observed is as expected on first and 10th capture
  expect_equal(subset(HAR_data, IndvID == "X-126587")$Age_observed[1], 1L)
  expect_equal(subset(HAR_data, IndvID == "X-126587")$Age_observed[11], 6L)
  #Test that age calculated is correct on first capture and 10th capture
  expect_equal(subset(HAR_data, IndvID == "X-126587")$Age_calculated[1], 1L)
  expect_equal(subset(HAR_data, IndvID == "X-126587")$Age_calculated[11], 13L)

  #Test 5: Individual ringed as a chick (single record)
  #Test for the expected number of captures
  expect_equal(nrow(subset(HAR_data, IndvID == "HL-010189")), 3)
  #Test that the first capture is as expected
  #This should be NA because no CaptureDate was listed. However, should have correct BreedingSeason
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$CaptureDate[1], as.Date(NA))
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$BreedingSeason[1], 2005L)
  #Test that the 3rd capture is as expected
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$CaptureDate[3], as.Date("2008-06-11"))
  #Test that age observed is as expected on first capture
  #Test that age observed is as expected on 3rd capture
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$Age_observed[1], 1L)
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$Age_observed[3], 5L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$Age_calculated[1], 1L)
  expect_equal(subset(HAR_data, IndvID == "HL-010189")$Age_calculated[3], 9L)

  #Test 6: caught only as adult with 1 record
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(HAR_data, IndvID == "V-675839")), 1)
  #Test that the first capture is as expected
  expect_equal(subset(HAR_data, IndvID == "V-675839")$CaptureDate[1], as.Date("1991-05-19"))
  #Test that first and last age observed is as expected
  expect_equal(subset(HAR_data, IndvID == "V-675839")$Age_observed[1], 5L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(HAR_data, IndvID == "V-675839")$Age_calculated[1], 4L)

  #Test 7: Caught only as adult with multiple records
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(HAR_data, IndvID == "X-103679")), 9)
  #Test that the first capture is as expected
  expect_equal(subset(HAR_data, IndvID == "X-103679")$CaptureDate[1], as.Date("1992-6-3"))
  #Test that the 5th capture is as expected
  expect_equal(subset(HAR_data, IndvID == "X-103679")$CaptureDate[9], as.Date("1995-6-22"))
  #Test that first and last age observed is as expected
  expect_equal(subset(HAR_data, IndvID == "X-103679")$Age_observed[1], 6L)
  expect_equal(subset(HAR_data, IndvID == "X-103679")$Age_observed[9], 6L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(HAR_data, IndvID == "X-103679")$Age_calculated[1], 4L)
  expect_equal(subset(HAR_data, IndvID == "X-103679")$Age_calculated[9], 10L)

  #Test 8: Caught with unknown age (FL)
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(HAR_data, IndvID == "X-103582")), 1)
  #Test that the first capture is as expected
  expect_equal(subset(HAR_data, IndvID == "X-103582")$CaptureDate[1], as.Date("1991-12-25"))
  #Test that first and last age observed is as expected
  expect_equal(subset(HAR_data, IndvID == "X-103582")$Age_observed[1], 2L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(HAR_data, IndvID == "X-103582")$Age_calculated[1], 2L)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of HAR data
  HAR_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "HAR")

  #Test 1: Nestbox check
  #Location listed as a nest box that has lat/long from separate file
  #Record has expected LocationType
  expect_true(subset(HAR_data, LocationID == "0018")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(HAR_data, LocationID == "0018")$NestboxID == "0018")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(HAR_data, LocationID == "0018")$StartSeason, 1981L)
  expect_equal(subset(HAR_data, LocationID == "0018")$EndSeason, 1998L)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(HAR_data, LocationID == "0018")$PopID, "HAR")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(HAR_data, LocationID == "0018")$Latitude, 2) %>% setNames(nm = NULL), 60.67)
  expect_equal(round(subset(HAR_data, LocationID == "0018")$Longitude, 2) %>% setNames(nm = NULL), 21.92)

})
