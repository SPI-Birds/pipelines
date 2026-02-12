pipeline_output <- format_KEV(db = paste0(data_path, "/KEV_Kevo_Finland"))

test_that("KEV outputs all files...", {
  expect_true("KEV" %in% pipeline_output$Brood_data$PopID)
  expect_true("KEV" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("KEV" %in% pipeline_output$Individual_data$PopID)
  expect_true("KEV" %in% pipeline_output$Location_data$PopID)
  expect_true(pipeline_output$protocol_version == "1.0.0")
})

test_that("Brood_data returns an expected outcome...", {
  # We want to run tests for all possible outcomes of ClutchType_calculated

  # Take a subset of only KEV data
  KEV_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "KEV")

  # There is no femaleID info in the data, so clutches could only be classified as first or replacement

  # Test 1: Brood where clutch type = first
  expect_equal(subset(KEV_data, BroodID == "1984_0007_1")$Species, "PARMAJ")
  expect_equal(subset(KEV_data, BroodID == "1984_0007_1")$ClutchType_calculated, "first")
  expect_equal(subset(KEV_data, BroodID == "1984_0007_1")$LayingDate, as.Date("1984-5-21"))
  expect_equal(subset(KEV_data, BroodID == "1984_0007_1")$ClutchSize, 10)
  expect_equal(subset(KEV_data, BroodID == "1984_0007_1")$BroodSize, 7)
  expect_equal(subset(KEV_data, BroodID == "1984_0007_1")$NumberFledged, 7)
  expect_equal(subset(KEV_data, BroodID == "1984_0007_1")$AvgChickMass, NA_real_)
  expect_equal(subset(KEV_data, BroodID == "1984_0007_1")$AvgTarsus, NA_real_)

  # Test 2: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(KEV_data, BroodID == "1984_0111_1")$Species, "PARMAJ")
  expect_equal(subset(KEV_data, BroodID == "1984_0111_1")$ClutchType_calculated, "replacement")
  expect_equal(subset(KEV_data, BroodID == "1984_0111_1")$LayingDate, as.Date("1984-7-5"))
  expect_equal(subset(KEV_data, BroodID == "1984_0111_1")$ClutchSize, NA_integer_)
  expect_equal(subset(KEV_data, BroodID == "1984_0111_1")$BroodSize, 6)
  expect_equal(subset(KEV_data, BroodID == "1984_0111_1")$NumberFledged, 6)
  expect_equal(subset(KEV_data, BroodID == "1984_0111_1")$AvgChickMass, NA_real_)
  expect_equal(subset(KEV_data, BroodID == "1984_0111_1")$AvgTarsus, NA_real_)
})

test_that("Individual data returns an expected outcome...", {
  # We want to run a test for each sex for individuals caught as adults and chicks

  # Take a subset of only KEV data
  KEV_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "KEV")

  # Test 1: Never caught as chick
  # Individual V-20856 should be listed as a great tit female
  expect_equal(subset(KEV_data, IndvID == "V-91428")$Sex, "F")
  expect_equal(subset(KEV_data, IndvID == "V-91428")$Species, "FICHYP")
  # They should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(KEV_data, IndvID == "V-91428")$BroodIDLaid, NA_character_)
  expect_equal(subset(KEV_data, IndvID == "V-91428")$BroodIDFledged, NA_character_)
  # Her ring season should be 1991 with a RingAge of 'adult'
  expect_equal(subset(KEV_data, IndvID == "V-91428")$RingSeason, 1984L)
  expect_equal(subset(KEV_data, IndvID == "V-91428")$RingAge, "adult")

  # Test 2: First caught as chick
  # Individual HL-010189 should be listed as a female flycatcher
  expect_equal(subset(KEV_data, IndvID == "V-815188")$Sex, "F")
  expect_equal(subset(KEV_data, IndvID == "V-815188")$Species, "FICHYP")
  expect_equal(subset(KEV_data, IndvID == "V-815188")$BroodIDLaid, "1989_1291_1")
  expect_equal(subset(KEV_data, IndvID == "V-815188")$BroodIDFledged, "1989_1291_1")
  expect_equal(subset(KEV_data, IndvID == "V-815188")$RingSeason, 1989L)
  expect_equal(subset(KEV_data, IndvID == "V-815188")$RingAge, "chick")
})

test_that("Capture data returns an expected outcome...", {
  # We want to run tests for captures as both chicks, males, and females
  # Currently we have no chick data, so we can only test adults

  # Take a subset of only KEV data
  KEV_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "KEV")

  # Test 1: Capture records only as adult
  # Test for the expected number of captures
  expect_equal(nrow(subset(KEV_data, IndvID == "V-20856")), 4)
  # Test that the first capture is as expected
  expect_equal(subset(KEV_data, IndvID == "V-20856")$CaptureDate[1], as.Date("1983-6-15"))
  expect_equal(subset(KEV_data, IndvID == "V-20856")$CaptureDate[4], as.Date("1987-6-1"))
  # Test that age observed is as expected on first and 4th capture
  expect_equal(subset(KEV_data, IndvID == "V-20856")$Age_observed[1], 4L)
  expect_equal(subset(KEV_data, IndvID == "V-20856")$Age_observed[4], 4L)
  # Test that age calculated is correct on first capture and last capture
  expect_equal(subset(KEV_data, IndvID == "V-20856")$Age_calculated[1], 4L)
  expect_equal(subset(KEV_data, IndvID == "V-20856")$Age_calculated[4], 12L)

  # Test 2: Capture records as single chick
  # Test for the expected number of captures
  expect_equal(nrow(subset(KEV_data, IndvID == "VL-501786")), 2)
  # Test that the first and 5th capture is as expected
  expect_equal(subset(KEV_data, IndvID == "VL-501786")$CaptureDate[1], as.Date("2013-6-24"))
  expect_equal(subset(KEV_data, IndvID == "VL-501786")$CaptureDate[2], as.Date("2016-5-11"))
  # Test that age observed is as expected on first and 2nd capture
  expect_equal(subset(KEV_data, IndvID == "VL-501786")$Age_observed[1], 1L)
  expect_equal(subset(KEV_data, IndvID == "VL-501786")$Age_observed[2], 4L)
  # Test that age calculated is correct on first capture and last capture
  expect_equal(subset(KEV_data, IndvID == "VL-501786")$Age_calculated[1], 1L)
  expect_equal(subset(KEV_data, IndvID == "VL-501786")$Age_calculated[2], 9L)

  # Test 3: Chick captures with separate info in capture and nestling tables
  # Test for the expected number of captures
  expect_equal(nrow(subset(KEV_data, IndvID == "V-478577")), 2)
  # Test that the first and 5th capture is as expected
  expect_equal(subset(KEV_data, IndvID == "V-478577")$CaptureDate[1], as.Date("1986-07-03"))
  expect_equal(subset(KEV_data, IndvID == "V-478577")$CaptureDate[2], as.Date("1986-07-08"))
  # Test that age observed is as expected on first and 16th capture
  expect_equal(subset(KEV_data, IndvID == "V-478577")$Age_observed[1], 1L)
  expect_equal(subset(KEV_data, IndvID == "V-478577")$Age_observed[2], 1L)
  # Test that age calculated is correct on first capture and last capture
  expect_equal(subset(KEV_data, IndvID == "V-478577")$Age_calculated[1], 1L)
  expect_equal(subset(KEV_data, IndvID == "V-478577")$Age_calculated[2], 1L)

  # Test 4: Individual in a multi-chick capture
  # Test for the expected number of captures
  expect_equal(nrow(subset(KEV_data, IndvID == "V-20847")), 2)
  # Test that the first capture is as expected
  expect_equal(subset(KEV_data, IndvID == "V-20847")$CaptureDate[1], as.Date("1983-6-14"))
  expect_equal(subset(KEV_data, IndvID == "V-20847")$CaptureDate[2], as.Date("1984-6-22"))
  # Test that age observed is as expected
  expect_equal(subset(KEV_data, IndvID == "V-20847")$Age_observed[1], 1L)
  expect_equal(subset(KEV_data, IndvID == "V-20847")$Age_observed[2], 4L)
  # Test that age calculated is as expected
  expect_equal(subset(KEV_data, IndvID == "V-20847")$Age_calculated[1], 1L)
  expect_equal(subset(KEV_data, IndvID == "V-20847")$Age_calculated[2], 5L)

  # Test 5: Individual ringed as a chick
  # Test for the expected number of captures
  expect_equal(nrow(subset(KEV_data, IndvID == "V-815188")), 3)
  # Test that the first capture is as expected
  expect_equal(subset(KEV_data, IndvID == "V-815188")$CaptureDate[1], as.Date("1989-07-03"))
  expect_equal(subset(KEV_data, IndvID == "V-815188")$BreedingSeason[1], 1989L)
  # Test that the 3rd capture is as expected
  expect_equal(subset(KEV_data, IndvID == "V-815188")$CaptureDate[3], as.Date("1990-06-18"))
  # Test that age observed is as expected on first capture
  # Test that age observed is as expected on 3rd capture
  expect_equal(subset(KEV_data, IndvID == "V-815188")$Age_observed[1], 1L)
  expect_equal(subset(KEV_data, IndvID == "V-815188")$Age_observed[3], 4L)
  # Test that age calculated is correct on first capture and last capture
  expect_equal(subset(KEV_data, IndvID == "V-815188")$Age_calculated[1], 1L)
  expect_equal(subset(KEV_data, IndvID == "V-815188")$Age_calculated[3], 5L)

  # Test 6: Caught only as adult with multiple records
  # Test it has the correct number of capture records
  expect_equal(nrow(subset(KEV_data, IndvID == "V-91428")), 3)
  # Test that the first capture is as expected
  expect_equal(subset(KEV_data, IndvID == "V-91428")$CaptureDate[1], as.Date("1984-6-15"))
  # Test that the 5th capture is as expected
  expect_equal(subset(KEV_data, IndvID == "V-91428")$CaptureDate[3], as.Date("1988-6-20"))
  # Test that first and last age observed is as expected
  expect_equal(subset(KEV_data, IndvID == "V-91428")$Age_observed[1], 4L)
  expect_equal(subset(KEV_data, IndvID == "V-91428")$Age_observed[3], 6L)
  # Test that first and last age calculated is as expected
  expect_equal(subset(KEV_data, IndvID == "V-91428")$Age_calculated[1], 4L)
  expect_equal(subset(KEV_data, IndvID == "V-91428")$Age_calculated[3], 12L)
})

test_that("Location_data returns an expected outcome...", {
  # We want to run tests for nest boxes (there are no mistnets)

  # Take a subset of KEV data
  KEV_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "KEV")

  # Test 1: Nestbox check
  # Location listed as a nest box that has lat/long from separate file
  # Record has expected LocationType
  expect_true(subset(KEV_data, LocationID == "0001")$LocationType == "NB")
  # Expect LocationID and NestboxID are the same
  expect_true(subset(KEV_data, LocationID == "0001")$NestboxID == "0001")
  # Expect Start and EndSeason is as expected
  expect_equal(subset(KEV_data, LocationID == "0001")$StartSeason, 1981L)
  expect_equal(subset(KEV_data, LocationID == "0001")$EndSeason, 2023L)
  # Check that LocationID is in the expected PopID
  expect_equal(subset(KEV_data, LocationID == "0001")$PopID, "KEV")
  # Check that latitude and longitude are as expected
  expect_equal(round(subset(KEV_data, LocationID == "0001")$Latitude, 2) %>% setNames(nm = NULL), 69.76)
  expect_equal(round(subset(KEV_data, LocationID == "0001")$Longitude, 2) %>% setNames(nm = NULL), 27.01)
})

## Test protocol compliance
test_protocol_compliance(pipeline_output)
