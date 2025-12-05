pipeline_output <- format_CHO(db = paste0(data_path, "/CHO_Choupal_Portugal"))

test_that("CHO outputs all files...", {
  expect_true("CHO" %in% pipeline_output$Brood_data$PopID)
  expect_true("CHO" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("CHO" %in% pipeline_output$Individual_data$PopID)
  expect_true("CHO" %in% pipeline_output$Location_data$PopID)
  expect_true(pipeline_output$protocol_version == "1.1.0")
})

test_that("Individual data returns an expected outcome...", {
  # We want to run a test for each sex for adults and chicks

  # Take a subset of only CHO data
  CHO_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "CHO")

  # Test 1: Adult great tit female
  # Individual C044309 should be listed as a female great tit
  expect_equal(subset(CHO_data, IndvID == "C044309")$Sex_calculated, "F")
  expect_equal(subset(CHO_data, IndvID == "C044309")$Species, "PARMAJ")
  # She should have no BroodIDLaid or Fledged because this individual was caught as an adult
  expect_equal(subset(CHO_data, IndvID == "C044309")$BroodIDLaid, NA_character_)
  expect_equal(subset(CHO_data, IndvID == "C044309")$BroodIDFledged, NA_character_)
  # Her ring season should be 2003 with a RingAge of 'adult'
  expect_equal(subset(CHO_data, IndvID == "C044309")$RingSeason, 2003)
  expect_equal(subset(CHO_data, IndvID == "C044309")$RingAge, "adult")

  # Test 2: Adult great tit male
  # Individual C029843 should be listed as a male great tit
  expect_equal(subset(CHO_data, IndvID == "C029843")$Sex_calculated, "M")
  expect_equal(subset(CHO_data, IndvID == "C029843")$Species, "PARMAJ")
  # She should have no BroodIDLaid or Fledged because this individual was caught as an adult
  expect_equal(subset(CHO_data, IndvID == "C029843")$BroodIDLaid, NA_character_)
  expect_equal(subset(CHO_data, IndvID == "C029843")$BroodIDFledged, NA_character_)
  # Her ring season should be 2003 with a RingAge of 'chick'
  expect_equal(subset(CHO_data, IndvID == "C029843")$RingSeason, 2003)
  expect_equal(subset(CHO_data, IndvID == "C029843")$RingAge, "adult")

  # Test 3: Caught as chick
  # Individual C048092 should be listed as a female blue tit
  expect_equal(subset(CHO_data, IndvID == "C048092")$Sex_calculated, "F")
  expect_equal(subset(CHO_data, IndvID == "C048092")$Species, "PARMAJ")
  # Should have BroodID Laid and Fledged of 2005_81
  expect_equal(subset(CHO_data, IndvID == "C048092")$BroodIDLaid, "2005_081")
  expect_equal(subset(CHO_data, IndvID == "C048092")$BroodIDFledged, "2005_081")
  # Ring season should be 2005 with a RingAge of 'chick'
  expect_equal(subset(CHO_data, IndvID == "C048092")$RingSeason, 2005)
  expect_equal(subset(CHO_data, IndvID == "C048092")$RingAge, "chick")
})

test_that("Brood_data returns an expected outcome...", {
  # We want to run tests for all possible outcomes of ClutchType_calculated

  # Take a subset of only CHO data
  CHO_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "CHO")

  # Test 1: Brood clutch type = first
  # BroodID 2005_021 should be PARMAJ
  expect_equal(subset(CHO_data, BroodID == "2005_021")$Species, "PARMAJ")
  # BroodID 2005_021 should have clutch type calculated 'first'
  expect_equal(subset(CHO_data, BroodID == "2005_021")$ClutchType_calculated, "first")
  # Laying date should be "2005-04-06"
  expect_equal(subset(CHO_data, BroodID == "2005_021")$LayDate_observed, as.Date("2005-04-06"))
  # Clutch size should be 6, BroodSize should be 6, NumberFledged should be 6
  expect_equal(subset(CHO_data, BroodID == "2005_021")$ClutchSize_observed, 6)
  expect_equal(subset(CHO_data, BroodID == "2005_021")$BroodSize_observed, 6)
  expect_equal(subset(CHO_data, BroodID == "2005_021")$NumberFledged_observed, 6)
  # AvgChickMass and AvgTarsus should be NA, there were no chicks
  expect_equal(round(subset(CHO_data, BroodID == "2005_021")$AvgChickMass, 1), 15.2)
  expect_equal(round(subset(CHO_data, BroodID == "2005_021")$AvgTarsus, 1), 17.4)
  # AvgEggMass should be 1.4
  expect_equal(round(subset(CHO_data, BroodID == "2005_021")$AvgEggMass, 1), 1.4)

  # Test 2: Brood clutch type = second
  # BroodID 2005_21b should be PARMAJ
  expect_equal(subset(CHO_data, BroodID == "2005_006")$Species, "PARMAJ")
  # BroodID 2005_21b should have clutch type calculated 'second' (clutch tested above was successful)
  expect_equal(subset(CHO_data, BroodID == "2005_006")$ClutchType_calculated, "second")
  # Laying date should be 2005-05-17
  expect_equal(subset(CHO_data, BroodID == "2005_006")$LayDate_observed, as.Date("2005-05-17"))
  # Clutch size should be 4, BroodSize should be 2, NumberFledged should be 2
  expect_equal(subset(CHO_data, BroodID == "2005_006")$ClutchSize_observed, 4)
  expect_equal(subset(CHO_data, BroodID == "2005_006")$BroodSize_observed, 2)
  expect_equal(subset(CHO_data, BroodID == "2005_006")$NumberFledged_observed, 2)
  # AvgChickMass and AvgTarsus as expected
  expect_equal(round(subset(CHO_data, BroodID == "2005_006")$AvgChickMass, 1), 16.4)
  expect_equal(round(subset(CHO_data, BroodID == "2005_006")$AvgTarsus, 1), 18.9)

  # Test 3: Great tit brood clutch type = replacement, where replacement is known (i.e. previous clutch was seen)
  # BroodID 2003_004 should be PARMAJ
  expect_equal(subset(CHO_data, BroodID == "2003_004")$Species, "PARMAJ")
  # BroodID 2005_004 should have clutch type calculated 'replacement' (clutch 2015_BC_026_24_04 had no fledlings)
  expect_equal(subset(CHO_data, BroodID == "2003_004")$ClutchType_calculated, "replacement")
  # Laying date should be "2003-06-05"
  expect_equal(subset(CHO_data, BroodID == "2003_004")$LayDate_observed, as.Date("2003-06-05"))
  # Clutch size should be 4, BroodSize should be 2, NumberFledged should be 2
  expect_equal(subset(CHO_data, BroodID == "2003_004")$ClutchSize_observed, 4)
  expect_equal(subset(CHO_data, BroodID == "2003_004")$BroodSize_observed, 2)
  expect_equal(subset(CHO_data, BroodID == "2003_004")$NumberFledged_observed, 2)
  # AvgChickMass and AvgTarsus should be NA, there were no chicks
  expect_equal(round(subset(CHO_data, BroodID == "2003_004")$AvgChickMass, 1), 18.9)
  expect_equal(round(subset(CHO_data, BroodID == "2003_004")$AvgTarsus, 1), 18.8)
  # AvgEggMass should be NA
  expect_equal(subset(CHO_data, BroodID == "2003_004")$AvgEggMass, NA_real_)

  # Test 4: Brood clutch type = replacement, where replacement calculated from cutoff
  # BroodID 2003_012 should be PARMAJ
  expect_equal(subset(CHO_data, BroodID == "2003_012")$Species, "PARMAJ")
  # BroodID 2005_012 should have clutch type calculated 'replacement' (laying date is > cutoff)
  expect_equal(subset(CHO_data, BroodID == "2003_012")$ClutchType_calculated, "replacement")
  # Laying date should be "2003-06-01"
  expect_equal(subset(CHO_data, BroodID == "2003_012")$LayDate_observed, as.Date("2003-06-01"))
  # Clutch size should be 5, BroodSize should be 4, NumberFledged should be 4
  expect_equal(subset(CHO_data, BroodID == "2003_012")$ClutchSize_observed, 5)
  expect_equal(subset(CHO_data, BroodID == "2003_012")$BroodSize_observed, 4)
  expect_equal(subset(CHO_data, BroodID == "2003_012")$NumberFledged_observed, 4)
  # AvgChickMass and AvgTarsus should be NA, chicks were measured >16 days old
  expect_equal(subset(CHO_data, BroodID == "2003_012")$AvgChickMass, NA_real_)
  expect_equal(subset(CHO_data, BroodID == "2003_012")$AvgTarsus, NA_real_)
  # AvgEggMass should be NA
  expect_equal(subset(CHO_data, BroodID == "2003_012")$AvgEggMass, NA_real_)
})

test_that("Capture_data returns an expected outcome...", {
  # We want to run tests for captures as both chicks, males, and females

  # Take a subset of only CHO data
  CHO_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "CHO")

  # Test 1: Female caught as adult
  # Test the female has the correct number of capture records (5)
  expect_equal(nrow(subset(CHO_data, IndvID == "C044309")), 5)
  # Test that the first capture of the female is as expected (2003-05-21)
  expect_equal(min(subset(CHO_data, IndvID == "C044309")$CaptureDate, na.rm = TRUE), as.Date("2003-05-21"))
  # Test that the 5th capture of the female is as expcted (2006-06-24)
  expect_equal(subset(CHO_data, IndvID == "C044309")$CaptureDate[5], as.Date("2006-06-24"))
  # Test that age observed is as expected (5, because it's listed as 'first year' i.e. first year breeder)
  expect_equal(subset(CHO_data, IndvID == "C044309")$Age_observed[1], 5L)
  # Test that age calculated is correct on first capture (4 because it's an adult of unknown age because it wasn't caught as a chick)
  expect_equal(subset(CHO_data, IndvID == "C044309")$Age_calculated[1], 4L)
  # Test that age calculated is correct on 5th capture (4 + 3*2 = 10 because it's an adult caught 3 years after its first capture)
  expect_equal(subset(CHO_data, IndvID == "C044309")$Age_calculated[5], 10L)

  # Test 2: Male caught as adult
  # Test the male has the correct number of capture records (2)
  expect_equal(nrow(subset(CHO_data, IndvID == "C029843")), 2)
  # Test that the first capture of the male is as expected (2013-06-02)
  expect_equal(min(subset(CHO_data, IndvID == "C029843")$CaptureDate, na.rm = TRUE), as.Date("2003-04-09"))
  # Test that the 2nd capture of the male is as expcted (2003-10-17)
  expect_equal(subset(CHO_data, IndvID == "C029843")$CaptureDate[2], as.Date("2003-10-17"))
  # Test that age observed is as expected on first record (5, because it's listed as 'first year' i.e. first year of breeding)
  expect_equal(subset(CHO_data, IndvID == "C029843")$Age_observed[1], 5L)
  # Test that age observed is as expected on second record (4, because it's as 'adult' i.e. no information on age)
  expect_equal(subset(CHO_data, IndvID == "C029843")$Age_observed[2], 4L)
  # Test that age calculated is correct on first capture (4 because it's an adult not caught as chick)
  expect_equal(subset(CHO_data, IndvID == "C029843")$Age_calculated[1], 4L)
  # Test that age calculated is correct on second capture (4 because it's the same year)
  expect_equal(subset(CHO_data, IndvID == "C029843")$Age_calculated[2], 4L)

  # Test 3: Caught first as chick
  # Test the male has the correct number of capture records (4)
  expect_equal(nrow(subset(CHO_data, IndvID == "C048092")), 4)
  # Test that the first capture of the male is as expected (2005-06-03)
  expect_equal(min(subset(CHO_data, IndvID == "C048092")$CaptureDate, na.rm = TRUE), as.Date("2005-06-03"))
  # Test that the 4th capture of the male is as expected (2006-07-01)
  expect_equal(subset(CHO_data, IndvID == "C048092")$CaptureDate[4], as.Date("2006-07-01"))
  # Test that age observed is as expected on first record (1, because it's caught as chick)
  expect_equal(subset(CHO_data, IndvID == "C048092")$Age_observed[1], 1L)
  # Test that age observed is as expected on last record (5, because it's as 'first year' i.e. first year of breeding)
  expect_equal(subset(CHO_data, IndvID == "C048092")$Age_observed[4], 5L)
  # Test that age calculated is correct on first capture (1 because it's a chick)
  expect_equal(subset(CHO_data, IndvID == "C048092")$Age_calculated[1], 1L)
  # Test that age calculated is correct on second capture (3 because it was caught as chick in this year)
  expect_equal(subset(CHO_data, IndvID == "C048092")$Age_calculated[2], 3L)
  # Test that age calculated is correct on 4th capture (5 because it was caught as chick in the previous year)
  expect_equal(subset(CHO_data, IndvID == "C048092")$Age_calculated[4], 5L)
})

test_that("Location_data returns an expected outcome...", {
  # We want to run tests for both nest box and mistnet locations

  # Take a subset of only CHO data
  CHO_data <- dplyr::filter(pipeline_output$Location_data, PopID == "CHO")

  # Test 1: Nestbox check
  # Nestbox 7 should be type "NB"
  expect_equal(subset(CHO_data, LocationID == "007")$LocationType, "NB")
  # Nest boxes are not moved during the study, so LocationID and NestboxID should be identical
  expect_equal(
    subset(CHO_data, LocationID == "007")$LocationID,
    subset(CHO_data, LocationID == "007")$NestboxID
  )

  # Test 1: Mistnet check
  # There should be a mistnet location called "MN1" (no exact mist net data provided)
  expect_equal(subset(CHO_data, LocationID == "MN1")$LocationType, "MN")
})

## Test protocol compliance
test_protocol_compliance(pipeline_output)
