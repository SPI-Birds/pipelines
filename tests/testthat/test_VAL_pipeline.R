context("Run data quality check on Valsein pipeline output")

test_that("VAL outputs all files...", {

  expect_true("VAL" %in% pipeline_output$Brood_data$PopID)
  expect_true("VAL" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("VAL" %in% pipeline_output$Individual_data$PopID)
  expect_true("VAL" %in% pipeline_output$Location_data$PopID)

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only VAL data
  VAL_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "VAL")

  #Test 1: Adult flycatcher female
  #Individual 332999 should be listed as a female flycatcher
  expect_equal(subset(VAL_data, IndvID == "332999")$Sex_calculated, "F")
  expect_equal(subset(VAL_data, IndvID == "332999")$Species, "FICHYP")
  #She should have no BroodIDLaid or Fledged because there is no chick info
  expect_equal(subset(VAL_data, IndvID == "332999")$BroodIDLaid, NA_character_)
  expect_equal(subset(VAL_data, IndvID == "332999")$BroodIDFledged, NA_character_)
  #Her ring season should be 2013 with a RingAge of 'adult'
  expect_equal(subset(VAL_data, IndvID == "332999")$RingSeason, 1991)
  expect_equal(subset(VAL_data, IndvID == "332999")$RingAge, "adult")

  #Test 2: Adult flycatcher male
  #Individual 735228 should be listed as a male flycatcher
  expect_equal(subset(VAL_data, IndvID == "735228")$Sex_calculated, "M")
  expect_equal(subset(VAL_data, IndvID == "735228")$Species, "FICHYP")
  #She should have same BroodIDLaid and Fledged (2016_089_05_05)
  expect_equal(subset(VAL_data, IndvID == "735228")$BroodIDLaid, NA_character_)
  expect_equal(subset(VAL_data, IndvID == "735228")$BroodIDFledged, NA_character_)
  #Her ring season should be 2016 with a RingAge of 'chick'
  expect_equal(subset(VAL_data, IndvID == "735228")$RingSeason, 1995)
  expect_equal(subset(VAL_data, IndvID == "735228")$RingAge, "adult")

  #Test 3: Chick flycatcher with adult capture
  #Individual RC0402 should be listed as a male flycatcher
  expect_equal(subset(VAL_data, IndvID == "RC0402")$Sex_calculated, "M")
  expect_equal(subset(VAL_data, IndvID == "RC0402")$Species, "FICHYP")
  #He should have same BroodIDLaid and Fledged (2016_036_11_05)
  expect_equal(subset(VAL_data, IndvID == "RC0402")$BroodIDLaid, "A023_2018")
  expect_equal(subset(VAL_data, IndvID == "RC0402")$BroodIDFledged, "A023_2018")
  #His ring season should be 2016 with a RingAge of 'chick'
  expect_equal(subset(VAL_data, IndvID == "RC0402")$RingSeason, 2018)
  expect_equal(subset(VAL_data, IndvID == "RC0402")$RingAge, "chick")

  #Test 4: Flycatcher caught only as chick
  #RC0401 was never caught as an adult
  expect_equal(subset(VAL_data, IndvID == "RC0401")$Sex_calculated, NA_character_)
  expect_equal(subset(VAL_data, IndvID == "RC0401")$Species, "FICHYP")
  #Check BroodIDLaid and Fledged are the same (1998_041_10_05)
  expect_equal(subset(VAL_data, IndvID == "RC0401")$BroodIDLaid, "A001_2018")
  expect_equal(subset(VAL_data, IndvID == "RC0401")$BroodIDFledged, "A001_2018")
  #Check RingSeason and RingAge are as expected (1998, 'chick')
  expect_equal(subset(VAL_data, IndvID == "RC0401")$RingSeason, 2018)
  expect_equal(subset(VAL_data, IndvID == "RC0401")$RingAge, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  #Take a subset of only VAL data
  VAL_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "VAL")

  #Test 1: Brood where (calculated) clutch type = first
  #From early data
  expect_equal(subset(VAL_data, BroodID == "A186_1998_1")$Species, "FICHYP")
  expect_equal(subset(VAL_data, BroodID == "A186_1998_1")$ClutchType_calculated, "first")
  expect_equal(subset(VAL_data, BroodID == "A186_1998_1")$LayDate_observed, as.Date("1998-05-18"))
  expect_equal(subset(VAL_data, BroodID == "A186_1998_1")$ClutchSize_observed, 6L)
  expect_equal(subset(VAL_data, BroodID == "A186_1998_1")$BroodSize_observed, NA_integer_)
  expect_equal(subset(VAL_data, BroodID == "A186_1998_1")$NumberFledged_observed, NA_integer_)
  expect_equal(subset(VAL_data, BroodID == "A186_1998_1")$AvgChickMass, NA_real_)
  expect_equal(subset(VAL_data, BroodID == "A186_1998_1")$AvgTarsus, NA_real_)

  #Test 2: Brood where (calculated) clutch type = replacement (due to failed nest)
  #From early data
  expect_equal(subset(VAL_data, BroodID == "A034_2000_2")$Species, "FICHYP")
  expect_equal(subset(VAL_data, BroodID == "A034_2000_2")$ClutchType_calculated, "replacement")
  expect_equal(subset(VAL_data, BroodID == "A034_2000_2")$LayDate_observed, as.Date("2000-06-10"))
  expect_equal(subset(VAL_data, BroodID == "A034_2000_2")$ClutchSize_observed, 5L)
  expect_equal(subset(VAL_data, BroodID == "A034_2000_2")$BroodSize_observed, 4L)
  expect_equal(subset(VAL_data, BroodID == "A034_2000_2")$NumberFledged_observed, 3L)
  expect_equal(round(subset(VAL_data, BroodID == "A034_2000_2")$AvgChickMass, 2), 11.75)
  expect_equal(round(subset(VAL_data, BroodID == "A034_2000_2")$AvgTarsus, 2), 16.22)

  #Test 3: Brood where (calculated) clutch type = first
  #From late data
  expect_equal(subset(VAL_data, BroodID == "A037_2017_1")$Species, "FICHYP")
  expect_equal(subset(VAL_data, BroodID == "A037_2017_1")$ClutchType_calculated, "first")
  expect_equal(subset(VAL_data, BroodID == "A037_2017_1")$LayDate_observed, as.Date("2017-05-25"))
  expect_equal(subset(VAL_data, BroodID == "A037_2017_1")$ClutchSize_observed, 1L)
  expect_equal(subset(VAL_data, BroodID == "A037_2017_1")$BroodSize_observed, NA_integer_)
  expect_equal(subset(VAL_data, BroodID == "A037_2017_1")$NumberFledged_observed, NA_integer_)
  expect_equal(subset(VAL_data, BroodID == "A037_2017_1")$AvgChickMass, NA_real_)
  expect_equal(subset(VAL_data, BroodID == "A037_2017_1")$AvgTarsus, NA_real_)

})

test_that("Capture_data returns an expected outcome...", {

  #Take a subset of only VAL data
  VAL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "VAL")

  #Test 1: Caught as chick first
  expect_equal(nrow(subset(VAL_data, IndvID == "RC0402")), 3L)
  expect_equal(min(subset(VAL_data, IndvID == "RC0402")$CaptureDate, na.rm = TRUE), as.Date("2018-06-17"))
  expect_equal(subset(VAL_data, IndvID == "RC0402")$CaptureDate[2], as.Date("2019-06-23"))
  expect_equal(subset(VAL_data, IndvID == "RC0402")$Age_observed[1], 1L)
  expect_equal(subset(VAL_data, IndvID == "RC0402")$Age_calculated[1], 1L)
  expect_equal(subset(VAL_data, IndvID == "RC0402")$Age_calculated[2], 5L)
  expect_equal(subset(VAL_data, IndvID == "RC0402")$Age_calculated[3], 7L)

  #Test 2: Never caught as chick
  expect_equal(nrow(subset(VAL_data, IndvID == "AS7967")), 4L)
  expect_equal(min(subset(VAL_data, IndvID == "AS7967")$CaptureDate, na.rm = TRUE), as.Date("2016-06-15"))
  expect_equal(subset(VAL_data, IndvID == "AS7967")$CaptureDate[2], as.Date("2017-06-08"))
  expect_equal(subset(VAL_data, IndvID == "AS7967")$Age_observed[1], 6L)
  expect_equal(subset(VAL_data, IndvID == "AS7967")$Age_calculated[1], 4L)
  expect_equal(subset(VAL_data, IndvID == "AS7967")$Age_calculated[4], 10L)

  #Test 3: Never caught as chick
  expect_equal(nrow(subset(VAL_data, IndvID == "CG5103")), 3L)
  expect_equal(min(subset(VAL_data, IndvID == "CG5103")$CaptureDate, na.rm = TRUE), as.Date("2014-06-07"))
  expect_equal(subset(VAL_data, IndvID == "CG5103")$CaptureDate[2], as.Date("2015-06-12"))
  expect_equal(subset(VAL_data, IndvID == "CG5103")$Age_observed[1], 4L)
  expect_equal(subset(VAL_data, IndvID == "CG5103")$Age_calculated[1], 4L)
  expect_equal(subset(VAL_data, IndvID == "CG5103")$Age_calculated[3], 8L)

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only VAL data
  VAL_data <- dplyr::filter(pipeline_output$Location_data, PopID == "VAL")

  #Test 1: Nest no longer in use
  expect_equal(subset(VAL_data, LocationID == "A001")$StartSeason, 1991)
  expect_equal(subset(VAL_data, LocationID == "A001")$EndSeason, NA_integer_)
  expect_equal(round(subset(VAL_data, LocationID == "A001")$Latitude, 2), 40.89)
  expect_equal(round(subset(VAL_data, LocationID == "A001")$Longitude, 2), -4.03)

  #Test 2: Nest still in use
  expect_equal(subset(VAL_data, LocationID == "B001")$StartSeason, 2016)
  expect_equal(subset(VAL_data, LocationID == "B001")$EndSeason, NA_integer_)
  expect_equal(round(subset(VAL_data, LocationID == "B001")$Latitude, 2), 40.86)
  expect_equal(round(subset(VAL_data, LocationID == "B001")$Longitude, 2), -4.06)

})
