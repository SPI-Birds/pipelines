testthat::skip_if(!exists("data_path"))

pipeline_output <- format_VAL(db = paste0(data_path, "/VAL_Valsain_Spain"))


testthat::test_that("VAL outputs all files...", {

  testthat::expect_true("VAL" %in% pipeline_output$Brood_data$PopID)
  testthat::expect_true("VAL" %in% pipeline_output$Capture_data$CapturePopID)
  testthat::expect_true("VAL" %in% pipeline_output$Individual_data$PopID)
  testthat::expect_true("VAL" %in% pipeline_output$Location_data$PopID)

})

testthat::test_that("Individual data returns an expected outcome...", {

  #Take a subset of only VAL data
  VAL_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "VAL")

  #Test 1: Adult flycatcher female
  #Individual 332999 should be listed as a female flycatcher
  testthat::expect_equal(subset(VAL_data, IndvID == "332999")$Sex_calculated, "F")
  testthat::expect_equal(subset(VAL_data, IndvID == "332999")$Species, "FICHYP")
  #She should have no BroodIDLaid or Fledged because there is no chick info
  testthat::expect_equal(subset(VAL_data, IndvID == "332999")$BroodIDLaid, NA_character_)
  testthat::expect_equal(subset(VAL_data, IndvID == "332999")$BroodIDFledged, NA_character_)
  #Her ring season should be 2013 with a RingAge of 'adult'
  testthat::expect_equal(subset(VAL_data, IndvID == "332999")$RingSeason, 1991)
  testthat::expect_equal(subset(VAL_data, IndvID == "332999")$RingAge, "adult")

  #Test 2: Adult flycatcher male
  #Individual 735228 should be listed as a male flycatcher
  testthat::expect_equal(subset(VAL_data, IndvID == "735228")$Sex_calculated, "M")
  testthat::expect_equal(subset(VAL_data, IndvID == "735228")$Species, "FICHYP")
  #She should have same BroodIDLaid and Fledged (2016_089_05_05)
  testthat::expect_equal(subset(VAL_data, IndvID == "735228")$BroodIDLaid, NA_character_)
  testthat::expect_equal(subset(VAL_data, IndvID == "735228")$BroodIDFledged, NA_character_)
  #Her ring season should be 2016 with a RingAge of 'chick'
  testthat::expect_equal(subset(VAL_data, IndvID == "735228")$RingSeason, 1995)
  testthat::expect_equal(subset(VAL_data, IndvID == "735228")$RingAge, "adult")

  #Test 3: Chick flycatcher with adult capture
  #Individual RC0402 should be listed as a male flycatcher
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$Sex_calculated, "M")
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$Species, "FICHYP")
  #He should have same BroodIDLaid and Fledged (2016_036_11_05)
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$BroodIDLaid, "VAL023_2018")
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$BroodIDFledged, "VAL023_2018")
  #His ring season should be 2016 with a RingAge of 'chick'
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$RingSeason, 2018)
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$RingAge, "chick")

  #Test 4: Flycatcher caught only as chick
  #RC0401 was never caught as an adult
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0401")$Sex_calculated, NA_character_)
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0401")$Species, "FICHYP")
  #Check BroodIDLaid and Fledged are the same (1998_041_10_05)
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0401")$BroodIDLaid, "VAL001_2018")
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0401")$BroodIDFledged, "VAL001_2018")
  #Check RingSeason and RingAge are as expected (1998, 'chick')
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0401")$RingSeason, 2018)
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0401")$RingAge, "chick")

})

testthat::test_that("Brood_data returns an expected outcome...", {

  #Take a subset of only VAL data
  VAL_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "VAL")

  #Test 1: Brood where (calculated) clutch type = first
  #From early data
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL186_1998_1")$Species, "FICHYP")
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL186_1998_1")$ClutchType_calculated, "first")
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL186_1998_1")$LayDate_observed, as.Date("1998-05-18"))
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL186_1998_1")$ClutchSize_observed, 6L)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL186_1998_1")$BroodSize_observed, NA_integer_)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL186_1998_1")$NumberFledged_observed, NA_integer_)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL186_1998_1")$AvgChickMass, NA_real_)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL186_1998_1")$AvgTarsus, NA_real_)

  #Test 2: Brood where (calculated) clutch type = replacement (due to failed nest)
  #From early data
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL034_2000_2")$Species, "FICHYP")
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL034_2000_2")$ClutchType_calculated, "replacement")
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL034_2000_2")$LayDate_observed, as.Date("2000-06-10"))
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL034_2000_2")$ClutchSize_observed, 5L)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL034_2000_2")$BroodSize_observed, 4L)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL034_2000_2")$NumberFledged_observed, 3L)
  testthat::expect_equal(round(subset(VAL_data, BroodID == "VAL034_2000_2")$AvgChickMass, 2), 11.75)
  testthat::expect_equal(round(subset(VAL_data, BroodID == "VAL034_2000_2")$AvgTarsus, 2), 16.22)

  #Test 3: Brood where (calculated) clutch type = first
  #From late data
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL037_2017_1")$Species, "FICHYP")
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL037_2017_1")$ClutchType_calculated, "first")
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL037_2017_1")$LayDate_observed, as.Date("2017-05-25"))
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL037_2017_1")$ClutchSize_observed, 1L)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL037_2017_1")$BroodSize_observed, NA_integer_)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL037_2017_1")$NumberFledged_observed, NA_integer_)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL037_2017_1")$AvgChickMass, NA_real_)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL037_2017_1")$AvgTarsus, NA_real_)

  #Test 4: Brood where (observed) clutch type = replacement (i.e. nest is 'bis')
  #From late data
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL133_2012_1")$Species, "FICHYP")
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL133_2012_1")$ClutchType_observed, "replacement")
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL133_2012_1")$LayDate_observed, as.Date("2012-05-26"))
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL133_2012_1")$ClutchSize_observed, 5L)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL133_2012_1")$BroodSize_observed, 5L)
  testthat::expect_equal(subset(VAL_data, BroodID == "VAL133_2012_1")$NumberFledged_observed, 4L)
  testthat::expect_equal(round(subset(VAL_data, BroodID == "VAL133_2012_1")$AvgChickMass, 2), 11.31)
  testthat::expect_equal(round(subset(VAL_data, BroodID == "VAL133_2012_1")$AvgTarsus, 2), 16.08)

})

testthat::test_that("Capture_data returns an expected outcome...", {

  #Take a subset of only VAL data
  VAL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "VAL")

  #Test 1: Caught as chick first
  testthat::expect_equal(nrow(subset(VAL_data, IndvID == "RC0402")), 3L)
  testthat::expect_equal(min(subset(VAL_data, IndvID == "RC0402")$CaptureDate, na.rm = TRUE), as.Date("2018-06-17"))
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$CaptureDate[2], as.Date("2019-06-23"))
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$Age_observed[1], 1L)
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$Age_calculated[1], 1L)
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$Age_calculated[2], 5L)
  testthat::expect_equal(subset(VAL_data, IndvID == "RC0402")$Age_calculated[3], 7L)

  #Test 2: Never caught as chick
  testthat::expect_equal(nrow(subset(VAL_data, IndvID == "AS7967")), 4L)
  testthat::expect_equal(min(subset(VAL_data, IndvID == "AS7967")$CaptureDate, na.rm = TRUE), as.Date("2016-06-15"))
  testthat::expect_equal(subset(VAL_data, IndvID == "AS7967")$CaptureDate[2], as.Date("2017-06-08"))
  testthat::expect_equal(subset(VAL_data, IndvID == "AS7967")$Age_observed[1], 6L)
  testthat::expect_equal(subset(VAL_data, IndvID == "AS7967")$Age_calculated[1], 4L)
  testthat::expect_equal(subset(VAL_data, IndvID == "AS7967")$Age_calculated[4], 10L)

  #Test 3: Never caught as chick
  testthat::expect_equal(nrow(subset(VAL_data, IndvID == "CG5103")), 3L)
  testthat::expect_equal(min(subset(VAL_data, IndvID == "CG5103")$CaptureDate, na.rm = TRUE), as.Date("2014-06-07"))
  testthat::expect_equal(subset(VAL_data, IndvID == "CG5103")$CaptureDate[2], as.Date("2015-06-12"))
  testthat::expect_equal(subset(VAL_data, IndvID == "CG5103")$Age_observed[1], 4L)
  testthat::expect_equal(subset(VAL_data, IndvID == "CG5103")$Age_calculated[1], 4L)
  testthat::expect_equal(subset(VAL_data, IndvID == "CG5103")$Age_calculated[3], 8L)

})

testthat::test_that("Location_data returns an expected outcome...", {

  #Take a subset of only VAL data
  VAL_data <- dplyr::filter(pipeline_output$Location_data, PopID == "VAL")

  #Test 1: Nest no longer in use
  testthat::expect_equal(subset(VAL_data, LocationID == "VAL001")$StartSeason, 1991)
  testthat::expect_equal(subset(VAL_data, LocationID == "VAL001")$EndSeason, NA_integer_)
  testthat::expect_equal(round(subset(VAL_data, LocationID == "VAL001")$Latitude, 2), 40.89)
  testthat::expect_equal(round(subset(VAL_data, LocationID == "VAL001")$Longitude, 2), -4.03)

  #Test 2: Nest still in use
  testthat::expect_equal(subset(VAL_data, LocationID == "B001")$StartSeason, 2016)
  testthat::expect_equal(subset(VAL_data, LocationID == "B001")$EndSeason, NA_integer_)
  testthat::expect_equal(round(subset(VAL_data, LocationID == "B001")$Latitude, 2), 40.86)
  testthat::expect_equal(round(subset(VAL_data, LocationID == "B001")$Longitude, 2), -4.06)

})






## General tests (for pipelines formatted to standard protocol version 1.1.0)

test_protocol_compliance(pipeline_output)
