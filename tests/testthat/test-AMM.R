testthat::skip_on_os("mac")
testthat::skip_if(!exists("data_path"))

pipeline_output <- format_AMM(db = paste0(data_path, "/AMM_Ammersee_Germany"))

test_that("AMM outputs all files...", {

  expect_true(all("AMM" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("AMM" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("AMM" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("AMM" %in% pipeline_output$Location_data$PopID))
  expect_true(pipeline_output$protocol_version == "1.1.0")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only AMM data
  AMM_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "AMM")

  #Test 1: Brood where clutch type = first
  expect_equal(subset(AMM_data, BroodID == "3642")$Species, "PARMAJ")
  expect_equal(subset(AMM_data, BroodID == "3642")$ClutchType_observed, "first")
  expect_equal(subset(AMM_data, BroodID == "3642")$ClutchType_calculated, "first")
  expect_equal(subset(AMM_data, BroodID == "3642")$LayDate_observed, as.Date("2015-04-21"))
  expect_equal(subset(AMM_data, BroodID == "3642")$LayDate_minimum, as.Date("2015-04-21"))
  expect_equal(subset(AMM_data, BroodID == "3642")$LayDate_maximum, as.Date("2015-04-21"))
  expect_equal(subset(AMM_data, BroodID == "3642")$ClutchSize_observed, 7L)
  expect_equal(subset(AMM_data, BroodID == "3642")$ClutchSize_minimum, 7L)
  expect_equal(subset(AMM_data, BroodID == "3642")$ClutchSize_maximum, 7L)
  expect_equal(subset(AMM_data, BroodID == "3642")$BroodSize_observed, NA_integer_)
  expect_equal(subset(AMM_data, BroodID == "3642")$BroodSize_minimum, NA_integer_)
  expect_equal(subset(AMM_data, BroodID == "3642")$BroodSize_maximum, NA_integer_)
  expect_equal(subset(AMM_data, BroodID == "3642")$NumberFledged_observed, 7L)
  expect_equal(subset(AMM_data, BroodID == "3642")$NumberFledged_minimum, 7L)
  expect_equal(subset(AMM_data, BroodID == "3642")$NumberFledged_observed, 7L)
  expect_equal(round(subset(AMM_data, BroodID == "3642")$AvgChickMass, 2), NA_real_)
  expect_equal(round(subset(AMM_data, BroodID == "3642")$AvgTarsus, 2), 20.46)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(AMM_data, BroodID == "4375")$Species, "PARMAJ")
  expect_equal(subset(AMM_data, BroodID == "4375")$ClutchType_observed, "replacement")
  expect_equal(subset(AMM_data, BroodID == "4375")$ClutchType_calculated, "replacement")
  expect_equal(subset(AMM_data, BroodID == "4375")$LayDate_observed, as.Date("2016-05-18"))
  expect_equal(subset(AMM_data, BroodID == "4375")$LayDate_minimum, as.Date("2016-05-18"))
  expect_equal(subset(AMM_data, BroodID == "4375")$LayDate_maximum, as.Date("2016-05-18"))
  expect_equal(subset(AMM_data, BroodID == "4375")$ClutchSize_observed, 8L)
  expect_equal(subset(AMM_data, BroodID == "4375")$ClutchSize_minimum, 8L)
  expect_equal(subset(AMM_data, BroodID == "4375")$ClutchSize_maximum, 8L)
  expect_equal(subset(AMM_data, BroodID == "4375")$BroodSize_observed, NA_integer_)
  expect_equal(subset(AMM_data, BroodID == "4375")$BroodSize_minimum, NA_integer_)
  expect_equal(subset(AMM_data, BroodID == "4375")$BroodSize_maximum, NA_integer_)
  expect_equal(subset(AMM_data, BroodID == "4375")$FledgeDate_observed, as.Date(NA))
  expect_equal(subset(AMM_data, BroodID == "4375")$FledgeDate_minimum, as.Date(NA))
  expect_equal(subset(AMM_data, BroodID == "4375")$FledgeDate_maximum, as.Date(NA))
  expect_equal(subset(AMM_data, BroodID == "4375")$NumberFledged_observed, 0L)
  expect_equal(subset(AMM_data, BroodID == "4375")$NumberFledged_minimum, 0L)
  expect_equal(subset(AMM_data, BroodID == "4375")$NumberFledged_maximum, 0L)
  #Measurements taken but not included in average because chick age was >16
  expect_equal(subset(AMM_data, BroodID == "4375")$AvgChickMass, NA_real_)
  expect_equal(subset(AMM_data, BroodID == "4375")$AvgTarsus, 20.25)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(AMM_data, BroodID == "59")$Species, "CYACAE")
  expect_equal(subset(AMM_data, BroodID == "59")$ClutchType_observed, "replacement")
  expect_equal(subset(AMM_data, BroodID == "59")$ClutchType_calculated, "replacement")
  expect_equal(subset(AMM_data, BroodID == "59")$LayDate_observed, as.Date("2010-05-11"))
  expect_equal(subset(AMM_data, BroodID == "59")$ClutchSize_observed, 1L)
  expect_equal(subset(AMM_data, BroodID == "59")$BroodSize_observed, 1L)
  expect_equal(subset(AMM_data, BroodID == "59")$NumberFledged_observed, 0L)
  expect_equal(subset(AMM_data, BroodID == "59")$AvgChickMass, NA_real_)
  expect_equal(subset(AMM_data, BroodID == "59")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(AMM_data, BroodID == "581")$Species, "PARMAJ")
  expect_equal(subset(AMM_data, BroodID == "581")$ClutchType_observed, "second")
  expect_equal(subset(AMM_data, BroodID == "581")$ClutchType_calculated, "second")
  expect_equal(subset(AMM_data, BroodID == "581")$LayDate_observed, as.Date("2011-05-22"))
  expect_equal(subset(AMM_data, BroodID == "581")$ClutchSize_observed, 8L)
  expect_equal(subset(AMM_data, BroodID == "581")$BroodSize_observed, 8L)
  expect_equal(subset(AMM_data, BroodID == "581")$NumberFledged_observed, 5L)
  expect_equal(subset(AMM_data, BroodID == "581")$AvgChickMass, NA_real_)
  expect_equal(round(subset(AMM_data, BroodID == "581")$AvgTarsus, 2), 18.82)

  #Test 5: Brood with error in measurements in counts (e.g. clutch/brood/nrfledge)
  expect_equal(subset(AMM_data, BroodID == "1")$Species, "CYACAE")
  expect_equal(subset(AMM_data, BroodID == "1")$ClutchType_observed, "first")
  expect_equal(subset(AMM_data, BroodID == "1")$ClutchType_calculated, "first")
  expect_equal(subset(AMM_data, BroodID == "1")$LayDate_observed, as.Date("2010-04-18"))
  expect_equal(subset(AMM_data, BroodID == "1")$ClutchSize_observed, 10L)
  expect_equal(subset(AMM_data, BroodID == "1")$BroodSize_observed, 6L)
  expect_equal(subset(AMM_data, BroodID == "1")$BroodSize_minimum, 6L)
  expect_equal(subset(AMM_data, BroodID == "1")$BroodSize_maximum, 8L)
  expect_equal(subset(AMM_data, BroodID == "1")$NumberFledged_observed, 6L)
  expect_equal(subset(AMM_data, BroodID == "1")$AvgChickMass, NA_real_)
  expect_equal(subset(AMM_data, BroodID == "1")$AvgTarsus, NA_real_)

  #Test 5: Brood with error in date
  expect_equal(subset(AMM_data, BroodID == "1241")$Species, "PARMAJ")
  expect_equal(subset(AMM_data, BroodID == "1241")$ClutchType_observed, "replacement")
  expect_equal(subset(AMM_data, BroodID == "1241")$ClutchType_calculated, "replacement")
  expect_equal(subset(AMM_data, BroodID == "1241")$LayDate_observed, as.Date("2012-05-13"))
  expect_equal(subset(AMM_data, BroodID == "1241")$LayDate_minimum, as.Date("2012-05-08"))
  expect_equal(subset(AMM_data, BroodID == "1241")$LayDate_maximum, as.Date("2012-05-13"))
  expect_equal(subset(AMM_data, BroodID == "1241")$ClutchSize_observed, 6L)
  expect_equal(subset(AMM_data, BroodID == "1241")$BroodSize_observed, 0L)
  expect_equal(subset(AMM_data, BroodID == "1241")$NumberFledged_observed, 0L)
  expect_equal(subset(AMM_data, BroodID == "1241")$AvgChickMass, NA_real_)
  expect_equal(subset(AMM_data, BroodID == "1241")$AvgTarsus, NA_real_)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only AMM data
  AMM_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "AMM")

  #Test 1: First caught as adult
  expect_equal(subset(AMM_data, IndvID == "251")$Sex_calculated, "F")
  expect_equal(subset(AMM_data, IndvID == "251")$Sex_genetic, NA_character_)
  expect_equal(subset(AMM_data, IndvID == "251")$Species, "PARMAJ")
  expect_equal(subset(AMM_data, IndvID == "251")$BroodIDLaid, NA_character_)
  expect_equal(subset(AMM_data, IndvID == "251")$BroodIDFledged, NA_character_)
  expect_equal(subset(AMM_data, IndvID == "251")$RingSeason, 2010L)
  expect_equal(subset(AMM_data, IndvID == "251")$RingAge, "adult")

  #Test 2: Caught first as chick (not cross fostered)
  expect_equal(subset(AMM_data, IndvID == "15412")$Sex_calculated, "F")
  expect_equal(subset(AMM_data, IndvID == "15412")$Sex_genetic, NA_character_)
  expect_equal(subset(AMM_data, IndvID == "15412")$Species, "PARMAJ")
  expect_equal(subset(AMM_data, IndvID == "15412")$BroodIDLaid, "3853")
  expect_equal(subset(AMM_data, IndvID == "15412")$BroodIDFledged, "3853")
  expect_equal(subset(AMM_data, IndvID == "15412")$RingSeason, 2015L)
  expect_equal(subset(AMM_data, IndvID == "15412")$RingAge, "chick")

  #Test 2: Caught first as chick (cross fostered)
  expect_equal(subset(AMM_data, IndvID == "2108")$Sex_calculated, NA_character_)
  expect_equal(subset(AMM_data, IndvID == "2108")$Sex_genetic, "M")
  expect_equal(subset(AMM_data, IndvID == "2108")$Species, "PARMAJ")
  expect_equal(subset(AMM_data, IndvID == "2108")$BroodIDLaid, "321")
  expect_equal(subset(AMM_data, IndvID == "2108")$BroodIDFledged, "331")
  expect_equal(subset(AMM_data, IndvID == "2108")$RingSeason, 2011L)
  expect_equal(subset(AMM_data, IndvID == "2108")$RingAge, "chick")

})

test_that("Capture data returns an expected outcome...", {

  #Take a subset of only AMM data
  AMM_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "AMM")

  #Test 1: Individual ringed as a chick
  expect_equal(nrow(subset(AMM_data, IndvID == "952")), 12L)
  expect_equal(subset(AMM_data, IndvID == "952")$CaptureDate[1], as.Date("2010-05-10"))
  expect_equal(subset(AMM_data, IndvID == "952")$CaptureDate[12], as.Date("2015-01-14"))
  expect_equal(subset(AMM_data, IndvID == "952")$Age_observed[1], 1L)
  expect_equal(subset(AMM_data, IndvID == "952")$Age_observed[12], 6L)
  expect_equal(subset(AMM_data, IndvID == "952")$Age_calculated[1], 1L)
  expect_equal(subset(AMM_data, IndvID == "952")$Age_calculated[12], 13L)

  #Test 2: Individual caught only as adult
  expect_equal(nrow(subset(AMM_data, IndvID == "15450")), 11)
  expect_equal(subset(AMM_data, IndvID == "15450")$CaptureDate[1], as.Date("2015-10-20"))
  expect_equal(subset(AMM_data, IndvID == "15450")$CaptureDate[11], as.Date("2019-06-17"))
  expect_equal(subset(AMM_data, IndvID == "15450")$Age_observed[1], 4L)
  expect_equal(subset(AMM_data, IndvID == "15450")$Age_observed[11], 6L)
  expect_equal(subset(AMM_data, IndvID == "15450")$Age_calculated[1], 4L)
  expect_equal(subset(AMM_data, IndvID == "15450")$Age_calculated[11], 12L)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  AMM_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "AMM")

  #Test 1: Nestbox check
  expect_true(subset(AMM_data, LocationID == "1144")$LocationType == "NB")
  expect_true(subset(AMM_data, LocationID == "1144")$NestboxID == "1144")
  expect_equal(subset(AMM_data, LocationID == "1144")$StartSeason, 2010L)
  expect_equal(subset(AMM_data, LocationID == "1144")$EndSeason, 2019L)
  expect_equal(subset(AMM_data, LocationID == "1144")$PopID, "AMM")
  expect_equal(round(subset(AMM_data, LocationID == "1144")$Latitude, 2) %>% setNames(nm = NULL), 47.98)
  expect_equal(round(subset(AMM_data, LocationID == "1144")$Longitude, 2) %>% setNames(nm = NULL), 11.16)

})
