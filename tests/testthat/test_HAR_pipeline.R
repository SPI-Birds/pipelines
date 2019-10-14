context("Run data quality check on Harjavalta pipeline output")

test_that("HAR outputs all files...", {

  expect_true("HAR" %in% pipeline_output$Brood_data$PopID)
  expect_true("HAR" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("HAR" %in% pipeline_output$Individual_data$PopID)
  expect_true("HAR" %in% pipeline_output$Location_data$PopID)

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
