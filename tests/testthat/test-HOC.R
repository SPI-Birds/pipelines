pipeline_output <- format_HOC(db = paste0(data_path, "/HOC_Hochstadt_Germany"))

test_that("HOC outputs all files...", {

  expect_true(all("HOC" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("HOC" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("HOC" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("HOC" %in% pipeline_output$Location_data$PopID))

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only HOC data
  HOC_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "HOC")

  #Test 1: Brood where clutch type = first
  expect_equal(subset(HOC_data, BroodID == "2016-H101-1")$Species, "PARMAJ")
  expect_equal(subset(HOC_data, BroodID == "2016-H101-1")$ClutchType_calculated, "first")
  expect_equal(subset(HOC_data, BroodID == "2016-H101-1")$LayDate, as.Date("2016-04-22"))
  expect_equal(subset(HOC_data, BroodID == "2016-H101-1")$ClutchSize, 7L)
  expect_equal(subset(HOC_data, BroodID == "2016-H101-1")$BroodSize, NA_integer_)
  expect_equal(subset(HOC_data, BroodID == "2016-H101-1")$NumberFledged, 0L)
  expect_equal(subset(HOC_data, BroodID == "2016-H101-1")$AvgChickMass, NA_real_)
  expect_equal(subset(HOC_data, BroodID == "2016-H101-1")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(HOC_data, BroodID == "2016-H111-2")$Species, "PARMAJ")
  expect_equal(subset(HOC_data, BroodID == "2016-H111-2")$ClutchType_calculated, "replacement")
  expect_equal(subset(HOC_data, BroodID == "2016-H111-2")$LayDate, as.Date("2016-05-30"))
  expect_equal(subset(HOC_data, BroodID == "2016-H111-2")$ClutchSize, 6)
  expect_equal(subset(HOC_data, BroodID == "2016-H111-2")$BroodSize, NA_integer_)
  expect_equal(subset(HOC_data, BroodID == "2016-H111-2")$NumberFledged, 4)
  expect_equal(round(subset(HOC_data, BroodID == "2016-H111-2")$AvgChickMass, 2), 13.65)
  expect_equal(round(subset(HOC_data, BroodID == "2016-H111-2")$AvgTarsus, 2), 18.20)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(HOC_data, BroodID == "2017-H101-2")$Species, "PARMAJ")
  expect_equal(subset(HOC_data, BroodID == "2017-H101-2")$ClutchType_calculated, "replacement")
  expect_equal(subset(HOC_data, BroodID == "2017-H101-2")$LayDate, as.Date("2017-06-03"))
  expect_equal(subset(HOC_data, BroodID == "2017-H101-2")$ClutchSize, 6L)
  expect_equal(subset(HOC_data, BroodID == "2017-H101-2")$BroodSize, NA_integer_)
  expect_equal(subset(HOC_data, BroodID == "2017-H101-2")$NumberFledged, 1L)
  expect_equal(round(subset(HOC_data, BroodID == "2017-H101-2")$AvgChickMass, 2), 12.00)
  expect_equal(round(subset(HOC_data, BroodID == "2017-H101-2")$AvgTarsus, 2), 15.92)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(HOC_data, BroodID == "2017-H71-2")$Species, "PARMAJ")
  expect_equal(subset(HOC_data, BroodID == "2017-H71-2")$ClutchType_calculated, "second")
  expect_equal(subset(HOC_data, BroodID == "2017-H71-2")$LayDate, as.Date("2017-05-31"))
  expect_equal(subset(HOC_data, BroodID == "2017-H71-2")$ClutchSize, 8L)
  expect_equal(subset(HOC_data, BroodID == "2017-H71-2")$BroodSize, NA_integer_)
  expect_equal(subset(HOC_data, BroodID == "2017-H71-2")$NumberFledged, 4L)
  expect_equal(round(subset(HOC_data, BroodID == "2017-H71-2")$AvgChickMass, 2), 13.75)
  expect_equal(round(subset(HOC_data, BroodID == "2017-H71-2")$AvgTarsus, 2), 18.55)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only HOC data
  HOC_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "HOC")

  #Test 1: First as adult
  expect_equal(subset(HOC_data, IndvID == "C1X0940")$Sex, "F")
  expect_equal(subset(HOC_data, IndvID == "C1X0940")$Species, "PARMAJ")
  #They should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(HOC_data, IndvID == "C1X0940")$BroodIDLaid, NA_character_)
  expect_equal(subset(HOC_data, IndvID == "C1X0940")$BroodIDFledged, NA_character_)
  #Ring age and season are as expected
  expect_equal(subset(HOC_data, IndvID == "C1X0940")$RingSeason, 2014L)
  expect_equal(subset(HOC_data, IndvID == "C1X0940")$RingAge, "adult")

  #Test 2: Caught first as chick
  expect_equal(subset(HOC_data, IndvID == "C1X0972")$Sex, NA_character_)
  expect_equal(subset(HOC_data, IndvID == "C1X0972")$Species, "PARMAJ")
  #Check that BroodIDLaid/Fledged are as expected
  expect_equal(subset(HOC_data, IndvID == "C1X0972")$BroodIDLaid, "2014-H61-1")
  expect_equal(subset(HOC_data, IndvID == "C1X0972")$BroodIDFledged, "2014-H61-1")
  #Ring season is as expected
  expect_equal(subset(HOC_data, IndvID == "C1X0972")$RingSeason, 2014)
  expect_equal(subset(HOC_data, IndvID == "C1X0972")$RingAge, "chick")

})

test_that("Capture data returns an expected outcome...", {

  #Take a subset of only HOC data
  HOC_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "HOC")

  #Test 1: Individual ringed as a chick
  #Test there are the correct number of capture records
  expect_equal(nrow(subset(HOC_data, IndvID == "C3F8488")), 5)
  #Test that the first and 5th capture are as expected
  expect_equal(subset(HOC_data, IndvID == "C3F8488")$CaptureDate[1], as.Date("2014-05-18"))
  expect_equal(subset(HOC_data, IndvID == "C3F8488")$CaptureDate[5], as.Date("2016-06-27"))
  #Test that age observed is as expected on first capture
  #Test that age observed is as expected on 5th capture
  expect_equal(subset(HOC_data, IndvID == "C3F8488")$Age_observed[1], 1L)
  expect_equal(subset(HOC_data, IndvID == "C3F8488")$Age_observed[5], 4L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(HOC_data, IndvID == "C3F8488")$Age_calculated[1], 1L)
  expect_equal(subset(HOC_data, IndvID == "C3F8488")$Age_calculated[5], 7L)

  #Test 2: Individual caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(HOC_data, IndvID == "C3K7291")), 7)
  #Test that the first capture is as expected
  expect_equal(subset(HOC_data, IndvID == "C3K7291")$CaptureDate[1], as.Date("2016-05-09"))
  #Test that the 7th capture is as expected
  expect_equal(subset(HOC_data, IndvID == "C3K7291")$CaptureDate[7], as.Date("2019-05-08"))
  #Test that first and last age observed is as expected
  expect_equal(subset(HOC_data, IndvID == "C3K7291")$Age_observed[1], 4L)
  expect_equal(subset(HOC_data, IndvID == "C3K7291")$Age_observed[7], 4L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(HOC_data, IndvID == "C3K7291")$Age_calculated[1], 4L)
  expect_equal(subset(HOC_data, IndvID == "C3K7291")$Age_calculated[7], 10L)

  #Test 3: Individual caught as 1st year adult pre 2019
  #This individual should be treated as EURING 4
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(HOC_data, IndvID == "C3K7272")), 5)
  #Test that the first capture is as expected
  expect_equal(subset(HOC_data, IndvID == "C3K7272")$CaptureDate[1], as.Date("2016-03-08"))
  #Test that the 7th capture is as expected
  expect_equal(subset(HOC_data, IndvID == "C3K7272")$CaptureDate[5], as.Date("2019-05-09"))
  #Test that first and last age observed is as expected
  expect_equal(subset(HOC_data, IndvID == "C3K7272")$Age_observed[1], 4L)
  expect_equal(subset(HOC_data, IndvID == "C3K7272")$Age_observed[5], 4L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(HOC_data, IndvID == "C3K7272")$Age_calculated[1], 4L)
  expect_equal(subset(HOC_data, IndvID == "C3K7272")$Age_calculated[5], 10L)

  #Test 4: Individual caught as 1st year adult post 2019
  #This individual should be treated as EURING 5
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(HOC_data, IndvID == "C5E9102")), 2)
  #Test that the first capture is as expected
  expect_equal(subset(HOC_data, IndvID == "C5E9102")$CaptureDate[1], as.Date("2019-03-16"))
  #Test that the 7th capture is as expected
  expect_equal(subset(HOC_data, IndvID == "C5E9102")$CaptureDate[2], as.Date("2019-05-08"))
  #Test that first and last age observed is as expected
  expect_equal(subset(HOC_data, IndvID == "C5E9102")$Age_observed[1], 5L)
  expect_equal(subset(HOC_data, IndvID == "C5E9102")$Age_observed[2], 4L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(HOC_data, IndvID == "C5E9102")$Age_calculated[1], 4L)
  expect_equal(subset(HOC_data, IndvID == "C5E9102")$Age_calculated[2], 4L)

  #Test 5: Individual found dead in DeadCapture ID table
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(HOC_data, IndvID == "C5E8940")), 3)
  #Test that the first capture is as expected
  expect_equal(subset(HOC_data, IndvID == "C5E8940")$CaptureDate[1], as.Date("2019-05-13"))
  #Test that the 7th capture is as expected
  expect_equal(subset(HOC_data, IndvID == "C5E8940")$CaptureDate[3], as.Date("2019-05-23"))
  #Test that first and last age observed is as expected
  expect_equal(subset(HOC_data, IndvID == "C5E8940")$Age_observed[1], 1L)
  expect_equal(subset(HOC_data, IndvID == "C5E8940")$Age_observed[3], 1L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(HOC_data, IndvID == "C5E8940")$Age_calculated[1], 1L)
  expect_equal(subset(HOC_data, IndvID == "C5E8940")$Age_calculated[3], 1L)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  HOC_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "HOC")

  #Test 1: Nestbox check
  expect_true(subset(HOC_data, LocationID == "H1")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(HOC_data, LocationID == "H1")$NestboxID == "H1")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(HOC_data, LocationID == "H1")$StartSeason, 2014L)
  expect_equal(subset(HOC_data, LocationID == "H1")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(HOC_data, LocationID == "H1")$PopID, "HOC")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(HOC_data, LocationID == "H1")$Latitude, 2) %>% setNames(nm = NULL), 11.26)
  expect_equal(round(subset(HOC_data, LocationID == "H1")$Longitude, 2) %>% setNames(nm = NULL), 48.06)

})
