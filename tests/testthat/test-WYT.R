pipeline_output <- format_WYT(db = paste0(data_path, "/WYT_WythamWoods_UK"))

test_that("WYT outputs all files...", {

  expect_true(all("WYT" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("WYT" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("WYT" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("WYT" %in% pipeline_output$Location_data$PopID))

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only WYT data
  WYT_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "WYT")

  #Test 1: Brood where clutch type = first
  expect_equal(subset(WYT_data, BroodID == "19671W84")$Species, "PARMAJ")
  expect_equal(subset(WYT_data, BroodID == "19671W84")$ClutchType_calculated, "first")
  expect_equal(subset(WYT_data, BroodID == "19671W84")$LayDate, as.Date("1967-04-27"))
  expect_equal(subset(WYT_data, BroodID == "19671W84")$ClutchSize, 8L)
  expect_equal(subset(WYT_data, BroodID == "19671W84")$BroodSize, 0L)
  expect_equal(subset(WYT_data, BroodID == "19671W84")$NumberFledged, 0L)
  expect_equal(subset(WYT_data, BroodID == "19671W84")$AvgChickMass, NA_real_)
  expect_equal(subset(WYT_data, BroodID == "19671W84")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(WYT_data, BroodID == "19751SW108")$Species, "CYACAE")
  expect_equal(subset(WYT_data, BroodID == "19751SW108")$ClutchType_calculated, "replacement")
  expect_equal(subset(WYT_data, BroodID == "19751SW108")$LayDate, as.Date("1975-05-06"))
  expect_equal(subset(WYT_data, BroodID == "19751SW108")$ClutchSize, 3L)
  expect_equal(subset(WYT_data, BroodID == "19751SW108")$BroodSize, NA_integer_)
  expect_equal(subset(WYT_data, BroodID == "19751SW108")$NumberFledged, 0L)
  expect_equal(subset(WYT_data, BroodID == "19751SW108")$AvgChickMass, NA_real_)
  expect_equal(subset(WYT_data, BroodID == "19751SW108")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(WYT_data, BroodID == "19861MP13")$Species, "PARMAJ")
  expect_equal(subset(WYT_data, BroodID == "19861MP13")$ClutchType_calculated, "replacement")
  expect_equal(subset(WYT_data, BroodID == "19861MP13")$LayDate, as.Date("1986-06-08"))
  expect_equal(subset(WYT_data, BroodID == "19861MP13")$ClutchSize, 7L)
  expect_equal(subset(WYT_data, BroodID == "19861MP13")$BroodSize, 7L)
  expect_equal(subset(WYT_data, BroodID == "19861MP13")$NumberFledged, 4L)
  #Although there are chicks fledged, they have no recorded capture date,
  #so we don't know how old they are and they are excluded
  expect_equal(subset(WYT_data, BroodID == "19861MP13")$AvgChickMass, NA_real_)
  expect_equal(subset(WYT_data, BroodID == "19861MP13")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(WYT_data, BroodID == "20041C134")$Species, "PARMAJ")
  expect_equal(subset(WYT_data, BroodID == "20041C134")$ClutchType_calculated, "second")
  expect_equal(subset(WYT_data, BroodID == "20041C134")$LayDate, as.Date("2004-05-28"))
  expect_equal(subset(WYT_data, BroodID == "20041C134")$ClutchSize, 6L)
  expect_equal(subset(WYT_data, BroodID == "20041C134")$BroodSize, 3L)
  expect_equal(subset(WYT_data, BroodID == "20041C134")$NumberFledged, 3L)
  expect_equal(round(subset(WYT_data, BroodID == "20041C134")$AvgChickMass, 2), 19.17)
  expect_equal(subset(WYT_data, BroodID == "20041C134")$AvgTarsus, NA_real_)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only WYT data
  WYT_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "WYT")

  #Test 1: First caught as adult
  expect_equal(subset(WYT_data, IndvID == "D472783")$Sex, "F")
  expect_equal(subset(WYT_data, IndvID == "D472783")$Species, "CYACAE")
  #They should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(WYT_data, IndvID == "D472783")$BroodIDLaid, NA_character_)
  expect_equal(subset(WYT_data, IndvID == "D472783")$BroodIDFledged, NA_character_)
  #Ring age and season are as expected
  expect_equal(subset(WYT_data, IndvID == "D472783")$RingSeason, 2014L)
  expect_equal(subset(WYT_data, IndvID == "D472783")$RingAge, "adult")

  #Test 2: Caught first as chick
  expect_equal(subset(WYT_data, IndvID == "TP27407")$Sex, "F")
  expect_equal(subset(WYT_data, IndvID == "TP27407")$Species, "PARMAJ")
  #Check that BroodIDLaid/Fledged are as expected
  expect_equal(subset(WYT_data, IndvID == "TP27407")$BroodIDLaid, "20131B70")
  expect_equal(subset(WYT_data, IndvID == "TP27407")$BroodIDFledged, "20131B70")
  #Ring season is as expected
  expect_equal(subset(WYT_data, IndvID == "TP27407")$RingSeason, 2013L)
  expect_equal(subset(WYT_data, IndvID == "TP27407")$RingAge, "chick")

  #Test 3: Caught pre-2012 and post-2012 (two different spreadsheets)
  expect_equal(subset(WYT_data, IndvID == "TR43591")$Sex, "F")
  expect_equal(subset(WYT_data, IndvID == "TR43591")$Species, "PARMAJ")
  #Check that BroodIDLaid/Fledged are as expected
  expect_equal(subset(WYT_data, IndvID == "TR43591")$BroodIDLaid, "20101EX49B")
  expect_equal(subset(WYT_data, IndvID == "TR43591")$BroodIDFledged, "20101EX49B")
  #Ring season is as expected
  expect_equal(subset(WYT_data, IndvID == "TR43591")$RingSeason, 2010L)
  expect_equal(subset(WYT_data, IndvID == "TR43591")$RingAge, "chick")

})

test_that("Capture data returns an expected outcome...", {

  #Take a subset of only WYT data
  WYT_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "WYT")

  #Test 1: Individual ringed as a chick
  #Test there are the correct number of capture records
  expect_equal(nrow(subset(WYT_data, IndvID == "TP27407")), 8)
  #Test that the first and 8th capture are as expected
  expect_equal(subset(WYT_data, IndvID == "TP27407")$CaptureDate[1], as.Date("2013-06-06"))
  expect_equal(subset(WYT_data, IndvID == "TP27407")$CaptureDate[8], as.Date("2018-05-19"))
  #Test that age observed is as expected on first capture
  #Test that age observed is as expected on 8th capture
  expect_equal(subset(WYT_data, IndvID == "TP27407")$Age_observed[1], 1L)
  expect_equal(subset(WYT_data, IndvID == "TP27407")$Age_observed[8], 6L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(WYT_data, IndvID == "TP27407")$Age_calculated[1], 1L)
  expect_equal(subset(WYT_data, IndvID == "TP27407")$Age_calculated[8], 13L)

  #Test 2: Individual caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(WYT_data, IndvID == "D472783")), 7)
  #Test that capture dates are as expected
  expect_equal(subset(WYT_data, IndvID == "D472783")$CaptureDate[1], as.Date("2014-06-09"))
  expect_equal(subset(WYT_data, IndvID == "D472783")$CaptureDate[7], as.Date("2018-05-21"))
  #Test that age observed is as expected
  expect_equal(subset(WYT_data, IndvID == "D472783")$Age_observed[1], 5L)
  expect_equal(subset(WYT_data, IndvID == "D472783")$Age_observed[7], 6L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(WYT_data, IndvID == "D472783")$Age_calculated[1], 4L)
  expect_equal(subset(WYT_data, IndvID == "D472783")$Age_calculated[7], 12L)

  #Test 3: Caught pre-2012 and post-2012 (two different spreadsheets)
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(WYT_data, IndvID == "TR43591")), 6)
  #Test that capture dates are as expected
  expect_equal(subset(WYT_data, IndvID == "TR43591")$CaptureDate[1], as.Date("2010-05-26"))
  expect_equal(subset(WYT_data, IndvID == "TR43591")$CaptureDate[6], as.Date("2013-06-04"))
  #Test that age observed is as expected
  expect_equal(subset(WYT_data, IndvID == "TR43591")$Age_observed[1], 1L)
  expect_equal(subset(WYT_data, IndvID == "TR43591")$Age_observed[6], 6L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(WYT_data, IndvID == "TR43591")$Age_calculated[1], 1L)
  expect_equal(subset(WYT_data, IndvID == "TR43591")$Age_calculated[6], 9L)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  WYT_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "WYT")

  #Test 1: Nestbox check
  expect_true(subset(WYT_data, LocationID == "P1")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(WYT_data, LocationID == "P1")$NestboxID == "P1")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(WYT_data, LocationID == "P1")$StartSeason, 1947L)
  expect_equal(subset(WYT_data, LocationID == "P1")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(WYT_data, LocationID == "P1")$PopID, "WYT")
  #Check that latitude and longitude are as expected
  expect_equal(subset(WYT_data, LocationID == "P1")$Latitude %>% setNames(nm = NULL), NA_real_)
  expect_equal(subset(WYT_data, LocationID == "P1")$Longitude %>% setNames(nm = NULL), NA_real_)

})
