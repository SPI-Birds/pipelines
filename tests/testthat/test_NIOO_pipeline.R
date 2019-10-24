context("Run data quality check on NIOO pipeline output")

test_that("NIOO outputs all files...", {

  expect_true(all(c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES") %in% pipeline_output$Location_data$PopID))

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex and each species for an adult and a chick
  #Currently, no chick data is available

  #Take a subset of only NIOO data
  NIOO_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES"))

  #Test 1: Adult male
  #Individual 7137 should be listed as a male great tit
  expect_equal(subset(NIOO_data, IndvID == "7137")$Sex, "M")
  expect_equal(subset(NIOO_data, IndvID == "7137")$Species, "PARMAJ")
  #Check expected BroodIDLaid and Fledged
  expect_equal(subset(NIOO_data, IndvID == "7137")$BroodIDLaid, NA_character_)
  expect_equal(subset(NIOO_data, IndvID == "7137")$BroodIDFledged, NA_character_)
  #Check expected RingSeason and Age
  expect_equal(subset(NIOO_data, IndvID == "7137")$RingSeason, 2005)
  expect_equal(subset(NIOO_data, IndvID == "7137")$RingAge, "adult")

  #Test 2: Adult female
  #Individual 373023 should be listed as a female great tit
  expect_equal(subset(NIOO_data, IndvID == "373023")$Sex, "F")
  expect_equal(subset(NIOO_data, IndvID == "373023")$Species, "PARMAJ")
  #BroodIDLaid and Fledged are as expected
  expect_equal(subset(NIOO_data, IndvID == "373023")$BroodIDLaid, "54001")
  expect_equal(subset(NIOO_data, IndvID == "373023")$BroodIDFledged, "54001")
  #Check expected RingSeason and Age
  expect_equal(subset(NIOO_data, IndvID == "373023")$RingSeason, 2011)
  expect_equal(subset(NIOO_data, IndvID == "373023")$RingAge, "chick")

  #Test 3: Cross-fostered individual
  #Check sex and species are as expected
  #FIND ONE IN THE RIGHT AREA
  # expect_equal(subset(NIOO_data, IndvID == "5025")$Sex, "F")
  # expect_equal(subset(NIOO_data, IndvID == "5025")$Species, "PARMAJ")
  # #BroodIDLaid and Fledged are as expected
  # expect_equal(subset(NIOO_data, IndvID == "5025")$BroodIDLaid, "59110")
  # expect_equal(subset(NIOO_data, IndvID == "5025")$BroodIDFledged, "44381")
  # #Check expected RingSeason and Age
  # expect_equal(subset(NIOO_data, IndvID == "5025")$RingSeason, 2006)
  # expect_equal(subset(NIOO_data, IndvID == "5025")$RingAge, "chick")

  #Test 4: Individual with no GeneticBrood listed
  #Check sex and species are as expected
  expect_equal(subset(NIOO_data, IndvID == "12503")$Sex, NA_character_)
  expect_equal(subset(NIOO_data, IndvID == "12503")$Species, "CYACAE")
  #BroodIDLaid and Fledged are as expected
  expect_equal(subset(NIOO_data, IndvID == "12503")$BroodIDLaid, "47011")
  expect_equal(subset(NIOO_data, IndvID == "12503")$BroodIDFledged, "47011")
  #Check expected RingSeason and Age
  expect_equal(subset(NIOO_data, IndvID == "12503")$RingSeason, 2007)
  expect_equal(subset(NIOO_data, IndvID == "12503")$RingAge, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only NIOO data
  NIOO_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES"))

  #Test 1: clutch type = first
  #Check species is as expected
  expect_equal(subset(NIOO_data, BroodID == "64088")$Species, "CYACAE")
  #Check clutch type is first
  expect_equal(subset(NIOO_data, BroodID == "64088")$ClutchType_calculated, "first")
  #Laying date is as expected
  expect_equal(subset(NIOO_data, BroodID == "64088")$LayDate, as.Date("2016-04-15"))
  #Clutch size, brood size and numberfledged is as expected
  expect_equal(subset(NIOO_data, BroodID == "64088")$ClutchSize, 10L)
  expect_equal(subset(NIOO_data, BroodID == "64088")$BroodSize, 3L)
  expect_equal(subset(NIOO_data, BroodID == "64088")$NumberFledged, 3L)
  #AvgChickMass and AvgTarsus are as expected
  expect_equal(subset(NIOO_data, BroodID == "64088")$AvgChickMass, 11.70)
  expect_equal(subset(NIOO_data, BroodID == "64088")$AvgTarsus, NA_real_)
  #Check ExperimentID is as expected
  expect_equal(subset(NIOO_data, BroodID == "64088")$ExperimentID, "FALSE")
  #Check that Plot is as expected
  expect_equal(subset(NIOO_data, BroodID == "64088")$Plot, "7")

  #Test 2: clutch type = second
  #Check species is as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$Species, "PARMAJ")
  #Check clutch type is first
  expect_equal(subset(NIOO_data, BroodID == "65073")$ClutchType_calculated, "second")
  #Laying date is as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$LayDate, as.Date("2016-05-30"))
  #Clutch size, brood size and numberfledged is as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$ClutchSize, 5L)
  expect_equal(subset(NIOO_data, BroodID == "65073")$BroodSize, 5L)
  expect_equal(subset(NIOO_data, BroodID == "65073")$NumberFledged, 4L)
  #AvgChickMass and AvgTarsus are as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$AvgChickMass, NA_real_)
  expect_equal(subset(NIOO_data, BroodID == "65073")$AvgTarsus, NA_real_)
  #Test ExperimentID is as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$ExperimentID, "FALSE")
  #Check that Plot is as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$Plot, "57")

  #Test 3: clutch type = replacement, where replacement is known (i.e. previous clutch was seen)
  #Check species is as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$Species, "PARMAJ")
  #Check clutch type is first
  expect_equal(subset(NIOO_data, BroodID == "64597")$ClutchType_calculated, "replacement")
  #Laying date is as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$LayDate, as.Date("2016-05-14"))
  #Clutch size, brood size and numberfledged is as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$ClutchSize, 9L)
  expect_equal(subset(NIOO_data, BroodID == "64597")$BroodSize, 0L)
  expect_equal(subset(NIOO_data, BroodID == "64597")$NumberFledged, 0L)
  #AvgChickMass and AvgTarsus are as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$AvgChickMass, NA_real_)
  expect_equal(subset(NIOO_data, BroodID == "64597")$AvgTarsus, NA_real_)
  #Test ExperimentID is as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$ExperimentID, "TRUE")
  #Check that Plot is as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$Plot, "41")

  #Test 4: clutch type = replacement, where replacement calculated from cutoff
  #Check species is as expected
  expect_equal(subset(NIOO_data, BroodID == "64825")$Species, "PARMAJ")
  #Check clutch type is first
  expect_equal(subset(NIOO_data, BroodID == "64825")$ClutchType_calculated, "replacement")
  #Laying date is as expected
  expect_equal(subset(NIOO_data, BroodID == "64825")$LayDate, as.Date("2016-06-01"))
  #Clutch size, brood size and numberfledged is as expected
  expect_equal(subset(NIOO_data, BroodID == "64825")$ClutchSize, 7L)
  expect_equal(subset(NIOO_data, BroodID == "64825")$BroodSize, 0L)
  expect_equal(subset(NIOO_data, BroodID == "64825")$NumberFledged, 0L)
  #AvgChickMass and AvgTarsus are as expected
  expect_equal(round(subset(NIOO_data, BroodID == "64825")$AvgChickMass, 1), NA_real_)
  expect_equal(round(subset(NIOO_data, BroodID == "64825")$AvgTarsus, 1), NA_real_)
  #Test ExperimentID is as expected
  expect_equal(subset(NIOO_data, BroodID == "64825")$ExperimentID, "FALSE")
  #Check that Plot is as expected
  expect_equal(subset(NIOO_data, BroodID == "64825")$Plot, "4")

})

test_that("Capture_data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females

  # Take a subset of only NIOO data
  NIOO_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES"))

  #Test 1: Caught as chick
  #Test number of capture records is as expected
  expect_equal(nrow(subset(NIOO_data, IndvID == "409502")), 10)
  #Test that the first capture is as expected
  expect_equal(subset(NIOO_data, IndvID == "409502")$CaptureDate[1], as.Date("2013-06-01"))
  #Test that the 10th capture is as expected
  expect_equal(subset(NIOO_data, IndvID == "409502")$CaptureDate[10], as.Date("2018-08-20"))
  #Test that first and last age observed is as expected
  expect_equal(subset(NIOO_data, IndvID == "409502")$Age_observed[1], 1L)
  expect_equal(subset(NIOO_data, IndvID == "409502")$Age_observed[10], 6L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(NIOO_data, IndvID == "409502")$Age_calculated[1], 1L)
  expect_equal(subset(NIOO_data, IndvID == "409502")$Age_calculated[10], 13L)

  #Test 2: Caught as adult
  #Test number of capture records is as expected
  expect_equal(nrow(subset(NIOO_data, IndvID == "110438")), 19)
  #Test that the first capture is as expected
  expect_equal(subset(NIOO_data, IndvID == "110438")$CaptureDate[1], as.Date("1974-02-13"))
  #Test that the 15th capture is as expected
  expect_equal(subset(NIOO_data, IndvID == "110438")$CaptureDate[19], as.Date("1977-05-31"))
  #Test that first and last age observed is as expected
  expect_equal(subset(NIOO_data, IndvID == "110438")$Age_observed[1], 5L)
  expect_equal(subset(NIOO_data, IndvID == "110438")$Age_observed[19], 4L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(NIOO_data, IndvID == "110438")$Age_calculated[1], 4L)
  expect_equal(subset(NIOO_data, IndvID == "110438")$Age_calculated[19], 10L)

  #Test 3: Translocation
  #Test number of capture records is as expected
  expect_equal(nrow(subset(NIOO_data, IndvID == "97175")), 2)
  #Test that the first capture is as expected
  expect_equal(subset(NIOO_data, IndvID == "97175")$CaptureDate[1], as.Date("1981-05-22"))
  #Test that the 15th capture is as expected
  expect_equal(subset(NIOO_data, IndvID == "97175")$CaptureDate[2], as.Date("1981-05-29"))
  #Test that first and last age observed is as expected
  expect_equal(subset(NIOO_data, IndvID == "97175")$Age_observed[1], 1L)
  expect_equal(subset(NIOO_data, IndvID == "97175")$Age_observed[2], 1L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(NIOO_data, IndvID == "97175")$Age_calculated[1], 1L)
  expect_equal(subset(NIOO_data, IndvID == "97175")$Age_calculated[2], 1L)
  #Test for cross-fostering in second record
  expect_equal(subset(NIOO_data, IndvID == "97175")$CapturePopID[2], "HOG")
  expect_equal(subset(NIOO_data, IndvID == "97175")$CapturePlot[2], "7")
  expect_equal(subset(NIOO_data, IndvID == "97175")$ReleasePopID[2], NA_character_)
  expect_equal(subset(NIOO_data, IndvID == "97175")$ReleasePlot[2], NA_character_)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  NIOO_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES"))

  #Test 1: Nestbox check
  #Location has multiple records (it had multiple nestboxes over time)
  expect_equal(nrow(subset(NIOO_data, LocationID == "47")), 3)
  #All records have expected LocationType
  #THIS RETURNS AN ERROR BECAUSE ONE OF THEM IS LISTED AS 'OUT OF USE'. This is a mistake in the database
  expect_true(all(subset(NIOO_data, LocationID == "47")$LocationType == "NB"))
  #Expect Start and EndSeason of first box at this location is as expected
  expect_equal(subset(NIOO_data, LocationID == "47")$StartSeason[1], 1996L)
  expect_equal(subset(NIOO_data, LocationID == "47")$EndSeason[1], 1996L)
  #Expect Start and EndSeason of second box at this location is as expected
  expect_equal(subset(NIOO_data, LocationID == "47")$StartSeason[2], 1997L)
  expect_equal(subset(NIOO_data, LocationID == "47")$EndSeason[2], 2006L)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(NIOO_data, LocationID == "47")$PopID[1], "HOG")

  #Test 1: Mistnet check
  #Location has only one record
  expect_equal(nrow(subset(NIOO_data, LocationID == "1841")), 1)
  #All records have expected LocationType
  expect_true(all(subset(NIOO_data, LocationID == "1841")$LocationType == "MN"))
  #Expect Start and EndSeason of first box at this location is as expected
  expect_equal(subset(NIOO_data, LocationID == "1841")$StartSeason, 1977L)
  expect_equal(subset(NIOO_data, LocationID == "1841")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(NIOO_data, LocationID == "1841")$PopID, "OOS")

})
