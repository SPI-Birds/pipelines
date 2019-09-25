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
  expect_equal(subset(NIOO_data, BroodID == "64088")$LayingDate, as.Date("2016-04-15"))
  #Clutch size, brood size and numberfledged is as expected
  expect_equal(subset(NIOO_data, BroodID == "64088")$ClutchSize, 10L)
  expect_equal(subset(NIOO_data, BroodID == "64088")$BroodSize, 3L)
  expect_equal(subset(NIOO_data, BroodID == "64088")$NumberFledged, 3L)
  #AvgChickMass and AvgTarsus are as expected
  expect_equal(subset(NIOO_data, BroodID == "64088")$AvgChickMass, NA_real_)
  expect_equal(subset(NIOO_data, BroodID == "64088")$AvgTarsus, NA_real_)

  #Test 2: clutch type = second
  #Check species is as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$Species, "PARMAJ")
  #Check clutch type is first
  expect_equal(subset(NIOO_data, BroodID == "65073")$ClutchType_calculated, "second")
  #Laying date is as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$LayingDate, as.Date("2016-05-30"))
  #Clutch size, brood size and numberfledged is as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$ClutchSize, 5L)
  expect_equal(subset(NIOO_data, BroodID == "65073")$BroodSize, 5L)
  expect_equal(subset(NIOO_data, BroodID == "65073")$NumberFledged, 4L)
  #AvgChickMass and AvgTarsus are as expected
  expect_equal(subset(NIOO_data, BroodID == "65073")$AvgChickMass, NA_real_)
  expect_equal(subset(NIOO_data, BroodID == "65073")$AvgTarsus, NA_real_)

  #Test 3: clutch type = replacement, where replacement is known (i.e. previous clutch was seen)
  #Check species is as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$Species, "PARMAJ")
  #Check clutch type is first
  expect_equal(subset(NIOO_data, BroodID == "64597")$ClutchType_calculated, "replacement")
  #Laying date is as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$LayingDate, as.Date("2016-05-14"))
  #Clutch size, brood size and numberfledged is as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$ClutchSize, 9L)
  expect_equal(subset(NIOO_data, BroodID == "64597")$BroodSize, 0L)
  expect_equal(subset(NIOO_data, BroodID == "64597")$NumberFledged, 0L)
  #AvgChickMass and AvgTarsus are as expected
  expect_equal(subset(NIOO_data, BroodID == "64597")$AvgChickMass, NA_real_)
  expect_equal(subset(NIOO_data, BroodID == "64597")$AvgTarsus, NA_real_)

  #Test 4: clutch type = replacement, where replacement calculated from cutoff
  #Check species is as expected
  expect_equal(subset(NIOO_data, BroodID == "64825")$Species, "PARMAJ")
  #Check clutch type is first
  expect_equal(subset(NIOO_data, BroodID == "64825")$ClutchType_calculated, "replacement")
  #Laying date is as expected
  expect_equal(subset(NIOO_data, BroodID == "64825")$LayingDate, as.Date("2016-06-01"))
  #Clutch size, brood size and numberfledged is as expected
  expect_equal(subset(NIOO_data, BroodID == "64825")$ClutchSize, 7L)
  expect_equal(subset(NIOO_data, BroodID == "64825")$BroodSize, 0L)
  expect_equal(subset(NIOO_data, BroodID == "64825")$NumberFledged, 0L)
  #AvgChickMass and AvgTarsus are as expected
  expect_equal(round(subset(NIOO_data, BroodID == "64825")$AvgChickMass, 1), NA_real_)
  expect_equal(round(subset(NIOO_data, BroodID == "64825")$AvgTarsus, 1), NA_real_)

})

test_that("Capture_data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only NIOO data
  # NIOO_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES"))
  #
  # #Test 1: Female caught as adult
  # #Test the female has the correct number of capture records (6)
  # expect_equal(nrow(subset(NIOO_data, IndvID == "D534573")), 6)
  # #Test that the first capture of the female is as expected (02/06/2013)
  # expect_equal(min(subset(NIOO_data, IndvID == "D534573")$CaptureDate, na.rm = TRUE), as.Date("2013-06-02"))
  # #Test that the 5th capture of the female is as expcted (2017-05-12) (6th one is NA so no good to test)
  # expect_equal(subset(NIOO_data, IndvID == "D534573")$CaptureDate[5], as.Date("2017-05-12"))
  # #Test that age observed is as expected (NA, because we just know it was caught as an adult, that's all)
  # expect_equal(subset(NIOO_data, IndvID == "D534573")$Age_observed[1], NA_integer_)
  # #Test that age calculated is correct on first capture (4 because it's an adult of unknown age)
  # expect_equal(subset(NIOO_data, IndvID == "D534573")$Age_calculated[1], 4L)
  # #Test that age calculated is correct on 2017 capture (4 + 4*2 = 12 because it's an adult caught 4 years after its first capture)
  # expect_equal(subset(NIOO_data, IndvID == "D534573")$Age_calculated[6], 12L)
  #
  # #Test 1: Male caught as adult
  # #Test the male has the correct number of capture records (6)
  # expect_equal(nrow(subset(NIOO_data, IndvID == "D534574")), 6)
  # #Test that the first capture of the male is as expected (2013-06-02)
  # expect_equal(min(subset(NIOO_data, IndvID == "D534574")$CaptureDate, na.rm = TRUE), as.Date("2013-06-02"))
  # #Test that the 6th capture of the male is as expcted (2018-06-08)
  # expect_equal(subset(NIOO_data, IndvID == "D534574")$CaptureDate[6], as.Date("2018-06-08"))
  # #Test that age observed is as expected (NA, because we just know it was caught as an adult, that's all)
  # expect_equal(subset(NIOO_data, IndvID == "D534574")$Age_observed[1], NA_integer_)
  # #Test that age calculated is correct on first capture (4 because it's an adult of unknown age)
  # expect_equal(subset(NIOO_data, IndvID == "D534574")$Age_calculated[1], 4L)
  # #Test that age calculated is correct on 2017 capture (4 + 5*2 = 14 because it's an adult caught 5 years after its first capture)
  # expect_equal(subset(NIOO_data, IndvID == "D534574")$Age_calculated[6], 14L)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  # NIOO_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES"))
  #
  # #Test 1: Nestbox check
  # #Nestbox BP_001 should be type "NB"
  # expect_equal(subset(NIOO_data, LocationID == "BP_001")$LocationType, "NB")
  # #Nest boxes are not moved during the study, so LocationID and NestboxID should be identical
  # expect_equal(subset(NIOO_data, LocationID == "BP_001")$LocationID, subset(NIOO_data, LocationID == "BP_001")$NestboxID)

})
