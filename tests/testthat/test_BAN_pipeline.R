context("Run data quality check on Bandon Valley pipeline output")

test_that("BAN outputs all files...", {

  expect_true("BAN" %in% pipeline_output$Brood_data$PopID)
  expect_true("BAN" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("BAN" %in% pipeline_output$Individual_data$PopID)
  expect_true("BAN" %in% pipeline_output$Location_data$PopID)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex and each species for an adult and a chick
  #Currently, no chick data is available

  #Take a subset of only BAN data
  BAN_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "BAN")

  #Test 1: Adult blue tit female
  #Individual A057192 should be listed as a female blue tit
  expect_equal(subset(BAN_data, IndvID == "A057192")$Sex, "F")
  expect_equal(subset(BAN_data, IndvID == "A057192")$Species, "CYACAE")
  #She should have no BroodIDLaid or Fledged because there is no chick info
  expect_equal(subset(BAN_data, IndvID == "A057192")$BroodIDLaid, NA_character_)
  expect_equal(subset(BAN_data, IndvID == "A057192")$BroodIDFledged, NA_character_)
  #Her ring season should be 2014 with a RingAge of 'adult'
  expect_equal(subset(BAN_data, IndvID == "A057192")$RingSeason, 2014)
  expect_equal(subset(BAN_data, IndvID == "A057192")$RingAge, "adult")

  #Test 2: Adult great tit female
  #Individual D534573 should be listed as a female great tit
  expect_equal(subset(BAN_data, IndvID == "D534573")$Sex, "F")
  expect_equal(subset(BAN_data, IndvID == "D534573")$Species, "PARMAJ")
  #She should have no BroodIDLaid or Fledged because there is no chick info
  expect_equal(subset(BAN_data, IndvID == "D534573")$BroodIDLaid, NA_character_)
  expect_equal(subset(BAN_data, IndvID == "D534573")$BroodIDFledged, NA_character_)
  #Her ring season should be 2011 with a RingAge of 'chick'
  expect_equal(subset(BAN_data, IndvID == "D534573")$RingSeason, 2013)
  expect_equal(subset(BAN_data, IndvID == "D534573")$RingAge, "adult")

  #Test 3: Adult blue tit male
  #Individual D534700 should be listed as a male blue tit
  expect_equal(subset(BAN_data, IndvID == "D534700")$Sex, "M")
  expect_equal(subset(BAN_data, IndvID == "D534700")$Species, "CYACAE")
  #She should have no BroodIDLaid or Fledged because there is no chick info
  expect_equal(subset(BAN_data, IndvID == "D534700")$BroodIDLaid, NA_character_)
  expect_equal(subset(BAN_data, IndvID == "D534700")$BroodIDFledged, NA_character_)
  #Her ring season should be 2014 with a RingAge of 'adult'
  expect_equal(subset(BAN_data, IndvID == "D534700")$RingSeason, 2014)
  expect_equal(subset(BAN_data, IndvID == "D534700")$RingAge, "adult")

  #Test 4: Adult great tit male
  #Individual D534574 should be listed as a female great tit
  expect_equal(subset(BAN_data, IndvID == "D534574")$Sex, "M")
  expect_equal(subset(BAN_data, IndvID == "D534574")$Species, "PARMAJ")
  #She should have no BroodIDLaid or Fledged because there is no chick info
  expect_equal(subset(BAN_data, IndvID == "D534574")$BroodIDLaid, NA_character_)
  expect_equal(subset(BAN_data, IndvID == "D534574")$BroodIDFledged, NA_character_)
  #Her ring season should be 2011 with a RingAge of 'chick'
  expect_equal(subset(BAN_data, IndvID == "D534574")$RingSeason, 2013)
  expect_equal(subset(BAN_data, IndvID == "D534574")$RingAge, "adult")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only BAN data
  BAN_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "BAN")

  #Test 1: Great tit brood clutch type = first
  #BroodID 2017_BP_010_16_04 should be PARMAJ
  expect_equal(subset(BAN_data, BroodID == "2017_BP_010_16_04")$Species, "PARMAJ")
  #BroodID 2017_BP_010_16_04 should have clutch type calculated 'first'
  expect_equal(subset(BAN_data, BroodID == "2017_BP_010_16_04")$ClutchType_calculated, "first")
  #Laying date should be "2010-04-15"
  expect_equal(subset(BAN_data, BroodID == "2017_BP_010_16_04")$LayingDate, as.Date("2017-04-16"))
  #Clutch size should be 8, BroodSize should be 0, NumberFledged should be 0
  expect_equal(subset(BAN_data, BroodID == "2017_BP_010_16_04")$ClutchSize, 6)
  expect_equal(subset(BAN_data, BroodID == "2017_BP_010_16_04")$BroodSize, NA_integer_)
  expect_equal(subset(BAN_data, BroodID == "2017_BP_010_16_04")$NumberFledged, 4)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks
  expect_equal(subset(BAN_data, BroodID == "2017_BP_010_16_04")$AvgChickMass, NA_real_)
  expect_equal(subset(BAN_data, BroodID == "2017_BP_010_16_04")$AvgTarsus, NA_real_)

  #Test 2: Great tit brood clutch type = second
  #BroodID 2017_BP_027_13_05 should be PARMAJ
  expect_equal(subset(BAN_data, BroodID == "2017_BP_027_13_05")$Species, "PARMAJ")
  #BroodID 2017_BP_010_16_04 should have clutch type calculated 'second' (clutch tested above was successful)
  expect_equal(subset(BAN_data, BroodID == "2017_BP_027_13_05")$ClutchType_calculated, "second")
  #Laying date should be "2010-04-15"
  expect_equal(subset(BAN_data, BroodID == "2017_BP_027_13_05")$LayingDate, as.Date("2017-05-13"))
  #Clutch size should be 8, BroodSize should be 0, NumberFledged should be 0
  expect_equal(subset(BAN_data, BroodID == "2017_BP_027_13_05")$ClutchSize, 7)
  expect_equal(subset(BAN_data, BroodID == "2017_BP_027_13_05")$BroodSize, NA_integer_)
  expect_equal(subset(BAN_data, BroodID == "2017_BP_027_13_05")$NumberFledged, 6)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks
  expect_equal(subset(BAN_data, BroodID == "2017_BP_027_13_05")$AvgChickMass, NA_real_)
  expect_equal(subset(BAN_data, BroodID == "2017_BP_027_13_05")$AvgTarsus, NA_real_)

  #Test 3: Great tit brood clutch type = replacement, where replacement is known (i.e. previous clutch was seen)
  #BroodID 2015_BC_024_05_06 should be PARMAJ
  expect_equal(subset(BAN_data, BroodID == "2015_BC_024_05_06")$Species, "PARMAJ")
  #BroodID 2017_BP_010_16_04 should have clutch type calculated 'replacement' (clutch 2015_BC_026_24_04 had no fledlings)
  expect_equal(subset(BAN_data, BroodID == "2015_BC_024_05_06")$ClutchType_calculated, "replacement")
  #Laying date should be "2010-04-15"
  expect_equal(subset(BAN_data, BroodID == "2015_BC_024_05_06")$LayingDate, as.Date("2015-06-05"))
  #Clutch size should be 8, BroodSize should be 0, NumberFledged should be 0
  expect_equal(subset(BAN_data, BroodID == "2015_BC_024_05_06")$ClutchSize, 5)
  expect_equal(subset(BAN_data, BroodID == "2015_BC_024_05_06")$BroodSize, NA_integer_)
  expect_equal(subset(BAN_data, BroodID == "2015_BC_024_05_06")$NumberFledged, 0)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks
  expect_equal(subset(BAN_data, BroodID == "2015_BC_024_05_06")$AvgChickMass, NA_real_)
  expect_equal(subset(BAN_data, BroodID == "2015_BC_024_05_06")$AvgTarsus, NA_real_)

  #Test 4: Great tit brood clutch type = replacement, where replacement calculated from cutoff
  #BroodID 2014_KB_028_14_06 should be PARMAJ
  expect_equal(subset(BAN_data, BroodID == "2014_KB_028_14_06")$Species, "PARMAJ")
  #BroodID 2017_BP_010_16_04 should have clutch type calculated 'replacement' (laying date is > cutoff)
  expect_equal(subset(BAN_data, BroodID == "2014_KB_028_14_06")$ClutchType_calculated, "replacement")
  #Laying date should be "2010-04-15"
  expect_equal(subset(BAN_data, BroodID == "2014_KB_028_14_06")$LayingDate, as.Date("2014-06-14"))
  #Clutch size should be 8, BroodSize should be 0, NumberFledged should be 0
  expect_equal(subset(BAN_data, BroodID == "2014_KB_028_14_06")$ClutchSize, 4)
  expect_equal(subset(BAN_data, BroodID == "2014_KB_028_14_06")$BroodSize, NA_integer_)
  expect_equal(subset(BAN_data, BroodID == "2014_KB_028_14_06")$NumberFledged, 2)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks
  expect_equal(subset(BAN_data, BroodID == "2014_KB_028_14_06")$AvgChickMass, NA_real_)
  expect_equal(subset(BAN_data, BroodID == "2014_KB_028_14_06")$AvgTarsus, NA_real_)

})

test_that("Capture_data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only BAN data
  BAN_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "BAN")

  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (6)
  expect_equal(nrow(subset(BAN_data, IndvID == "D534573")), 6)
  #Test that the first capture of the female is as expected (02/06/2013)
  expect_equal(min(subset(BAN_data, IndvID == "D534573")$CaptureDate, na.rm = TRUE), as.Date("2013-06-02"))
  #Test that the 5th capture of the female is as expcted (2017-05-12) (6th one is NA so no good to test)
  expect_equal(subset(BAN_data, IndvID == "D534573")$CaptureDate[5], as.Date("2017-05-12"))
  #Test that age observed is as expected (NA, because we just know it was caught as an adult, that's all)
  expect_equal(subset(BAN_data, IndvID == "D534573")$Age_observed[1], NA_integer_)
  #Test that age calculated is correct on first capture (4 because it's an adult of unknown age)
  expect_equal(subset(BAN_data, IndvID == "D534573")$Age_calculated[1], 4L)
  #Test that age calculated is correct on 2017 capture (4 + 4*2 = 12 because it's an adult caught 4 years after its first capture)
  expect_equal(subset(BAN_data, IndvID == "D534573")$Age_calculated[6], 12L)

  #Test 1: Male caught as adult
  #Test the male has the correct number of capture records (6)
  expect_equal(nrow(subset(BAN_data, IndvID == "D534574")), 6)
  #Test that the first capture of the male is as expected (2013-06-02)
  expect_equal(min(subset(BAN_data, IndvID == "D534574")$CaptureDate, na.rm = TRUE), as.Date("2013-06-02"))
  #Test that the 6th capture of the male is as expcted (2018-06-08)
  expect_equal(subset(BAN_data, IndvID == "D534574")$CaptureDate[6], as.Date("2018-06-08"))
  #Test that age observed is as expected (NA, because we just know it was caught as an adult, that's all)
  expect_equal(subset(BAN_data, IndvID == "D534574")$Age_observed[1], NA_integer_)
  #Test that age calculated is correct on first capture (4 because it's an adult of unknown age)
  expect_equal(subset(BAN_data, IndvID == "D534574")$Age_calculated[1], 4L)
  #Test that age calculated is correct on 2017 capture (4 + 5*2 = 14 because it's an adult caught 5 years after its first capture)
  expect_equal(subset(BAN_data, IndvID == "D534574")$Age_calculated[6], 14L)

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only BAN data
  BAN_data <- dplyr::filter(pipeline_output$Location_data, PopID == "BAN")

  #Test 1: Nestbox check
  #Nestbox BP_001 should be type "NB"
  expect_equal(subset(BAN_data, LocationID == "BP_001")$LocationType, "NB")
  #Nest boxes are not moved during the study, so LocationID and NestboxID should be identical
  expect_equal(subset(BAN_data, LocationID == "BP_001")$LocationID, subset(BAN_data, LocationID == "BP_001")$NestboxID)

})
