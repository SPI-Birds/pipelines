context("Run data quality check on Velky Kosir pipeline output")

test_that("VEL outputs all files...", {

  expect_true("VEL" %in% pipeline_output$Brood_data$PopID)
  expect_true("VEL" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("VEL" %in% pipeline_output$Individual_data$PopID)
  expect_true("VEL" %in% pipeline_output$Location_data$PopID)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for individuals first caught as adult males, adult
  #females, and chicks for both flycatchers and tits Important to distinguish
  #these two species groups as the primary data is stored in two separate files.
  #N.B. Male tits is impossible. Only females were recorded.

  #Take a subset of only VEL data
  VEL_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "VEL")

  #Test 1: Adult tit female
  #Individual n550440 should be listed as a female great tit
  expect_equal(subset(VEL_data, IndvID == "n550440")$Sex, "F")
  expect_equal(subset(VEL_data, IndvID == "n550440")$Species, "PARMAJ")
  #She should have no BroodIDLaid or Fledged because there is no chick info
  expect_equal(subset(VEL_data, IndvID == "n550440")$BroodIDLaid, NA_character_)
  expect_equal(subset(VEL_data, IndvID == "n550440")$BroodIDFledged, NA_character_)
  #Her ring season should be 2013 with a RingAge of 'adult'
  expect_equal(subset(VEL_data, IndvID == "n550440")$RingSeason, 2013)
  expect_equal(subset(VEL_data, IndvID == "n550440")$RingAge, "adult")

  #Test 2: Adult flycatcher female
  #Individual s754396 should be listed as a female flycatcher
  expect_equal(subset(VEL_data, IndvID == "s754396")$Sex, "F")
  expect_equal(subset(VEL_data, IndvID == "s754396")$Species, "FICALB")
  #She should have same BroodIDLaid and Fledged (2016_089_05_05)
  expect_equal(subset(VEL_data, IndvID == "s754396")$BroodIDLaid, "2016_089_05_05")
  expect_equal(subset(VEL_data, IndvID == "s754396")$BroodIDFledged, "2016_089_05_05")
  #Her ring season should be 2016 with a RingAge of 'chick'
  expect_equal(subset(VEL_data, IndvID == "s754396")$RingSeason, 2016)
  expect_equal(subset(VEL_data, IndvID == "s754396")$RingAge, "chick")

  #Test 3: Adult flycatcher male
  #Individual s754714 should be listed as a male flycatcher
  expect_equal(subset(VEL_data, IndvID == "s754714")$Sex, "M")
  expect_equal(subset(VEL_data, IndvID == "s754714")$Species, "FICALB")
  #He should have same BroodIDLaid and Fledged (2016_036_11_05)
  expect_equal(subset(VEL_data, IndvID == "s754714")$BroodIDLaid, "2016_036_11_05")
  expect_equal(subset(VEL_data, IndvID == "s754714")$BroodIDFledged, "2016_036_11_05")
  #His ring season should be 2016 with a RingAge of 'chick'
  expect_equal(subset(VEL_data, IndvID == "s754714")$RingSeason, 2016)
  expect_equal(subset(VEL_data, IndvID == "s754714")$RingAge, "chick")

  #Test 4: Flycatcher caught only as chick
  #Individual t893658 should be listed as a flycatcher with no sex.
  #Never caught as an adult so never sexed
  expect_equal(subset(VEL_data, IndvID == "t893658")$Sex, NA_character_)
  expect_equal(subset(VEL_data, IndvID == "t893658")$Species, "FICALB")
  #Check BroodIDLaid and Fledged are the same (1998_041_10_05)
  expect_equal(subset(VEL_data, IndvID == "t893658")$BroodIDLaid, "1998_041_10_05")
  expect_equal(subset(VEL_data, IndvID == "t893658")$BroodIDFledged, "1998_041_10_05")
  #Check RingSeason and RingAge are as expected (1998, 'chick')
  expect_equal(subset(VEL_data, IndvID == "t893658")$RingSeason, 1998)
  expect_equal(subset(VEL_data, IndvID == "t893658")$RingAge, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated
  #In tits and flycatchers

  #Take a subset of only VEL data
  VEL_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "VEL")

  #Test 1: Tit brood where clutch type = first
  #BroodID 2001_006_20_04 should be PARMAJ
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$Species, "PARMAJ")
  #BroodID 2017_BP_010_16_04 should have clutch type calculated 'first'
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$ClutchType_calculated, "first")
  #Check laying date is as expected (2001-04-20)
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$LayDate, as.Date("2001-04-20"))
  #Check clutch size, brood size, and number fledged is as expected (11, NA, 11)
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$ClutchSize, 11)
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$BroodSize, NA_integer_)
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$NumberFledged, 11)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$AvgChickMass, NA_real_)
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$AvgTarsus, NA_real_)

  #Test 2: Tit brood where clutch type = second
  #BroodID 2013_004_14_06 should be PARMAJ
  expect_equal(subset(VEL_data, BroodID == "2013_004_14_06")$Species, "PARMAJ")
  #BroodID 2013_004_14_06 should have clutch type calculated 'replacement'
  expect_equal(subset(VEL_data, BroodID == "2013_004_14_06")$ClutchType_calculated, "second")
  #Check laying date is as expected (2013-06-14)
  expect_equal(subset(VEL_data, BroodID == "2013_004_14_06")$LayDate, as.Date("2013-06-14"))
  #Check clutch size, brood size, and number fledged is as expected (2, NA, NA)
  expect_equal(subset(VEL_data, BroodID == "2013_004_14_06")$ClutchSize, 2)
  expect_equal(subset(VEL_data, BroodID == "2013_004_14_06")$BroodSize, NA_integer_)
  expect_equal(subset(VEL_data, BroodID == "2013_004_14_06")$NumberFledged, NA_integer_)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$AvgChickMass, NA_real_)
  expect_equal(subset(VEL_data, BroodID == "2001_006_20_04")$AvgTarsus, NA_real_)

  #Test 3: Tit brood where clutch type = replacement (due to past cutoff)
  #BroodID 2013_104_24_06 should be PARMAJ
  expect_equal(subset(VEL_data, BroodID == "2013_104_24_06")$Species, "PARMAJ")
  #BroodID 2013_004_14_06 should have clutch type calculated 'replacement'
  expect_equal(subset(VEL_data, BroodID == "2013_104_24_06")$ClutchType_calculated, "replacement")
  #Check laying date is as expected (2013-06-24)
  expect_equal(subset(VEL_data, BroodID == "2013_104_24_06")$LayDate, as.Date("2013-06-24"))
  #Check clutch size, brood size, and number fledged is as expected (11, NA, 11)
  expect_equal(subset(VEL_data, BroodID == "2013_104_24_06")$ClutchSize, 11)
  expect_equal(subset(VEL_data, BroodID == "2013_104_24_06")$BroodSize, NA_integer_)
  expect_equal(subset(VEL_data, BroodID == "2013_104_24_06")$NumberFledged, 11)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(VEL_data, BroodID == "2013_104_24_06")$AvgChickMass, NA_real_)
  expect_equal(subset(VEL_data, BroodID == "2013_104_24_06")$AvgTarsus, NA_real_)

  #Test 4: Tit brood where clutch type = replacement (due to past cutoff)
  #BroodID 2013_043_27_04 should be PARMAJ
  expect_equal(subset(VEL_data, BroodID == "2013_043_27_04")$Species, "PARMAJ")
  #BroodID 2013_004_14_06 should have clutch type calculated 'replacement'
  expect_equal(subset(VEL_data, BroodID == "2013_043_27_04")$ClutchType_calculated, "second")
  #Check laying date is as expected (2013-04-27)
  expect_equal(subset(VEL_data, BroodID == "2013_043_27_04")$LayDate, as.Date("2013-04-27"))
  #Check clutch size, brood size, and number fledged is as expected (10, NA, 10)
  expect_equal(subset(VEL_data, BroodID == "2013_043_27_04")$ClutchSize, 10)
  expect_equal(subset(VEL_data, BroodID == "2013_043_27_04")$BroodSize, NA_integer_)
  expect_equal(subset(VEL_data, BroodID == "2013_043_27_04")$NumberFledged, 10)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(VEL_data, BroodID == "2013_043_27_04")$AvgChickMass, NA_real_)
  expect_equal(subset(VEL_data, BroodID == "2013_043_27_04")$AvgTarsus, NA_real_)

  #Test 5: Flycatcher brood where clutch type = first
  #BroodID 2009_053_07_05 should be FICALB
  expect_equal(subset(VEL_data, BroodID == "2009_053_07_05")$Species, "FICALB")
  #BroodID 2017_BP_010_16_04 should have clutch type calculated 'first'
  expect_equal(subset(VEL_data, BroodID == "2009_053_07_05")$ClutchType_calculated, "first")
  #Check laying date is as expected (2009-05-07)
  expect_equal(subset(VEL_data, BroodID == "2009_053_07_05")$LayDate, as.Date("2009-05-07"))
  #Check clutch size, brood size, and number fledged is as expected (11, NA, 11)
  expect_equal(subset(VEL_data, BroodID == "2009_053_07_05")$ClutchSize, 6)
  expect_equal(subset(VEL_data, BroodID == "2009_053_07_05")$BroodSize, 0)
  expect_equal(subset(VEL_data, BroodID == "2009_053_07_05")$NumberFledged, 0)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(VEL_data, BroodID == "2009_053_07_05")$AvgChickMass, NA_real_)
  expect_equal(subset(VEL_data, BroodID == "2009_053_07_05")$AvgTarsus, NA_real_)

  #Test 6: Flycatcher brood where clutch type = replacement (due to previous unsuccessful clutch)
  #BroodID 2009_035_02_06 should be PARMAJ
  expect_equal(subset(VEL_data, BroodID == "2009_035_02_06")$Species, "FICALB")
  #BroodID 2009_035_02_06 should have clutch type calculated 'replacement'
  expect_equal(subset(VEL_data, BroodID == "2009_035_02_06")$ClutchType_calculated, "replacement")
  #Check laying date is as expected (2009-06-02)
  expect_equal(subset(VEL_data, BroodID == "2009_035_02_06")$LayDate, as.Date("2009-06-02"))
  #Check clutch size, brood size, and number fledged is as expected (2, NA, NA)
  expect_equal(subset(VEL_data, BroodID == "2009_035_02_06")$ClutchSize, 5)
  expect_equal(subset(VEL_data, BroodID == "2009_035_02_06")$BroodSize, 5)
  expect_equal(subset(VEL_data, BroodID == "2009_035_02_06")$NumberFledged, 3)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(VEL_data, BroodID == "2009_035_02_06")$AvgChickMass, NA_real_)
  expect_equal(subset(VEL_data, BroodID == "2009_035_02_06")$AvgTarsus, NA_real_)

  #Test 7: Tit brood where clutch type = replacement (due to past cutoff)
  #BroodID 1998_027_05_06 should be FICALB
  expect_equal(subset(VEL_data, BroodID == "1998_027_05_06")$Species, "FICALB")
  #BroodID 1998_027_05_06 should have clutch type calculated 'replacement'
  expect_equal(subset(VEL_data, BroodID == "1998_027_05_06")$ClutchType_calculated, "replacement")
  #Check laying date is as expected (1998-06-05)
  expect_equal(subset(VEL_data, BroodID == "1998_027_05_06")$LayDate, as.Date("1998-06-05"))
  #Check clutch size, brood size, and number fledged is as expected (3, 3, 0)
  expect_equal(subset(VEL_data, BroodID == "1998_027_05_06")$ClutchSize, 3)
  expect_equal(subset(VEL_data, BroodID == "1998_027_05_06")$BroodSize, 3)
  expect_equal(subset(VEL_data, BroodID == "1998_027_05_06")$NumberFledged, 0)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(VEL_data, BroodID == "1998_027_05_06")$AvgChickMass, NA_real_)
  expect_equal(subset(VEL_data, BroodID == "1998_027_05_06")$AvgTarsus, NA_real_)

  #Test 8: Flycatcher brood where clutch type = second
  #BroodID 2010_026_23_05 should be PARMAJ
  expect_equal(subset(VEL_data, BroodID == "2010_026_23_05")$Species, "FICALB")
  #BroodID 2010_026_23_05 should have clutch type calculated 'second'
  expect_equal(subset(VEL_data, BroodID == "2010_026_23_05")$ClutchType_calculated, "second")
  #Check laying date is as expected (2013-04-27)
  expect_equal(subset(VEL_data, BroodID == "2010_026_23_05")$LayDate, as.Date("2010-05-23"))
  #Check clutch size, brood size, and number fledged is as expected (4, 4, 4)
  expect_equal(subset(VEL_data, BroodID == "2010_026_23_05")$ClutchSize, 4)
  expect_equal(subset(VEL_data, BroodID == "2010_026_23_05")$BroodSize, 4)
  expect_equal(subset(VEL_data, BroodID == "2010_026_23_05")$NumberFledged, 4)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(VEL_data, BroodID == "2010_026_23_05")$AvgChickMass, NA_real_)
  expect_equal(subset(VEL_data, BroodID == "2010_026_23_05")$AvgTarsus, NA_real_)

  #Test 9: Flycatcher brood where chick measurements were taken
  #BroodID 2000_021_08_05 should be PARMAJ
  expect_equal(subset(VEL_data, BroodID == "2000_021_08_05")$Species, "FICALB")
  #BroodID 2000_021_08_05 should have clutch type calculated 'first'
  expect_equal(subset(VEL_data, BroodID == "2000_021_08_05")$ClutchType_calculated, "first")
  #Check laying date is as expected (2010-05-08)
  expect_equal(subset(VEL_data, BroodID == "2000_021_08_05")$LayDate, as.Date("2000-05-08"))
  #Check clutch size, brood size, and number fledged is as expected (5, 5, 5)
  expect_equal(subset(VEL_data, BroodID == "2000_021_08_05")$ClutchSize, 5)
  expect_equal(subset(VEL_data, BroodID == "2000_021_08_05")$BroodSize, 5)
  expect_equal(subset(VEL_data, BroodID == "2000_021_08_05")$NumberFledged, 5)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(round(subset(VEL_data, BroodID == "2000_021_08_05")$AvgChickMass, 1), 15.9)
  expect_equal(round(subset(VEL_data, BroodID == "2000_021_08_05")$AvgTarsus, 1), 18.1)

})

test_that("Capture_data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females for both tits and flycatchers
  #Currently, chicks and males are only available in flycatchers

  #Take a subset of only VEL data
  VEL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "VEL")

  #Test 1: Female tit caught as adult
  #Test the female has the correct number of capture records (2)
  expect_equal(nrow(subset(VEL_data, IndvID == "n550440")), 2)
  #Test that the first capture of the female is as expected (05/05/2013) <- laying date + clutch size
  expect_equal(min(subset(VEL_data, IndvID == "n550440")$CaptureDate, na.rm = TRUE), as.Date("2013-05-05"))
  #Test that the 2nd capture of the female is as expcted (2013-06-16)
  expect_equal(subset(VEL_data, IndvID == "n550440")$CaptureDate[2], as.Date("2013-06-16"))
  #Test that age observed is as expected (4, because we just know it was caught as an adult, that's all)
  expect_equal(subset(VEL_data, IndvID == "n550440")$Age_observed[1], 4L)
  #Test that age calculated is correct on first capture (4 because it's an adult of unknown age)
  expect_equal(subset(VEL_data, IndvID == "n550440")$Age_calculated[1], 4L)
  #Test that age calculated is correct on the 2nd capture (4)
  expect_equal(subset(VEL_data, IndvID == "n550440")$Age_calculated[2], 4L)

  #Test 2: Female caught as adult
  #Test the female has the correct number of capture records (3)
  expect_equal(nrow(subset(VEL_data, IndvID == "cs8621")), 3)
  #Test that the first capture of the male is as expected (1999-06-11)
  expect_equal(min(subset(VEL_data, IndvID == "cs8621")$CaptureDate, na.rm = TRUE), as.Date("1999-06-11"))
  #Test that the 3rd capture is as expected (2001-06-02)
  expect_equal(subset(VEL_data, IndvID == "cs8621")$CaptureDate[3], as.Date("2001-06-02"))
  #Test that age observed is as expected (NA, because there is no recorded age column for adults)
  expect_equal(subset(VEL_data, IndvID == "cs8621")$Age_observed[1], NA_integer_)
  #Test that age calculated is correct on first capture (4 because it's an adult of unknown age)
  expect_equal(subset(VEL_data, IndvID == "cs8621")$Age_calculated[1], 4L)
  #Test that age calculated is correct on 3rd capture (4 + 2*2 = 8 because it's an adult caught 2 years after its first capture)
  expect_equal(subset(VEL_data, IndvID == "cs8621")$Age_calculated[3], 8L)

  #Test 3: Male caught as adult
  #Test the individual has the correct number of capture records (5)
  expect_equal(nrow(subset(VEL_data, IndvID == "f17690")), 5)
  #Test that the first capture is as expected (2004-06-02)
  expect_equal(min(subset(VEL_data, IndvID == "f17690")$CaptureDate, na.rm = TRUE), as.Date("2004-06-02"))
  #Test that the 5th capture is as expected (2008-05-26)
  expect_equal(subset(VEL_data, IndvID == "f17690")$CaptureDate[5], as.Date("2008-05-26"))
  #Test that age observed is as expected (6, because it's listed as 'old' (>2nd year))
  expect_equal(subset(VEL_data, IndvID == "f17690")$Age_observed[1], 6L)
  #Test that age calculated is correct on first capture (4 because it's an adult of unknown age)
  expect_equal(subset(VEL_data, IndvID == "f17690")$Age_calculated[1], 4L)
  #Test that age calculated is correct on 3rd capture (4 + 4*2 = 12 because it's an adult caught 2 years after its first capture)
  expect_equal(subset(VEL_data, IndvID == "f17690")$Age_calculated[3], 8L)

  #Test 4: Indvidual caught as chick
  #Test the individual has the correct number of capture records (3)
  expect_equal(nrow(subset(VEL_data, IndvID == "s754396")), 3)
  #Test that the first capture is as expected (2016-05-28) <- caught 6 days after hatching date
  expect_equal(min(subset(VEL_data, IndvID == "s754396")$CaptureDate, na.rm = TRUE), as.Date("2016-05-28"))
  #Test that the 2nd capture is as expected (2018-05-30)
  expect_equal(subset(VEL_data, IndvID == "s754396")$CaptureDate[2], as.Date("2018-05-30"))
  #Test that age observed is as expected when first caught (1, because it's caught as a chick)
  expect_equal(subset(VEL_data, IndvID == "s754396")$Age_observed[1], 1L)
  #Test that age calculated is correct on first capture (1 because it's a chick)
  expect_equal(subset(VEL_data, IndvID == "s754396")$Age_calculated[1], 1L)
  #Test that age calculated is correct on 2nd capture (3 + 2*2 = 7 because it's an adult caught 2 years after its first capture)
  expect_equal(subset(VEL_data, IndvID == "s754396")$Age_calculated[2], 7L)

  message("We expect some errors in VEL pipeline because we haven't yet dealt with cases where a chick was measured only once (i.e. day 6 but not 13)")

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only VEL data
  VEL_data <- dplyr::filter(pipeline_output$Location_data, PopID == "VEL")

  #Test 1: Nestbox deciduous
  #LocationType is as expected
  expect_equal(subset(VEL_data, LocationID == "B1")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(VEL_data, LocationID == "B1")$Habitat, "deciduous")
  #Latitude and Longitude are as expected
  #We use 3 decimal places to clearly distinguish close nest boxes
  expect_equal(round(subset(VEL_data, LocationID == "B1")$Latitude, 3), 49.552)
  expect_equal(round(subset(VEL_data, LocationID == "B1")$Longitude, 3), 17.044)

  #Test 1: Nestbox evergreen
  #LocationType is as expected
  expect_equal(subset(VEL_data, LocationID == "E9")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(VEL_data, LocationID == "E9")$Habitat, "evergreen")
  #Latitude and Longitude are as expected
  #We use 3 decimal places to clearly distinguish close nest boxes
  expect_equal(round(subset(VEL_data, LocationID == "E9")$Latitude, 3), 49.552)
  expect_equal(round(subset(VEL_data, LocationID == "E9")$Longitude, 3), 17.067)



})
