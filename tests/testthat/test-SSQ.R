testthat::skip_if(!exists("data_path"))

pipeline_output <- format_SSQ(db = paste0(data_path, "/SSQ_SantoStefanoQuisquina_Italy"))

test_that("SSQ outputs all files...", {

  expect_true("SSQ" %in% pipeline_output$Brood_data$PopID)
  expect_true("SSQ" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("SSQ" %in% pipeline_output$Individual_data$PopID)
  expect_true("SSQ" %in% pipeline_output$Location_data$PopID)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex and each species for an adult and a chick
  #N.B. In this case, all individuals are listed as female (or sex unknown)

  #Take a subset of only SSQ data
  SSQ_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "SSQ")

  #Test 1: Adult blue tit
  #Individual AS29440 should be listed as a female blue tit
  expect_equal(subset(SSQ_data, IndvID == "AS29440")$Sex_calculated, "F")
  expect_equal(subset(SSQ_data, IndvID == "AS29440")$Species, "CYACAE")
  #She should have no BroodIDLaid or Fledged
  expect_equal(subset(SSQ_data, IndvID == "AS29440")$BroodIDLaid, NA_character_)
  expect_equal(subset(SSQ_data, IndvID == "AS29440")$BroodIDFledged, NA_character_)
  #Her ring season should be 2006 with a RingAge of 'adult'
  expect_equal(subset(SSQ_data, IndvID == "AS29440")$RingSeason, 2006)
  expect_equal(subset(SSQ_data, IndvID == "AS29440")$RingAge, "adult")

  #Test 2: Adult great tit
  #Individual LS30008 should be listed as a female great tit
  expect_equal(subset(SSQ_data, IndvID == "LS30008")$Sex_calculated, "F")
  expect_equal(subset(SSQ_data, IndvID == "LS30008")$Species, "PARMAJ")
  #She should have a BroodIDLaid and Fledged of 2011_030_039
  expect_equal(subset(SSQ_data, IndvID == "LS30008")$BroodIDLaid, "2011_030_039")
  expect_equal(subset(SSQ_data, IndvID == "LS30008")$BroodIDFledged, "2011_030_039")
  #Her ring season should be 2011 with a RingAge of 'chick'
  expect_equal(subset(SSQ_data, IndvID == "LS30008")$RingSeason, 2011)
  expect_equal(subset(SSQ_data, IndvID == "LS30008")$RingAge, "chick")

  #Test 3: Chick great tit
  #Individual LA44309 should be listed as a great tit with unknown sex (NA)
  expect_equal(subset(SSQ_data, IndvID == "LA44309")$Sex_calculated, NA_character_)
  expect_equal(subset(SSQ_data, IndvID == "LA44309")$Species, "PARMAJ")
  #She should have a BroodIDLaid and Fledged of 2006_002_051
  expect_equal(subset(SSQ_data, IndvID == "LA44309")$BroodIDLaid, "2006_002_051")
  expect_equal(subset(SSQ_data, IndvID == "LA44309")$BroodIDFledged, "2006_002_051")
  #Her ring season should be 2006 with a RingAge of 'chick'
  expect_equal(subset(SSQ_data, IndvID == "LA44309")$RingSeason, 2006)
  expect_equal(subset(SSQ_data, IndvID == "LA44309")$RingAge, "chick")

  #Test 4: Chick blue tit
  #Individual 6A33818 should be listed as a blue tit with unknown sex (NA)
  expect_equal(subset(SSQ_data, IndvID == "6A33818")$Sex_calculated, NA_character_)
  expect_equal(subset(SSQ_data, IndvID == "6A33818")$Species, "CYACAE")
  #She should have a BroodIDLaid and Fledged of 2011_026_043
  expect_equal(subset(SSQ_data, IndvID == "6A33818")$BroodIDLaid, "2011_026_043")
  expect_equal(subset(SSQ_data, IndvID == "6A33818")$BroodIDFledged, "2011_026_043")
  #Her ring season should be 2011 with a RingAge of 'chick'
  expect_equal(subset(SSQ_data, IndvID == "6A33818")$RingSeason, 2011)
  expect_equal(subset(SSQ_data, IndvID == "6A33818")$RingAge, "chick")

  #Test 5: Individual with conflicted species
  #Individual 25A9699 should be listed as a female with conflicting species
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$Sex_calculated, "F")
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$Species, "CCCCCC")
  #She should have a BroodIDLaid and Fledged of 2011_026_043
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$BroodIDLaid, NA_character_)
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$BroodIDFledged, NA_character_)
  #Her ring season should be 2011 with a RingAge of 'chick'
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$RingSeason, 2016)
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$RingAge, "adult")

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for great tit and blue tit broods
  #We want to manually check all possible outcomes of ClutchType_calculated

  #Take a subset of only SSQ data
  SSQ_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "SSQ")

  #Test 1: Blue tit brood clutch type = first
  #BroodID 2010_005_046 should be CYACAE
  expect_equal(subset(SSQ_data, BroodID == "2010_005_046")$Species, "CYACAE")
  #BroodID 2010_005_046 should have clutch type calculated 'first'
  expect_equal(subset(SSQ_data, BroodID == "2010_005_046")$ClutchType_calculated, "first")
  #Laying date should be "2010-04-15"
  expect_equal(subset(SSQ_data, BroodID == "2010_005_046")$LayDate_observed, as.Date("2010-04-15"))
  #Clutch size should be 8, BroodSize should be 0, NumberFledged should be 0
  expect_equal(subset(SSQ_data, BroodID == "2010_005_046")$ClutchSize_observed, 8)
  expect_equal(subset(SSQ_data, BroodID == "2010_005_046")$BroodSize_observed, 0)
  expect_equal(subset(SSQ_data, BroodID == "2010_005_046")$NumberFledged_observed, 0)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks
  expect_equal(subset(SSQ_data, BroodID == "2010_005_046")$AvgChickMass, NA_real_)
  expect_equal(subset(SSQ_data, BroodID == "2010_005_046")$AvgTarsus, NA_real_)

  #Test 2: Blue tit brood clutch type = replacement
  #BroodID 2010_006_061 should be CYACAE
  expect_equal(subset(SSQ_data, BroodID == "2010_006_061")$Species, "CYACAE")
  #BroodID 2010_005_046 should have clutch type calculated 'first'
  expect_equal(subset(SSQ_data, BroodID == "2010_006_061")$ClutchType_calculated, "replacement")
  #Laying date should be "2010-04-15"
  expect_equal(subset(SSQ_data, BroodID == "2010_006_061")$LayDate_observed, as.Date("2010-04-30"))
  #Clutch size should be 8, BroodSize should be 5, NumberFledged should be 5
  expect_equal(subset(SSQ_data, BroodID == "2010_006_061")$ClutchSize_observed, 8)
  expect_equal(subset(SSQ_data, BroodID == "2010_006_061")$BroodSize_observed, 7)
  expect_equal(subset(SSQ_data, BroodID == "2010_006_061")$NumberFledged_observed, 5)
  #AvgChickMass and AvgTarsus should be NA, there are no records for this
  expect_equal(subset(SSQ_data, BroodID == "2010_006_061")$AvgChickMass, NA_real_)
  expect_equal(subset(SSQ_data, BroodID == "2010_006_061")$AvgTarsus, NA_real_)

  #Test 3: Great tit brood clutch type = first
  #BroodID 2012_00X_044 should be PARMAJ
  expect_equal(subset(SSQ_data, BroodID == "2012_00X_044")$Species, "PARMAJ")
  #BroodID 2010_005_046 should have clutch type calculated 'first'
  expect_equal(subset(SSQ_data, BroodID == "2012_00X_044")$ClutchType_calculated, "first")
  #Laying date should be "2010-04-15"
  expect_equal(subset(SSQ_data, BroodID == "2012_00X_044")$LayDate_observed, as.Date("2012-04-13"))
  #Clutch size should be 8, BroodSize should be 0, NumberFledged should be 0
  expect_equal(subset(SSQ_data, BroodID == "2012_00X_044")$ClutchSize_observed, 8)
  expect_equal(subset(SSQ_data, BroodID == "2012_00X_044")$BroodSize_observed, 0)
  expect_equal(subset(SSQ_data, BroodID == "2012_00X_044")$NumberFledged_observed, 0)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks
  expect_equal(subset(SSQ_data, BroodID == "2012_00X_044")$AvgChickMass, NA_real_)
  expect_equal(subset(SSQ_data, BroodID == "2012_00X_044")$AvgTarsus, NA_real_)

  #Test 4: Great tit brood clutch type = replacement
  #BroodID 2012_004_059 should be CYACAE
  expect_equal(subset(SSQ_data, BroodID == "2012_004_059")$Species, "PARMAJ")
  #BroodID 2010_005_046 should have clutch type calculated 'first'
  expect_equal(subset(SSQ_data, BroodID == "2012_004_059")$ClutchType_calculated, "replacement")
  #Laying date should be "2010-04-15"
  expect_equal(subset(SSQ_data, BroodID == "2012_004_059")$LayDate_observed, as.Date("2012-04-28"))
  #Clutch size should be 8, BroodSize should be 8, NumberFledged should be 8
  expect_equal(subset(SSQ_data, BroodID == "2012_004_059")$ClutchSize_observed, 8)
  expect_equal(subset(SSQ_data, BroodID == "2012_004_059")$BroodSize_observed, 8)
  expect_equal(subset(SSQ_data, BroodID == "2012_004_059")$NumberFledged_observed, 8)
  #AvgChickMass and AvgTarsus should be NA, there are no records for this
  expect_equal(subset(SSQ_data, BroodID == "2012_004_059")$AvgChickMass, NA_real_)
  expect_equal(subset(SSQ_data, BroodID == "2012_004_059")$AvgTarsus, NA_real_)

})

test_that("Capture_data returns an expected outcome...", {

  #We want to run tests for great tit and blue tit capture as both chicks and adults

  #Take a subset of only SSQ data
  SSQ_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "SSQ")

  #Test 1: Great tit caught as a chick
  #Individual LS05355 should be caught at "2016-05-18" (laying date + clutch size + 27)
  expect_equal(subset(SSQ_data, IndvID == "LS05355")$CaptureDate[1], as.Date("2016-05-18"))
  #Age observed and calculated should be 1
  expect_equal(subset(SSQ_data, IndvID == "LS05355")$Age_observed[1], 1)
  expect_equal(subset(SSQ_data, IndvID == "LS05355")$Age_calculated[1], 1)

  #Test 2: Great tit caught as a adult
  #Individual LS05355 should be caught at "2017-05-03" (laying date of clutch)
  expect_equal(subset(SSQ_data, IndvID == "LS05355")$CaptureDate[2], as.Date("2017-05-03"))
  #Age observed should be 4, and Age calculated should be 5
  expect_equal(subset(SSQ_data, IndvID == "LS05355")$Age_observed[2], 5)
  expect_equal(subset(SSQ_data, IndvID == "LS05355")$Age_calculated[2], 5)

  #Test 3: Blue tit caught as a chick
  #Individual 6A33877 should be caught at "2016-05-18" (laying date + clutch size + 27)
  expect_equal(subset(SSQ_data, IndvID == "6A33877")$CaptureDate[1], as.Date("2011-05-20"))
  # #Age observed and calculated should be 1
  expect_equal(subset(SSQ_data, IndvID == "6A33877")$Age_observed[1], 1)
  expect_equal(subset(SSQ_data, IndvID == "6A33877")$Age_calculated[1], 1)

  #Test 4: Blue tit caught as a adult
  #Individual 8A07415 was caught for a second time at "2016-04-27" (laying date of clutch)
  expect_equal(subset(SSQ_data, IndvID == "8A07415")$CaptureDate[3], as.Date("2016-04-27"))
  #Age observed should be 4, and Age calculated should be 5 + 3*2 = 11
  expect_equal(subset(SSQ_data, IndvID == "8A07415")$Age_observed[3], NA_integer_)
  expect_equal(subset(SSQ_data, IndvID == "8A07415")$Age_calculated[3], 11)

  #Test 5: Blue tit caught as an adult (second time after being a chick)
  #Individual 6A33877 was caught for a second time at "2013-04-20" (laying date of clutch)
  expect_equal(subset(SSQ_data, IndvID == "6A33877")$CaptureDate[3], as.Date("2013-04-20"))
  #Age observed should be 4, and Age calculated should be 5 + 2 = 7
  expect_equal(subset(SSQ_data, IndvID == "6A33877")$Age_observed[3], 6)
  expect_equal(subset(SSQ_data, IndvID == "6A33877")$Age_calculated[3], 7)

  #Test 5: Individual classified as different species on 2 captures
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$CaptureDate, c(as.Date("2016-04-16"), as.Date("2017-04-15")))
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$Species, c("PARMAJ", "CYACAE"))
  #Age observed should be 4, and Age calculated should be 5 + 3*2 = 11
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$Age_observed, c(NA, 5))
  expect_equal(subset(SSQ_data, IndvID == "25A9699")$Age_calculated, c(4, 6))

})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only SSQ data
  SSQ_data <- dplyr::filter(pipeline_output$Location_data, PopID == "SSQ")

  #Test 1: Nestbox check
  #Nest box "011" should be type "NB"
  expect_equal(subset(SSQ_data, LocationID == "011")$LocationType, "NB")
  #Latitude and longitude should be 13.5 and 37.6
  expect_equal(round(subset(SSQ_data, LocationID == "011")$Latitude, 1) %>% setNames(nm = NULL), 13.5)
  expect_equal(round(subset(SSQ_data, LocationID == "011")$Longitude, 1) %>% setNames(nm = NULL), 37.6)

})
