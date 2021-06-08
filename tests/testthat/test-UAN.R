pipeline_output <- format_UAN(db = paste0(data_path, "/UAN_UAntwerpEvolutionaryEcologyGrp_Belgium"))

test_that("UAN outputs all files...", {

  expect_true(all(c("PEE", "BOS") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("PEE", "BOS") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("PEE", "BOS") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("PEE", "BOS") %in% pipeline_output$Location_data$PopID))

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only UAN data
  UAN_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("PEE", "BOS"))

  #Test 1: Brood where clutch type = first
  expect_equal(subset(UAN_data, BroodID == "20160067")$Species, "CYACAE")
  expect_equal(subset(UAN_data, BroodID == "20160067")$ClutchType_calculated, "first")
  expect_equal(subset(UAN_data, BroodID == "20160067")$LayDate, as.Date("2016-04-04"))
  expect_equal(subset(UAN_data, BroodID == "20160067")$ClutchSize, 13)
  expect_equal(subset(UAN_data, BroodID == "20160067")$BroodSize, 13)
  expect_equal(subset(UAN_data, BroodID == "20160067")$NumberFledged, 11)
  expect_equal(round(subset(UAN_data, BroodID == "20160067")$AvgChickMass, 1), 11.3)
  expect_equal(subset(UAN_data, BroodID == "20160067")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(UAN_data, BroodID == "20170318")$Species, "PARMAJ")
  expect_equal(subset(UAN_data, BroodID == "20170318")$ClutchType_calculated, "replacement")
  expect_equal(subset(UAN_data, BroodID == "20170318")$LayDate, as.Date("2017-05-18"))
  expect_equal(subset(UAN_data, BroodID == "20170318")$ClutchSize, 9)
  expect_equal(subset(UAN_data, BroodID == "20170318")$BroodSize, 9)
  expect_equal(subset(UAN_data, BroodID == "20170318")$NumberFledged, 3)
  expect_equal(round(subset(UAN_data, BroodID == "20170318")$AvgChickMass, 1), 15.5)
  expect_equal(subset(UAN_data, BroodID == "20170318")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(UAN_data, BroodID == "2013320")$Species, "PARMAJ")
  expect_equal(subset(UAN_data, BroodID == "2013320")$ClutchType_calculated, "replacement")
  expect_equal(subset(UAN_data, BroodID == "2013320")$LayDate, as.Date("2013-06-01"))
  expect_equal(subset(UAN_data, BroodID == "2013320")$ClutchSize, 1)
  expect_equal(subset(UAN_data, BroodID == "2013320")$BroodSize, 0)
  expect_equal(subset(UAN_data, BroodID == "2013320")$NumberFledged, 0)
  expect_equal(subset(UAN_data, BroodID == "2013320")$AvgChickMass, NA_real_)
  expect_equal(subset(UAN_data, BroodID == "2013320")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(UAN_data, BroodID == "20150180")$Species, "PARMAJ")
  expect_equal(subset(UAN_data, BroodID == "20150180")$ClutchType_calculated, "second")
  expect_equal(subset(UAN_data, BroodID == "20150180")$LayDate, as.Date("2015-05-19"))
  expect_equal(subset(UAN_data, BroodID == "20150180")$ClutchSize, 5)
  expect_equal(subset(UAN_data, BroodID == "20150180")$BroodSize, 5)
  expect_equal(subset(UAN_data, BroodID == "20150180")$NumberFledged, 5)
  expect_equal(subset(UAN_data, BroodID == "20150180")$AvgChickMass, NA_real_)
  expect_equal(subset(UAN_data, BroodID == "20150180")$AvgTarsus, NA_real_)

  #Test 5: Brood where AvgChickMass is estimated from Capture_data and replace recorded value
  expect_equal(subset(UAN_data, BroodID == "20180002")$Species, "CYACAE")
  expect_equal(subset(UAN_data, BroodID == "20180002")$ClutchType_calculated, "first")
  expect_equal(subset(UAN_data, BroodID == "20180002")$LayDate, as.Date("2018-04-18"))
  expect_equal(subset(UAN_data, BroodID == "20180002")$ClutchSize, 8)
  expect_equal(subset(UAN_data, BroodID == "20180002")$BroodSize, 8)
  expect_equal(subset(UAN_data, BroodID == "20180002")$NumberFledged, 3)
  #This should be different to the recorded AvgChickMass because one chicks is 13 days (we only consider 14 - 16)
  expect_equal(round(subset(UAN_data, BroodID == "20180002")$AvgChickMass, 2), 9.75)
  expect_equal(subset(UAN_data, BroodID == "20180002")$AvgTarsus, NA_real_)

  #Test 6: Brood where AvgTarsus needs to be converted
  expect_equal(subset(UAN_data, BroodID == "1950359")$Species, "PARMAJ")
  expect_equal(subset(UAN_data, BroodID == "1950359")$ClutchType_calculated, "replacement")
  expect_equal(subset(UAN_data, BroodID == "1950359")$LayDate, as.Date("1996-04-14"))
  expect_equal(subset(UAN_data, BroodID == "1950359")$ClutchSize, 6)
  expect_equal(subset(UAN_data, BroodID == "1950359")$BroodSize, 6)
  expect_equal(subset(UAN_data, BroodID == "1950359")$NumberFledged, 6)
  expect_equal(round(subset(UAN_data, BroodID == "1950359")$AvgChickMass, 2), 18.47)
  expect_equal(round(subset(UAN_data, BroodID == "1950359")$AvgTarsus, 2), 20.49)

  #Test 7: Brood where AvgTarsus is estimated from Capture_data
  expect_equal(subset(UAN_data, BroodID == "20140778")$Species, "PARMAJ")
  expect_equal(subset(UAN_data, BroodID == "20140778")$ClutchType_calculated, "first")
  expect_equal(subset(UAN_data, BroodID == "20140778")$LayDate, as.Date("2014-03-28"))
  expect_equal(subset(UAN_data, BroodID == "20140778")$ClutchSize, 9)
  expect_equal(subset(UAN_data, BroodID == "20140778")$BroodSize, 9)
  expect_equal(subset(UAN_data, BroodID == "20140778")$NumberFledged, 7)
  expect_equal(round(subset(UAN_data, BroodID == "20140778")$AvgChickMass, 2), 13.33)
  expect_equal(round(subset(UAN_data, BroodID == "20140778")$AvgTarsus, 2), 19.12)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only UAN data
  UAN_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("PEE", "BOS"))

  #Test 1: Male caught first as chick
  #Expected sexand species
  expect_equal(subset(UAN_data, IndvID == "10277110")$Sex, "M")
  expect_equal(subset(UAN_data, IndvID == "10277110")$Species, "CYACAE")
  #Expected BroodIDLaid and Fledged
  expect_equal(subset(UAN_data, IndvID == "10277110")$BroodIDLaid, "2008199")
  expect_equal(subset(UAN_data, IndvID == "10277110")$BroodIDFledged, "2008199")
  #Her ring season should be 1991 with a RingAge of 'adult'
  expect_equal(subset(UAN_data, IndvID == "10277110")$RingSeason, 2008L)
  expect_equal(subset(UAN_data, IndvID == "10277110")$RingAge, "chick")

  #Test 2: Caught first as adult
  #Individual 2546616 should be listed as a female blue tit
  expect_equal(subset(UAN_data, IndvID == "1002599")$Sex, "M")
  expect_equal(subset(UAN_data, IndvID == "1002599")$Species, "CYACAE")
  #She should have no BroodIDLaid or Fledged because this individual was caught as an adult
  expect_equal(subset(UAN_data, IndvID == "1002599")$BroodIDLaid, NA_character_)
  expect_equal(subset(UAN_data, IndvID == "1002599")$BroodIDFledged, NA_character_)
  #Her ring season should be 2003 with a RingAge of 'chick'
  expect_equal(subset(UAN_data, IndvID == "1002599")$RingSeason, 1981)
  expect_equal(subset(UAN_data, IndvID == "1002599")$RingAge, "adult")

})

test_that("Capture data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only UAN data
  UAN_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("PEE", "BOS"))

  #Test 1: Individual ringed as a chick
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(UAN_data, IndvID == "10277110")), 8)
  #Test that the first capture is as expected
  expect_equal(subset(UAN_data, IndvID == "10277110")$CaptureDate[1], as.Date("2008-05-16"))
  #Test that the 8th capture of the female is as expcted
  expect_equal(subset(UAN_data, IndvID == "10277110")$CaptureDate[8], as.Date("2010-01-18"))
  #Test that age observed is as expected on first capture
  #Test that age observed is as expected on 8th capture
  expect_equal(subset(UAN_data, IndvID == "10277110")$Age_observed[1], 1L)
  expect_equal(subset(UAN_data, IndvID == "10277110")$Age_observed[8], NA_integer_)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(UAN_data, IndvID == "10277110")$Age_calculated[1], 1L)
  expect_equal(subset(UAN_data, IndvID == "10277110")$Age_calculated[8], 7L)

  #Test 2: Female caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(UAN_data, IndvID == "1002599")), 4)
  #Test that the first capture is as expected
  expect_equal(subset(UAN_data, IndvID == "1002599")$CaptureDate[1], as.Date("1981-05-25"))
  #Test that the 4th capture is as expected
  expect_equal(subset(UAN_data, IndvID == "1002599")$CaptureDate[4], as.Date("1982-03-04"))
  #Test that first and 4th age observed is as expected
  expect_equal(subset(UAN_data, IndvID == "1002599")$Age_observed[1], 5L)
  expect_equal(subset(UAN_data, IndvID == "1002599")$Age_observed[4], 6L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(UAN_data, IndvID == "1002599")$Age_calculated[1], 4L)
  expect_equal(subset(UAN_data, IndvID == "1002599")$Age_calculated[4], 6L)

  #Test 3: Second chick example
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(UAN_data, IndvID == "A277487")), 7)
  #Test that the first capture is as expected
  expect_equal(subset(UAN_data, IndvID == "A277487")$CaptureDate[1], as.Date("1979-06-03"))
  #Test that the 15th capture is as expected
  expect_equal(subset(UAN_data, IndvID == "A277487")$CaptureDate[7], as.Date("1981-02-09"))
  #Test that first and last age observed is as expected
  expect_equal(subset(UAN_data, IndvID == "A277487")$Age_observed[1], 1L)
  expect_equal(subset(UAN_data, IndvID == "A277487")$Age_observed[7], 6L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(UAN_data, IndvID == "A277487")$Age_calculated[1], 1L)
  expect_equal(subset(UAN_data, IndvID == "A277487")$Age_calculated[7], 7L)

  #Test 4: Second adult example
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(UAN_data, IndvID == "10277670")), 7)
  #Test that the first capture is as expected
  expect_equal(subset(UAN_data, IndvID == "10277670")$CaptureDate[1], as.Date("2008-06-11"))
  #Test that the 8th capture is as expected
  expect_equal(subset(UAN_data, IndvID == "10277670")$CaptureDate[7], as.Date("2012-05-07"))
  #Test that first and last age observed is as expected
  expect_equal(subset(UAN_data, IndvID == "10277670")$Age_observed[1], 5L)
  expect_equal(subset(UAN_data, IndvID == "10277670")$Age_observed[7], 6L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(UAN_data, IndvID == "10277670")$Age_calculated[1], 4L)
  expect_equal(subset(UAN_data, IndvID == "10277670")$Age_calculated[7], 12L)


})

test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only NIOO data
  UAN_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("PEE", "BOS"))

  #Test 1: Nestbox with coordinates only in Lambert system
  expect_true(subset(UAN_data, LocationID == "BN10")$LocationType == "NB")
  #Expect LocationID and NestboxID are the same
  expect_true(subset(UAN_data, LocationID == "BN10")$NestboxID == "BN10")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(UAN_data, LocationID == "BN10")$StartSeason, 2000L)
  expect_equal(subset(UAN_data, LocationID == "BN10")$EndSeason, 2006L)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(UAN_data, LocationID == "BN10")$PopID, "BOS")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(UAN_data, LocationID == "BN10")$Latitude, 2) %>% setNames(nm = NULL), 51.15)
  expect_equal(round(subset(UAN_data, LocationID == "BN10")$Longitude, 2) %>% setNames(nm = NULL), 4.52)

  #Test 2: Nestbox with coordinates in both coordinates. Lambert should take precedence
  #LocationType is as expected
  expect_true(subset(UAN_data, LocationID == "BW1")$LocationType == "NB")
  #Expect no NestboxID
  expect_equal(subset(UAN_data, LocationID == "BW1")$NestboxID, "BW1")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(UAN_data, LocationID == "BW1")$StartSeason, 2000L)
  expect_equal(subset(UAN_data, LocationID == "BW1")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(UAN_data, LocationID == "BW1")$PopID, "BOS")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(UAN_data, LocationID == "BW1")$Latitude, 2) %>% setNames(nm = NULL), 51.14)
  expect_equal(round(subset(UAN_data, LocationID == "BW1")$Longitude, 2) %>% setNames(nm = NULL), 4.51)

  #Test 3: Feeder
  #LocationType is as expected
  expect_true(subset(UAN_data, LocationID == "TUF_J2")$LocationType == "FD")
  #Expect no NestboxID
  expect_equal(subset(UAN_data, LocationID == "TUF_J2")$NestboxID, NA_character_)
  #Expect Start and EndSeason is as expected
  expect_equal(subset(UAN_data, LocationID == "TUF_J2")$StartSeason, 2012L)
  expect_equal(subset(UAN_data, LocationID == "TUF_J2")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(UAN_data, LocationID == "TUF_J2")$PopID, "BOS")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(UAN_data, LocationID == "TUF_J2")$Latitude, 2) %>% setNames(nm = NULL), 51.14)
  expect_equal(round(subset(UAN_data, LocationID == "TUF_J2")$Longitude, 2) %>% setNames(nm = NULL), 4.52)

  #Test 4: Nestbox where no type provided
  #LocationType is as expected
  expect_true(subset(UAN_data, LocationID == "BW11BIS")$LocationType == "NB")
  #Expect no NestboxID
  expect_equal(subset(UAN_data, LocationID == "BW11BIS")$NestboxID, "BW11BIS")
  #Expect Start and EndSeason is as expected
  expect_equal(subset(UAN_data, LocationID == "BW11BIS")$StartSeason, 2007L)
  expect_equal(subset(UAN_data, LocationID == "BW11BIS")$EndSeason, 2008L)
  #Check that LocationID is in the expected PopID
  expect_equal(subset(UAN_data, LocationID == "BW11BIS")$PopID, "BOS")
  #Check that latitude and longitude are as expected
  expect_equal(round(subset(UAN_data, LocationID == "BW11BIS")$Latitude, 2) %>% setNames(nm = NULL), 51.14)
  expect_equal(round(subset(UAN_data, LocationID == "BW11BIS")$Longitude, 2) %>% setNames(nm = NULL), 4.51)

})
