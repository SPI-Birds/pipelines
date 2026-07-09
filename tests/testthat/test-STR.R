testthat::skip_if(!exists("data_path"))
pipeline_output <- format_STR(db = paste0(data_path, "/STR_Strasbourg_France"),
                              optional_variables = "all")


testthat::test_that("STR outputs all files...", {

  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Brood_data$PopID))
  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Capture_data$CapturePopID))
  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Individual_data$PopID))
  testthat::expect_true(all(c("WAN", "STR", "ROB") %in% pipeline_output$Location_data$PopID))

})



testthat::test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of calculatedClutchType
  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("STR", "ROB", "WAN"))

  #Test 1: Brood where clutch type = first
  testthat::expect_equal(subset(STR_data, BroodID == "2019-351")$Species, "PARMAJ")
  testthat::expect_equal(subset(STR_data, BroodID == "2019-351")$ClutchType_calculated, "first")
  testthat::expect_equal(subset(STR_data, BroodID == "2019-351")$ClutchSize_observed, 10)
  testthat::expect_equal(subset(STR_data, BroodID == "2019-351")$BroodSize_observed, 10)
  testthat::expect_equal(subset(STR_data, BroodID == "2019-351")$NumberFledged_observed, 3)
  testthat::expect_equal(subset(STR_data, BroodID == "2019-351")$BreedingSeason, 2019)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  testthat::expect_equal(subset(STR_data, BroodID == "2022-148")$Species, "PARMAJ")
  testthat::expect_equal(subset(STR_data, BroodID == "2022-148")$ClutchType_calculated, "replacement")
  testthat::expect_equal(subset(STR_data, BroodID == "2022-148")$ClutchSize_observed, 10)
  testthat::expect_equal(subset(STR_data, BroodID == "2022-148")$BroodSize_observed, 10)
  testthat::expect_equal(subset(STR_data, BroodID == "2022-148")$NumberFledged_observed, 5)
  testthat::expect_equal(subset(STR_data, BroodID == "2022-148")$BreedingSeason, 2022)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  testthat::expect_equal(subset(STR_data, BroodID == "2017-913")$Species, "PARMAJ")
  testthat::expect_equal(subset(STR_data, BroodID == "2017-913")$ClutchType_calculated, "replacement")
  testthat::expect_equal(subset(STR_data, BroodID == "2017-913")$ClutchSize_observed, NA_integer_)
  testthat::expect_equal(subset(STR_data, BroodID == "2017-913")$BroodSize_observed, 7)
  testthat::expect_equal(subset(STR_data, BroodID == "2017-913")$NumberFledged_observed, 6)
  testthat::expect_equal(subset(STR_data, BroodID == "2017-913")$BreedingSeason, 2017)

  #Test 4: Brood where clutch type = second
  testthat::expect_equal(subset(STR_data, BroodID == "2021-441")$Species, "PARMAJ")
  testthat::expect_equal(subset(STR_data, BroodID == "2021-441")$ClutchType_calculated, "second")
  testthat::expect_equal(subset(STR_data, BroodID == "2021-441")$ClutchSize_observed, 6)
  testthat::expect_equal(subset(STR_data, BroodID == "2021-441")$BroodSize_observed, NA_integer_)
  testthat::expect_equal(subset(STR_data, BroodID == "2021-441")$NumberFledged_observed, NA_integer_)
  testthat::expect_equal(subset(STR_data, BroodID == "2021-441")$BreedingSeason, 2021)

  #Test 5: FemaleIDs only contain numbers (or V/O at the first character) and all are 6-8 characters long (until 2022, only chicks with V+6digits ring number, but males
  #and females breeders recruited can have one in 2023 onwards)
  testthat::expect_true(all(nchar(STR_data$FemaleID[!is.na(STR_data$FemaleID)]) %in% c(6,7,8) &
                              stringr::str_detect(STR_data$FemaleID[!is.na(STR_data$FemaleID)], "^(V|[0-9])+[:digit:]+$")))

  #Test 8: MaleIDs only contain numbers (or V/O at the first character) and all are 6-8 characters long
  testthat::expect_true(all(nchar(STR_data$MaleID[!is.na(STR_data$MaleID)]) %in% c(6,7,8)
                            & stringr::str_detect(STR_data$MaleID[!is.na(STR_data$MaleID)], "^(V|O|[0-9])+[:digit:]+$")))

})


testthat::test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("WAN", "STR", "ROB"))

  #Test 1: Male caught first as breeder (adult)
  #Individual 7971223 should be listed as a male great tit
  testthat::expect_equal(subset(STR_data, IndvID == "7971223")$Sex_calculated, "M")
  testthat::expect_equal(subset(STR_data, IndvID == "7971223")$Species, "PARMAJ")
  #They should have no broodIDLaid or Fledged because she was never caught as a chick
  testthat::expect_equal(subset(STR_data, IndvID == "7971223")$BroodIDLaid, NA_character_)
  testthat::expect_equal(subset(STR_data, IndvID == "7971223")$BroodIDFledged, NA_character_)
  #His tag year should be 2016 with a tagStage of 'NA' (age was not identified when first tagged)
  testthat::expect_equal(subset(STR_data, IndvID == "7971223")$RingSeason, 2016)
  testthat::expect_equal(subset(STR_data, IndvID == "7971223")$RingAge, "adult")

  #Test 2: Female caught first as breeder (adult)
  #Individual 7971215 should be listed as a female great tit
  testthat::expect_equal(subset(STR_data, IndvID == "7971215")$Sex_calculated, "F")
  testthat::expect_equal(subset(STR_data, IndvID == "7971215")$Species, "PARMAJ")
  #She should have no broodIDLaid or Fledged because this individual was first caught as a breeder
  testthat::expect_equal(subset(STR_data, IndvID == "7971215")$BroodIDLaid, NA_character_)
  testthat::expect_equal(subset(STR_data, IndvID == "7971215")$BroodIDFledged, NA_character_)
  #Her tag year should be 2016 with a tagStage of 'NA' (unidentified age when captured)
  testthat::expect_equal(subset(STR_data, IndvID == "7971215")$RingSeason, 2016)
  testthat::expect_equal(subset(STR_data, IndvID == "7971215")$RingAge, "adult")

  #Test 3: Caught as chick
  #Individual 7971346 should be listed as a [conflicted sex] great tit
  testthat::expect_equal(subset(STR_data, IndvID == "7971346")$Sex_calculated, "C")
  testthat::expect_equal(subset(STR_data, IndvID == "7971346")$Species, "PARMAJ")
  #Check that broodIDLaid/Fledged are as expected
  #This individual was not cross-fostered, so they should be the same
  testthat::expect_equal(subset(STR_data, IndvID == "7971346")$BroodIDLaid, "2017-53")
  testthat::expect_equal(subset(STR_data, IndvID == "7971346")$BroodIDFledged, "2017-53")
  #tag Year should be 2017 with a tagstage of 'chick'
  testthat::expect_equal(subset(STR_data, IndvID == "7971346")$RingSeason, 2017)
  testthat::expect_equal(subset(STR_data, IndvID == "7971346")$RingAge, "chick")

  #Test 4: individualIDs only contain numbers (or V as the first character) and all are 6-8 characters long
  testthat::expect_true(all(nchar(STR_data$IndvID[!is.na(STR_data$IndvID)]) %in% c(6,7,8) &
                              stringr::str_detect(STR_data$IndvID[!is.na(STR_data$IndvID)], "^(V|[0-9])+[:digit:]+$")))

})



testthat::test_that("Capture data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("STR", "WAN", "ROB"))

  #Test 1: Individual ringed as a chick
  #Test the male has the correct number of capture records
  testthat::expect_equal(nrow(subset(STR_data, IndvID == "8211502")), 7)
  #Test that the first capture is as expected (2017-06-09)
  testthat::expect_equal(subset(STR_data, IndvID == "8211502")$CaptureDate[1], as.Date('2017-06-09', format = "%Y-%m-%d"))

  #Test that the 6th capture of the male is as expected (2022-05-06)
  testthat::expect_equal(subset(STR_data, IndvID == "8211502")$CaptureDate[6], as.Date('2022-05-06', format = "%Y-%m-%d"))


  #Test 2: Female caught only as adult
  #Test it has the correct number of capture records
  testthat::expect_equal(nrow(subset(STR_data, IndvID == "8211824")), 5)
  #Test that the first capture is as expected (2018-05-08)
  testthat::expect_equal(subset(STR_data, IndvID == "8211824")$CaptureDate[1], as.Date('2018-05-08', format = "%Y-%m-%d"))
  #Test that the 5th capture is as expected (2021-06-11)
  testthat::expect_equal(subset(STR_data, IndvID == "8211824")$CaptureDate[5], as.Date('2021-06-11', format = "%Y-%m-%d"))


  #Test 3: Male caught only as adult
  #Test it has the correct number of capture records
  testthat::expect_equal(nrow(subset(STR_data, IndvID == "7971525")), 6)
  #Test that the first capture date is as expected (2018-05-18)
  testthat::expect_equal(subset(STR_data, IndvID == "7971525")$CaptureDate[1], as.Date('2018-05-18', format = "%Y-%m-%d"))
  #Test that the 4th capture is as expected (2022-05-24)
  testthat::expect_equal(subset(STR_data, IndvID == "7971525")$CaptureDate[4], as.Date('2022-05-24', format = "%Y-%m-%d"))


  #Test 4: IndvIDs are all properly formatted
  testthat::expect_true(all(nchar(STR_data$IndvID) %in% c(6,7,8) & stringr::str_detect(STR_data$IndvID, "^(V|[0-9])+[:digit:]+$")))

})


testthat::test_that("Location_data returns an expected outcome...", {

  #We want to run tests for nest boxes (there are no mistnets)

  #Take a subset of only STR data
  STR_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("STR", "WAN", "ROB"))

  #Test 1: Nestbox check
  #Location listed as a nest box that has lat/long from separate file
  #Record has expected LocationType
  testthat::expect_true(subset(STR_data, NestboxID == "CITA_69_1")$LocationType == "NB")
  #habitatID as expected
  testthat::expect_true(subset(STR_data, NestboxID == "CITA_69_1")$HabitatType == "urban")
  #Expect Start and EndYear is as expected
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_69_1")$StartSeason, 2018L)
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_69_1")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_69_1")$PopID, "STR")
  #Check that latitude and longitude are as expected
  testthat::expect_equal(round(subset(STR_data, NestboxID == "CITA_69_1")$Latitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, NestboxID == "CITA_69_1")$Longitude, 2) %>% setNames(nm = NULL), 7.78)

  #Test 2: Nestbox check
  #Location with no lat/long info
  #Record has expected LocationType
  testthat::expect_true(subset(STR_data, NestboxID == "OBSE_11a_1")$LocationType == "NB")
  #habitatID is set to J1 (zone corresponds to "downtown", so urban habitat)
  testthat::expect_equal(subset(STR_data, NestboxID == "OBSE_11a_1")$HabitatType, "urban")
  #Expect Start and End Year set respectively to 2014 and 2020
  testthat::expect_equal(subset(STR_data, NestboxID == "OBSE_11a_1")$StartSeason, 2014L)
  testthat::expect_equal(subset(STR_data, NestboxID == "OBSE_11a_1")$EndSeason, 2020L)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, NestboxID == "OBSE_11a_1")$PopID, "STR")
  #Check that latitude and longitude are as expected
  testthat::expect_equal(round(subset(STR_data, NestboxID == "OBSE_11a_1")$Latitude, 2) %>% setNames(nm = NULL), NA_real_)
  testthat::expect_equal(round(subset(STR_data, NestboxID == "OBSE_11a_1")$Longitude, 2) %>% setNames(nm = NULL), NA_real_)

  #Test 3: Mistnet check (location outside main study area)
  #LocationType is as expected
  testthat::expect_true(subset(STR_data, LocationID == "WANT_MN")$LocationType == "MN")
  #habitatID should correspond to forest habitat (G1) since "want" site corresponds to a forest site (within WAN siteID)
  testthat::expect_equal(subset(STR_data, LocationID == "WANT_MN")$HabitatType, "deciduous")
  #Expect Start and EndYear is as expected
  testthat::expect_equal(subset(STR_data, LocationID == "WANT_MN")$StartSeason, 2019L)
  testthat::expect_equal(subset(STR_data, LocationID == "WANT_MN")$EndSeason, 2019L)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, LocationID == "WANT_MN")$PopID, "WAN")

  #Test 4: Nestbox check: case when nest box type was changed
  #LocationType is as expected for both occurrences
  testthat::expect_true(subset(STR_data, NestboxID == "ESPL_21_1")$LocationType == "NB")
  testthat::expect_true(subset(STR_data, NestboxID == "ESPL_21_2")$LocationType == "NB")
  #habitatID as expected for both occurrences
  testthat::expect_equal(subset(STR_data, NestboxID == "ESPL_21_1")$HabitatType, "urban")
  testthat::expect_equal(subset(STR_data, NestboxID == "ESPL_21_2")$HabitatType, "urban")
  #Expect Start and EndYear is as expected for both occurrences
  testthat::expect_equal(subset(STR_data, NestboxID == "ESPL_21_1")$StartSeason, 2014L)
  testthat::expect_equal(subset(STR_data, NestboxID == "ESPL_21_1")$EndSeason, 2019L)
  testthat::expect_equal(subset(STR_data, NestboxID == "ESPL_21_2")$StartSeason, 2021L)
  testthat::expect_equal(subset(STR_data, NestboxID == "ESPL_21_2")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, NestboxID == "ESPL_21_1")$PopID, "STR")
  testthat::expect_equal(subset(STR_data, NestboxID == "ESPL_21_2")$PopID, "STR")
  #Check that latitude and longitude are as expected
  testthat::expect_equal(round(subset(STR_data, NestboxID == "ESPL_21_1")$Latitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, NestboxID == "ESPL_21_1")$Longitude, 2) %>% setNames(nm = NULL), 7.77)
  testthat::expect_equal(round(subset(STR_data, NestboxID == "ESPL_21_2")$Latitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, NestboxID == "ESPL_21_2")$Longitude, 2) %>% setNames(nm = NULL), 7.77)

  #Test 5: Nestbox check: case when there was a gap in monitoring the nest box
  #LocationType is as expected for both occurrences
  testthat::expect_true(subset(STR_data, NestboxID == "CITA_32_1")$LocationType == "NB")
  testthat::expect_true(subset(STR_data, NestboxID == "CITA_32_2")$LocationType == "NB")
  #habitatID as expected for both occurrences
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_32_1")$HabitatType, "urban")
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_32_2")$HabitatType, "urban")
  #Expect Start and EndYear is as expected for both occurrences
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_32_1")$StartSeason, 2017L)
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_32_1")$EndSeason, 2018L)
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_32_2")$StartSeason, 2022L)
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_32_2")$EndSeason, NA_integer_)
  #Check that LocationID is in the expected siteID
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_32_1")$PopID, "STR")
  testthat::expect_equal(subset(STR_data, NestboxID == "CITA_32_2")$PopID, "STR")
  #Check that latitude and longitude are as expected
  testthat::expect_equal(round(subset(STR_data, NestboxID == "CITA_32_1")$Latitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, NestboxID == "CITA_32_1")$Longitude, 2) %>% setNames(nm = NULL), 7.77)
  testthat::expect_equal(round(subset(STR_data, NestboxID == "CITA_32_2")$Latitude, 2) %>% setNames(nm = NULL), 48.58)
  testthat::expect_equal(round(subset(STR_data, NestboxID == "CITA_32_2")$Longitude, 2) %>% setNames(nm = NULL), 7.77)

})




## General tests (for pipelines formatted to standard protocol version 2.0)

test_protocol_compliance(pipeline_output)
