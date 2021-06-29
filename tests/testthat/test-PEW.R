pipeline_output <- format_PEW(db = paste0(data_path, "/PEW_PeerdsbosWest_Belgium"))

test_that("PEW outputs all files...", {

  expect_true("PEW" %in% pipeline_output$Brood_data$PopID)
  expect_true("PEW" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("PEW" %in% pipeline_output$Individual_data$PopID)
  expect_true("PEW" %in% pipeline_output$Location_data$PopID)

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only PEW data
  PEW_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "PEW")

  #Test 1: Adult blue tit female
  #Individual 13617132 should be listed as a female
  expect_equal(subset(PEW_data, IndvID == "13617132")$Sex_calculated, "F")
  expect_equal(subset(PEW_data, IndvID == "13617132")$Species, "CYACAE")
  expect_equal(subset(PEW_data, IndvID == "13619447")$BroodIDLaid, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "13619447")$BroodIDFledged, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "13617132")$RingSeason, 2015)
  expect_equal(subset(PEW_data, IndvID == "13617132")$RingAge, "adult")

  #Test 2: Adult blue tit male
  #Individual 13617661 should be listed as a male
  expect_equal(subset(PEW_data, IndvID == "13617661")$Sex_calculated, "M")
  expect_equal(subset(PEW_data, IndvID == "13617661")$Species, "CYACAE")
  expect_equal(subset(PEW_data, IndvID == "13617661")$BroodIDLaid, "2015_26")
  expect_equal(subset(PEW_data, IndvID == "13617661")$BroodIDFledged, "2015_26")
  expect_equal(subset(PEW_data, IndvID == "13617661")$RingSeason, 2015)
  expect_equal(subset(PEW_data, IndvID == "13617661")$RingAge, "chick")

  #Test 3: Bird ringed as chick
  expect_equal(subset(PEW_data, IndvID == "14154507")$Sex_calculated, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "14154507")$Species, "CYACAE")
  expect_equal(subset(PEW_data, IndvID == "14154507")$BroodIDLaid, "2016_54")
  expect_equal(subset(PEW_data, IndvID == "14154507")$BroodIDFledged, "2016_54")
  expect_equal(subset(PEW_data, IndvID == "14154507")$RingSeason, 2016)
  expect_equal(subset(PEW_data, IndvID == "14154507")$RingAge, "chick")

  #Test 4: Individual with not clear sex
  expect_equal(subset(PEW_data, IndvID == "13619447")$Sex_calculated, "C")
  expect_equal(subset(PEW_data, IndvID == "13619447")$BroodIDLaid, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "13619447")$BroodIDFledged, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "13619447")$RingSeason, 2015)
  expect_equal(subset(PEW_data, IndvID == "13619447")$RingAge, "adult")

  #Test 5: Individual with sex unknown
  expect_equal(subset(PEW_data, IndvID == "14156129")$Sex_calculated, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "14156129")$BroodIDLaid, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "14156129")$BroodIDFledged, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "14156129")$RingSeason, 2016)
  expect_equal(subset(PEW_data, IndvID == "14156129")$RingAge, "adult")

})

test_that("Brood_data returns an expected outcome...", {

  #Take a subset of only PEW data
  PEW_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "PEW")

  #Test 1: Tit brood where clutch type = first
  #BroodID 2017_63
  expect_equal(subset(PEW_data, BroodID == "2017_63")$ClutchType_calculated, "first")
  expect_equal(subset(PEW_data, BroodID == "2017_63")$Species, "CYACAE")
  expect_equal(subset(PEW_data, BroodID == "2017_63")$FemaleID, "14156281")
  expect_equal(subset(PEW_data, BroodID == "2017_63")$MaleID, "14156244")
  expect_equal(subset(PEW_data, BroodID == "2017_63")$HatchDate_observed, as.Date("2017-04-21"))
  expect_equal(subset(PEW_data, BroodID == "2017_63")$ClutchSize_observed, 9)

  #Test 2: Tit brood where clutch type = first
  #BroodID 2016_k89
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$ClutchType_observed, "first")
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$ClutchType_calculated, NA_character_)
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$Species, "CYACAE")
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$FemaleID, "13617034")
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$MaleID, "unringed")
  expect_true(is.na(subset(PEW_data, BroodID == "2016_k89")$HatchDate_observed))
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$ClutchSize_observed, 4)
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$NumberFledged_observed, 0)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$AvgChickMass, NA_real_)
  expect_equal(subset(PEW_data, BroodID == "2016_k89")$AvgTarsus, NA_real_)

  #Test 3: Tit brood where clutch type = replacement (due to past cutoff)
  #BroodID 2017_23
  expect_equal(subset(PEW_data, BroodID == "2017_23")$Species, "CYACAE")
  #BroodID 2013_004_14_06 should have clutch type calculated 'replacement'
  expect_equal(subset(PEW_data, BroodID == "2017_23")$ClutchType_calculated, "replacement")
  expect_equal(subset(PEW_data, BroodID == "2017_23")$FemaleID, "13619463")
  expect_equal(subset(PEW_data, BroodID == "2017_23")$MaleID, "unringed")
  expect_equal(subset(PEW_data, BroodID == "2017_23")$LayDate_observed, as.Date("2017-05-05"))
  expect_equal(subset(PEW_data, BroodID == "2017_23")$HatchDate_observed, as.Date("2017-05-23"))
  expect_equal(subset(PEW_data, BroodID == "2017_23")$ClutchSize_observed, 9)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PEW_data, BroodID == "2017_23")$AvgChickMass, NA_real_)
  expect_equal(subset(PEW_data, BroodID == "2017_23")$AvgTarsus, NA_real_)

  #Clutch that should have uncertainty in clutch size (i.e. has ?)
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$Species, "CYACAE")
  #ClutchType_calc is NA because no laying date given
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$ClutchType_calculated, NA_character_)
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$FemaleID, "14154588")
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$MaleID, "13619450")
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$LayDate_observed, as.Date(NA))
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$HatchDate_observed, as.Date("2016-05-14"))
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$ClutchSize_observed, 8)
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$ClutchSize_min, 8)
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$ClutchSize_max, Inf)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$AvgChickMass, NA_real_)
  expect_equal(subset(PEW_data, BroodID == "2016_k72")$AvgTarsus, NA_real_)

})

test_that("Capture_data returns an expected outcome...", {

  #Take a subset of only PEW data
  PEW_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "PEW")

  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (2)
  expect_equal(nrow(subset(PEW_data, IndvID == "12224446")), 2)
  #Test the first capture of the female
  expect_equal(subset(PEW_data, IndvID == "12224446")$Sex_observed[1], "F")
  expect_equal(subset(PEW_data, IndvID == "12224446")$LocationID[1], "kk21")
  expect_equal(min(subset(PEW_data, IndvID == "12224446")$CaptureDate, na.rm = TRUE), as.Date("2017-04-22"))

  #Test 2: Male caught as adult
  #Test the female has the correct number of capture records (6)
  expect_equal(nrow(subset(PEW_data, IndvID == "12706199")), 6)
  #Test the first capture of the male
  expect_equal(subset(PEW_data, IndvID == "12706199")$Sex_observed[1], "M")
  expect_equal(subset(PEW_data, IndvID == "12706199")$LocationID[1], "76")
  expect_equal(min(subset(PEW_data, IndvID == "12706199")$CaptureDate, na.rm = TRUE), as.Date("2016-05-17"))

  #Test 3: Female caught first time as chick
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(PEW_data, IndvID == "13617071")), 3)
  #Test the first capture of the female
  expect_equal(subset(PEW_data, IndvID == "13617071")$Sex_observed[1], "F")
  expect_equal(subset(PEW_data, IndvID == "13617071")$LocationID[1], "91")
  expect_equal(subset(PEW_data, IndvID == "13617071")$BreedingSeason[1], 2016)
  expect_equal(min(subset(PEW_data, IndvID == "13617071")$CaptureDate, na.rm = TRUE), as.Date("2016-05-18"))

  #Test 4: Male caught first time as chick
  #Test the male has the correct number of capture records
  expect_equal(nrow(subset(PEW_data, IndvID == "13617015")), 11)
  #Test the first capture of the male
  expect_equal(subset(PEW_data, IndvID == "13617015")$Sex_observed[1], "M")
  expect_equal(subset(PEW_data, IndvID == "13617015")$LocationID[1], "6")
  expect_equal(subset(PEW_data, IndvID == "13617015")$BreedingSeason[1], 2015)
  expect_equal(min(subset(PEW_data, IndvID == "13617015")$CaptureDate, na.rm = TRUE), as.Date("2015-05-27"))

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only PEW data
  PEW_data <- dplyr::filter(pipeline_output$Location_data, PopID == "PEW")

  #Test 1: Nestbox
  #LocationType is as expected
  expect_equal(subset(PEW_data, LocationID == "42")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(PEW_data, LocationID == "42")$HabitatType, "deciduous")
  #Start season
  expect_equal(subset(PEW_data, LocationID == "42")$StartSeason, 2015)
  #End season
  expect_equal(subset(PEW_data, LocationID == "42")$EndSeason, NA_integer_)

  #Test 2: Nestbox
  #LocationType is as expected
  expect_equal(subset(PEW_data, LocationID == "88")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(PEW_data, LocationID == "88")$HabitatType, "deciduous")
  #Start season
  expect_equal(subset(PEW_data, LocationID == "88")$StartSeason, 2016)
  #End season
  expect_equal(subset(PEW_data, LocationID == "88")$EndSeason, NA_integer_)

})

