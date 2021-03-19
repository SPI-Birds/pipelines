context("Run data quality check on Institute of Biology, Karelian Research Centre, Russian Academy of Sciences, Petrozavodsk, Russia, pipeline output")

test_that("PET outputs all files...", {

  expect_true("PET" %in% pipeline_output$Brood_data$PopID)
  expect_true("PET" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("PET" %in% pipeline_output$Individual_data$PopID)
  expect_true("PET" %in% pipeline_output$Location_data$PopID)

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only PET data
  PET_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "PET")

  #Test 1: Adult great tit female
  #Individual KS87696 should be listed as a female
  expect_equal(subset(PET_data, IndvID == "KS87696")$Sex_calculated, "F")
  expect_equal(subset(PET_data, IndvID == "KS87696")$Species, "PARMAJ")
  expect_equal(subset(PET_data, IndvID == "KS87696")$BroodIDLaid, NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87696")$BroodIDFledged, NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87696")$RingSeason, 2016L)
  expect_equal(subset(PET_data, IndvID == "KS87696")$RingAge, "adult")

  #Test 2: Adult great tit male
  #Individual KS87760 should be listed as a male
  expect_equal(subset(PET_data, IndvID == "KS87760")$Sex_calculated, "M")
  expect_equal(subset(PET_data, IndvID == "KS87760")$Species, "PARMAJ")
  expect_equal(subset(PET_data, IndvID == "KS87760")$BroodIDLaid, NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87760")$BroodIDFledged, NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87760")$RingSeason, 2018L)
  expect_equal(subset(PET_data, IndvID == "KS87760")$RingAge, "adult")

  #Test 3: Bird great tit ringed as chick
  expect_equal(subset(PET_data, IndvID == "XX84974")$Sex_calculated, NA_character_)
  expect_equal(subset(PET_data, IndvID == "XX84974")$Species, "PARMAJ")
  expect_equal(subset(PET_data, IndvID == "XX84974")$BroodIDLaid, "2019_Z_28_1")
  expect_equal(subset(PET_data, IndvID == "XX84974")$BroodIDFledged, "2019_Z_28_1")
  expect_equal(subset(PET_data, IndvID == "XX84974")$RingSeason, 2019L)
  expect_equal(subset(PET_data, IndvID == "XX84974")$RingAge, "chick")

  #Test 4: Individual with not clear sex
  expect_equal(subset(PET_data, IndvID == "13619447")$Sex_calculated, "C")
  expect_equal(subset(PET_data, IndvID == "13619447")$BroodIDLaid, NA_character_)
  expect_equal(subset(PET_data, IndvID == "13619447")$BroodIDFledged, NA_character_)
  expect_equal(subset(PET_data, IndvID == "13619447")$RingSeason, 2015)
  expect_equal(subset(PET_data, IndvID == "13619447")$RingAge, "adult")


#### here, do for other species




  #Test 5: Individual with sex unknown
  expect_equal(subset(PET_data, IndvID == "14156129")$Sex_calculated, NA_character_)
  expect_equal(subset(PET_data, IndvID == "14156129")$BroodIDLaid, NA_character_)
  expect_equal(subset(PET_data, IndvID == "14156129")$BroodIDFledged, NA_character_)
  expect_equal(subset(PET_data, IndvID == "14156129")$RingSeason, 2016)
  expect_equal(subset(PET_data, IndvID == "14156129")$RingAge, "adult")

})

test_that("Brood_data returns an expected outcome...", {

  #Take a subset of only PET data
  PET_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "PET")

  #Test 1: Tit brood where clutch type = first
  #BroodID 2017_63
  expect_equal(subset(PET_data, BroodID == "2017_63")$ClutchType_calculated, "first")
  expect_equal(subset(PET_data, BroodID == "2017_63")$Species, "CYACAE")
  expect_equal(subset(PET_data, BroodID == "2017_63")$FemaleID, "14156281")
  expect_equal(subset(PET_data, BroodID == "2017_63")$MaleID, "14156244")
  expect_equal(subset(PET_data, BroodID == "2017_63")$HatchDate_observed, as.Date("2017-04-21"))
  expect_equal(subset(PET_data, BroodID == "2017_63")$ClutchSize_observed, 9)

  #Test 2: Tit brood where clutch type = first
  #BroodID 2016_k89
  expect_equal(subset(PET_data, BroodID == "2016_k89")$ClutchType_observed, "first")
  expect_equal(subset(PET_data, BroodID == "2016_k89")$ClutchType_calculated, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2016_k89")$Species, "CYACAE")
  expect_equal(subset(PET_data, BroodID == "2016_k89")$FemaleID, "13617034")
  expect_equal(subset(PET_data, BroodID == "2016_k89")$MaleID, "unringed")
  expect_true(is.na(subset(PET_data, BroodID == "2016_k89")$HatchDate_observed))
  expect_equal(subset(PET_data, BroodID == "2016_k89")$ClutchSize_observed, 4)
  expect_equal(subset(PET_data, BroodID == "2016_k89")$NumberFledged_observed, 0)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2016_k89")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2016_k89")$AvgTarsus, NA_real_)

  #Test 3: Tit brood where clutch type = replacement (due to past cutoff)
  #BroodID 2017_23
  expect_equal(subset(PET_data, BroodID == "2017_23")$Species, "CYACAE")
  #BroodID 2013_004_14_06 should have clutch type calculated 'replacement'
  expect_equal(subset(PET_data, BroodID == "2017_23")$ClutchType_calculated, "replacement")
  expect_equal(subset(PET_data, BroodID == "2017_23")$FemaleID, "13619463")
  expect_equal(subset(PET_data, BroodID == "2017_23")$MaleID, "unringed")
  expect_equal(subset(PET_data, BroodID == "2017_23")$LayDate_observed, as.Date("2017-05-05"))
  expect_equal(subset(PET_data, BroodID == "2017_23")$HatchDate_observed, as.Date("2017-05-23"))
  expect_equal(subset(PET_data, BroodID == "2017_23")$ClutchSize_observed, 9)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2017_23")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2017_23")$AvgTarsus, NA_real_)

})

test_that("Capture_data returns an expected outcome...", {

  #Take a subset of only PET data
  PET_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "PET")

  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (2)
  expect_equal(nrow(subset(PET_data, IndvID == "12224446")), 2)
  #Test the first capture of the female
  expect_equal(subset(PET_data, IndvID == "12224446")$Sex_observed[1], "F")
  expect_equal(subset(PET_data, IndvID == "12224446")$LocationID[1], "kk21")
  expect_equal(min(subset(PET_data, IndvID == "12224446")$CaptureDate, na.rm = TRUE), as.Date("2017-04-22"))

  #Test 2: Male caught as adult
  #Test the female has the correct number of capture records (6)
  expect_equal(nrow(subset(PET_data, IndvID == "12706199")), 6)
  #Test the first capture of the male
  expect_equal(subset(PET_data, IndvID == "12706199")$Sex_observed[1], "M")
  expect_equal(subset(PET_data, IndvID == "12706199")$LocationID[1], "76")
  expect_equal(min(subset(PET_data, IndvID == "12706199")$CaptureDate, na.rm = TRUE), as.Date("2016-05-17"))

  #Test 3: Female caught first time as chick
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(PET_data, IndvID == "13617071")), 3)
  #Test the first capture of the female
  expect_equal(subset(PET_data, IndvID == "13617071")$Sex_observed[1], "F")
  expect_equal(subset(PET_data, IndvID == "13617071")$LocationID[1], "91")
  expect_equal(subset(PET_data, IndvID == "13617071")$BreedingSeason[1], 2016)
  expect_equal(min(subset(PET_data, IndvID == "13617071")$CaptureDate, na.rm = TRUE), as.Date("2016-05-18"))

  #Test 4: Male caught first time as chick
  #Test the male has the correct number of capture records
  expect_equal(nrow(subset(PET_data, IndvID == "13617015")), 11)
  #Test the first capture of the male
  expect_equal(subset(PET_data, IndvID == "13617015")$Sex_observed[1], "M")
  expect_equal(subset(PET_data, IndvID == "13617015")$LocationID[1], "6")
  expect_equal(subset(PET_data, IndvID == "13617015")$BreedingSeason[1], 2015)
  expect_equal(min(subset(PET_data, IndvID == "13617015")$CaptureDate, na.rm = TRUE), as.Date("2015-05-27"))

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only PET data
  PET_data <- dplyr::filter(pipeline_output$Location_data, PopID == "PET")

  #Test 1: Nestbox
  #LocationType is as expected
  expect_equal(subset(PET_data, LocationID == "42")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(PET_data, LocationID == "42")$HabitatType, "deciduous")
  #Start season
  expect_equal(subset(PET_data, LocationID == "42")$StartSeason, 2015)
  #End season
  expect_equal(subset(PET_data, LocationID == "42")$EndSeason, NA_integer_)

  #Test 2: Nestbox
  #LocationType is as expected
  expect_equal(subset(PET_data, LocationID == "88")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(PET_data, LocationID == "88")$HabitatType, "deciduous")
  #Start season
  expect_equal(subset(PET_data, LocationID == "88")$StartSeason, 2016)
  #End season
  expect_equal(subset(PET_data, LocationID == "88")$EndSeason, NA_integer_)

})

