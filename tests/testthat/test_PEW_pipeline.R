context("Run data quality check on Peerdsbos West, Belgium, pipeline output")


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
  expect_equal(subset(PEW_data, IndvID == "13617132")$Sex, "F")
  expect_equal(subset(PEW_data, IndvID == "13617132")$Species, "CYACAE")

  expect_equal(subset(PEW_data, IndvID == "13617132")$RingSeason, 2015)
  expect_equal(subset(PEW_data, IndvID == "13617132")$RingAge, "adult")

  #Test 2: Bird ringed as chick
  expect_equal(subset(PEW_data, IndvID == "14154507")$Sex, NA_character_)
  expect_equal(subset(PEW_data, IndvID == "14154507")$Species, "CYACAE")
  expect_equal(subset(PEW_data, IndvID == "14154507")$BroodIDLaid, "2016_54")
  expect_equal(subset(PEW_data, IndvID == "14154507")$BroodIDFledged, "2016_54")
  expect_equal(subset(PEW_data, IndvID == "14154507")$RingSeason, 2016)
  expect_equal(subset(PEW_data, IndvID == "14154507")$RingAge, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  #Take a subset of only PEW data
  PEW_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "PEW")

  #Test 1: Tit brood where clutch type = first
  #BroodID 2016_42
  expect_equal(subset(PEW_data, BroodID == "2016_42")$Species, "CYACAE")
  expect_equal(subset(PEW_data, BroodID == "2016_42")$FemaleID, "13619438")
  expect_equal(subset(PEW_data, BroodID == "2016_42")$MaleID, "13617730")
  expect_equal(subset(PEW_data, BroodID == "2016_42")$HatchDate_observed, as.Date("2016-05-07"))
  expect_equal(subset(PEW_data, BroodID == "2016_42")$ClutchSize_observed, 9)


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

})

test_that("Capture_data returns an expected outcome...", {

  #Take a subset of only PEW data
  PEW_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "PEW")

  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (2)
  expect_equal(nrow(subset(PEW_data, IndvID == "12224446")), 2)
  #Test the first capture of the female
  expect_equal(subset(PEW_data, IndvID == "12224446")$Sex_observed, "F")
  expect_equal(subset(PEW_data, IndvID == "12224446")$LocationID, "kk21")
  expect_equal(min(subset(PEW_data, IndvID == "12224446")$CaptureDate, na.rm = TRUE), as.Date("2017-04-22"))


  #Test 2: Male caught as adult
  #Test the female has the correct number of capture records (6)
  expect_equal(nrow(subset(PEW_data, IndvID == "12706199")), 6)
  #Test the first capture of the male
  expect_equal(min(subset(PEW_data, IndvID == "12706199")$Sex_observed), "M")
  expect_equal(min(subset(PEW_data, IndvID == "12706199")$LocationID), "76")
  expect_equal(min(subset(PEW_data, IndvID == "12706199")$CaptureDate, na.rm = TRUE), as.Date("2016-05-17"))


})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only PEW data
  PEW_data <- dplyr::filter(pipeline_output$Location_data, PopID == "PEW")

  #Test 1: Nestbox deciduous
  #LocationType is as expected
  expect_equal(subset(PEW_data, LocationID == "42")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(PEW_data, LocationID == "42")$Habitat, "deciduous")
  #Start season
  expect_equal(subset(PEW_data, LocationID == "42")$StartSeason, 2015)
  #End season
  expect_equal(subset(PEW_data, LocationID == "42")$EndSeason, NA_character_)

})
