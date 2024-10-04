testthat::skip_if(!exists("data_path"))

pipeline_output <- format_KIL(db = paste0(data_path, "/KIL_KilingiNomme_Estonia"))

test_that("KIL outputs all files...", {

  expect_true("KIL" %in% pipeline_output$Brood_data$PopID)
  expect_true("KIL" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("KIL" %in% pipeline_output$Individual_data$PopID)
  expect_true("KIL" %in% pipeline_output$Location_data$PopID)
  expect_true(pipeline_output$protocol_version == "1.1.0")

})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only KIL data
  KIL_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "KIL")

  #Test 1: Adult tit female
  #Individual 1742516 should be listed as a female
  expect_equal(subset(KIL_data, IndvID == "1742516")$Sex_calculated, "F")
  expect_equal(subset(KIL_data, IndvID == "1742516")$Species, "PARMAJ")
  expect_equal(subset(KIL_data, IndvID == "1742516")$BroodIDLaid, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "1742516")$BroodIDFledged, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "1742516")$RingSeason, 1999L)
  expect_equal(subset(KIL_data, IndvID == "1742516")$RingAge, "adult")

  #Test 2: Adult tit female
  #Individual 107323  should be listed as a female
  expect_equal(subset(KIL_data, IndvID == "107323")$Sex_calculated, "F")
  expect_equal(subset(KIL_data, IndvID == "107323")$Species, "PARMAJ")
  expect_equal(subset(KIL_data, IndvID == "107323")$BroodIDLaid, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "107323")$BroodIDFledged, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "107323")$RingSeason, 1974L)
  expect_equal(subset(KIL_data, IndvID == "107323")$RingAge, "adult")

  #Test 3: Adult tit male
  #Individual 1697275 should be listed as a male
  expect_equal(subset(KIL_data, IndvID == "1697275")$Sex_calculated, "M")
  expect_equal(subset(KIL_data, IndvID == "1697275")$Species, "PARMAJ")
  expect_equal(subset(KIL_data, IndvID == "1697275")$BroodIDLaid, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "1697275")$BroodIDFledged, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "1697275")$RingSeason, 1997L)
  expect_equal(subset(KIL_data, IndvID == "1697275")$RingAge, "adult")

  #Test 4: Adult tit male
  #Individual 5991026 should be listed as a male
  expect_equal(subset(KIL_data, IndvID == "5991026")$Sex_calculated, "M")
  expect_equal(subset(KIL_data, IndvID == "5991026")$Species, "PARMAJ")
  expect_equal(subset(KIL_data, IndvID == "5991026")$BroodIDLaid, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "5991026")$BroodIDFledged, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "5991026")$RingSeason, 1996L)
  expect_equal(subset(KIL_data, IndvID == "5991026")$RingAge, "adult")

  #Test 5: Bird ringed as chick
  expect_equal(subset(KIL_data, IndvID == "100002")$Sex_calculated, "F")
  expect_equal(subset(KIL_data, IndvID == "100002")$Species, "PARMAJ")
  expect_equal(subset(KIL_data, IndvID == "100002")$BroodIDLaid, "477")
  expect_equal(subset(KIL_data, IndvID == "100002")$BroodIDFledged, "477")
  expect_equal(subset(KIL_data, IndvID == "100002")$RingSeason, 1973L)
  expect_equal(subset(KIL_data, IndvID == "100002")$RingAge, "chick")

  #Test 6: Individual with not clear sex
  expect_equal(subset(KIL_data, IndvID == "100093")$Sex_calculated, "C")
  expect_equal(subset(KIL_data, IndvID == "100093")$BroodIDLaid, "483")
  expect_equal(subset(KIL_data, IndvID == "100093")$BroodIDFledged, "483")
  expect_equal(subset(KIL_data, IndvID == "100093")$RingSeason, 1973L)
  expect_equal(subset(KIL_data, IndvID == "100093")$RingAge, "chick")

  #Test 7: Individual with sex unknown
  expect_equal(subset(KIL_data, IndvID == "671705")$Sex_calculated, NA_character_)
  expect_equal(subset(KIL_data, IndvID == "671705")$BroodIDLaid, "3406")
  expect_equal(subset(KIL_data, IndvID == "671705")$BroodIDFledged, "3406")
  expect_equal(subset(KIL_data, IndvID == "671705")$RingSeason, 1983L)
  expect_equal(subset(KIL_data, IndvID == "671705")$RingAge, "chick")

})


test_that("Brood_data returns an expected outcome...", {

  #Take a subset of only KIL data
  KIL_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "KIL")

  #Test 1: Tit brood where clutch type = first
  #BroodID 82
  expect_equal(subset(KIL_data, BroodID == "82")$ClutchType_calculated, "first")
  expect_equal(subset(KIL_data, BroodID == "82")$Species, "PARMAJ")
  expect_equal(subset(KIL_data, BroodID == "82")$FemaleID, "1847")
  expect_equal(subset(KIL_data, BroodID == "82")$MaleID, "26537")
  expect_equal(subset(KIL_data, BroodID == "82")$LocationID, "kn471")
  expect_equal(subset(KIL_data, BroodID == "82")$BreedingSeason, 1971L)
  expect_equal(subset(KIL_data, BroodID == "82")$ClutchSize_observed, 7L)

  #Test 2: Tit brood where clutch type = first
  #BroodID 1996_t845
  expect_equal(subset(KIL_data, BroodID == "1996_t845_1")$ClutchType_calculated, "first")
  expect_equal(subset(KIL_data, BroodID == "1996_t845_1")$Species, "PARMAJ")
  expect_equal(subset(KIL_data, BroodID == "1996_t845_1")$FemaleID, "5990017")
  expect_equal(subset(KIL_data, BroodID == "1996_t845_1")$MaleID, "5990023")
  expect_equal(subset(KIL_data, BroodID == "1996_t845_1")$ClutchSize_observed, 8L)
  expect_equal(subset(KIL_data, BroodID == "1996_t845_1")$HatchDate_observed, as.Date("1996-05-30"))

  #Test 3: Tit brood where clutch type = second
  #BroodID 96
  expect_equal(subset(KIL_data, BroodID == "96")$ClutchType_observed, "second")
  expect_equal(subset(KIL_data, BroodID == "96")$ClutchType_calculated, NA_character_)
  expect_equal(subset(KIL_data, BroodID == "96")$Species, "PARMAJ")
  expect_equal(subset(KIL_data, BroodID == "96")$FemaleID, "71900")
  expect_equal(subset(KIL_data, BroodID == "96")$MaleID, NA_character_)
  expect_true(is.na(subset(KIL_data, BroodID == "96")$HatchDate_observed))
  expect_equal(subset(KIL_data, BroodID == "96")$ClutchSize_observed, NA_integer_)
  expect_equal(subset(KIL_data, BroodID == "96")$NumberFledged_observed, 7L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(KIL_data, BroodID == "96")$AvgChickMass, NA_real_)
  expect_equal(subset(KIL_data, BroodID == "96")$AvgTarsus, NA_real_)

  #### THIS MAY CHANGE AFTER THE NEWS FROM DATA OWNER REGARDING THE FLIPPED DATES OF HATCHING??

  #Test 4: Tit brood where clutch type = replacement (due to past cutoff)
  #BroodID 259
  expect_equal(subset(KIL_data, BroodID == "259")$Species, "PARMAJ")
  #BroodID 2013_004_14_06 should have clutch type calculated 'replacement'
  expect_equal(subset(KIL_data, BroodID == "259")$ClutchType_calculated, "replacement")
  expect_equal(subset(KIL_data, BroodID == "259")$FemaleID, "70228")
  expect_equal(subset(KIL_data, BroodID == "259")$MaleID, "70234")
  expect_equal(subset(KIL_data, BroodID == "259")$LayDate_observed, as.Date("1973-06-04"))
  expect_equal(subset(KIL_data, BroodID == "259")$ClutchSize_observed, NA_integer_)
  expect_equal(subset(KIL_data, BroodID == "259")$NumberFledged_observed, 6L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(KIL_data, BroodID == "259")$AvgChickMass, NA_real_)
  expect_equal(subset(KIL_data, BroodID == "259")$AvgTarsus, NA_real_)

})


test_that("Capture_data returns an expected outcome...", {

  #### NEED TO FIX THE CAPTURE DATE AFTER THE RESPONSE FROM DATA OWNERS

  #Take a subset of only KIL data
  KIL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "KIL")

  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (2)
  expect_equal(nrow(subset(KIL_data, IndvID == "5991287")), 2)
  #Test the first capture of the female
  expect_equal(subset(KIL_data, IndvID == "5991287")$Sex_observed[1], "F")
  expect_equal(subset(KIL_data, IndvID == "5991287")$LocationID[1], "n71")
  expect_equal(min(subset(KIL_data, IndvID == "5991287")$CaptureDate, na.rm = TRUE), as.Date("2000-07-10"))

  #Test 2: Male caught as adult
  #Test the male has the correct number of capture records
  expect_equal(nrow(subset(KIL_data, IndvID == "107184")), 3)
  #Test the first capture of the male
  expect_equal(subset(KIL_data, IndvID == "107184")$Sex_observed[1], "M")
  expect_equal(subset(KIL_data, IndvID == "107184")$LocationID[1], "kn328")
  # expect_equal(min(subset(KIL_data, IndvID == "107184")$CaptureDate, na.rm = TRUE), as.Date(""))

  #Test 3: Female caught first time as chick
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(KIL_data, IndvID == "100002")), 5)
  #Test the first capture of the female
  expect_equal(subset(KIL_data, IndvID == "100002")$Sex_observed[1], NA_character_)
  expect_equal(subset(KIL_data, IndvID == "100002")$Sex_observed[5], "F")
  expect_equal(subset(KIL_data, IndvID == "100002")$LocationID[1], "kn795")
  expect_equal(subset(KIL_data, IndvID == "100002")$BreedingSeason[1], 1973L)
  expect_equal(min(subset(KIL_data, IndvID == "100002")$CaptureDate, na.rm = TRUE), as.Date("1973-05-01"))

  #Test 4: Male caught first time as chick
  #Test the male has the correct number of capture records
  expect_equal(nrow(subset(KIL_data, IndvID == "884944")), 5)
  #Test the first capture of the male
  expect_equal(subset(KIL_data, IndvID == "884944")$Sex_observed[1], NA_character_)
  expect_equal(subset(KIL_data, IndvID == "884944")$LocationID[1], "kn515")
  expect_equal(subset(KIL_data, IndvID == "884944")$BreedingSeason[1], 1985L)
  expect_equal(min(subset(KIL_data, IndvID == "884944")$CaptureDate, na.rm = TRUE), as.Date("1985-05-01"))

})


test_that("Location_data returns an expected outcome...", {

  #Take a subset of only KIL data
  KIL_data <- dplyr::filter(pipeline_output$Location_data, PopID == "KIL")

  #Test 1: Nestbox evergreen
  #LocationType is as expected
  expect_equal(subset(KIL_data, LocationID == "r18")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(KIL_data, LocationID == "r18")$HabitatType, "evergreen")
  #Start season
  expect_equal(subset(KIL_data, LocationID == "r18")$StartSeason, 1995L)
  #End season
  expect_equal(subset(KIL_data, LocationID == "r18")$EndSeason, NA_integer_)

  #Test 2: Nestbox
  #LocationType is as expected
  expect_equal(subset(KIL_data, LocationID == "b170")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(KIL_data, LocationID == "b170")$HabitatType, "deciduous")
  #Start season
  expect_equal(subset(KIL_data, LocationID == "b170")$StartSeason, 2000L)
  #End season
  expect_equal(subset(KIL_data, LocationID == "b170")$EndSeason, NA_integer_)

  #Test 3: Nestbox
  #LocationType is as expected
  expect_equal(subset(KIL_data, LocationID == "kn1010")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(KIL_data, LocationID == "kn1010")$HabitatType, NA_character_)
  #Start season
  expect_equal(subset(KIL_data, LocationID == "kn1010")$StartSeason, 1971L)
  #End season
  expect_equal(subset(KIL_data, LocationID == "kn1010")$EndSeason, NA_integer_)

  #Test 4: Nestbox
  #LocationType is as expected
  expect_equal(subset(KIL_data, LocationID == "a22")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(KIL_data, LocationID == "a22")$HabitatType, "evergreen")
  #Start season
  expect_equal(subset(KIL_data, LocationID == "a22")$StartSeason, 2001L)
  #End season
  expect_equal(subset(KIL_data, LocationID == "a22")$EndSeason, NA_integer_)

})

