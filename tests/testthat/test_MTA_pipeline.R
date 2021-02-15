context("Run data quality check on MMTA-PE Evolutionary Ecology Group, University of Pannonia, Hungary, pipeline output")

test_that("MTA outputs all files...", {
  expect_true(unique(c("BAL", "SZE", "VES", "VIL", "GUL") %in% pipeline_output$Brood_data$PopID))
  expect_true(unique(c("BAL", "SZE", "VES", "VIL", "GUL") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(unique(c("BAL", "SZE", "VES", "VIL", "GUL") %in% pipeline_output$Individual_data$PopID))
  expect_true(unique(c("BAL", "SZE", "VES", "VIL", "GUL") %in% pipeline_output$Location_data$PopID))
})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only MTA data
  MTA_data <- dplyr::filter(pipeline_output$Individual_data,
                            PopID %in% c("BAL", "SZE", "VES", "VIL", "GUL"))

  #Test 1: Adult tit female
  #Individual N194407 should be listed as a female
  expect_equal(subset(MTA_data, IndvID == "N194407")$Sex_calculated, "F")
  expect_equal(subset(MTA_data, IndvID == "N194407")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, IndvID == "N194407")$BroodIDLaid, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N194407")$BroodIDFledged, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N194407")$RingSeason, 2013L)
  expect_equal(subset(MTA_data, IndvID == "N194407")$RingAge, "adult")
  expect_equal(subset(MTA_data, IndvID == "N194407")$PopID, "VIL")

  #Test 2: Adult tit female
  #Individual N194553 should be listed as a female
  expect_equal(subset(MTA_data, IndvID == "N194553")$Sex_calculated, "F")
  expect_equal(subset(MTA_data, IndvID == "N194553")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, IndvID == "N194553")$BroodIDLaid, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N194553")$BroodIDFledged, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N194553")$RingSeason, 2013L)
  expect_equal(subset(MTA_data, IndvID == "N194553")$RingAge, "adult")
  expect_equal(subset(MTA_data, IndvID == "N194553")$PopID, "SZE")

  #Test 3: Adult tit male
  #Individual N309711 should be listed as a male
  expect_equal(subset(MTA_data, IndvID == "N309711")$Sex_calculated, "M")
  expect_equal(subset(MTA_data, IndvID == "N309711")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, IndvID == "N309711")$BroodIDLaid, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N309711")$BroodIDFledged, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N309711")$RingSeason, 2018L)
  expect_equal(subset(MTA_data, IndvID == "N309711")$RingAge, "adult")
  expect_equal(subset(MTA_data, IndvID == "N309711")$PopID, "BAL")

  #Test 4: Adult tit male
  #Individual N331771 should be listed as a male
  expect_equal(subset(MTA_data, IndvID == "N331771")$Sex_calculated, "M")
  expect_equal(subset(MTA_data, IndvID == "N331771")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, IndvID == "N331771")$BroodIDLaid, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N331771")$BroodIDFledged, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N331771")$RingSeason, 2020L)
  expect_equal(subset(MTA_data, IndvID == "N331771")$RingAge, "adult")
  expect_equal(subset(MTA_data, IndvID == "N331771")$PopID, "SZE")

  #Test 5: Adult tit male
  #Individual N350650 should be listed as a male
  expect_equal(subset(MTA_data, IndvID == "N350650")$Sex_calculated, "M")
  expect_equal(subset(MTA_data, IndvID == "N350650")$BroodIDLaid, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N350650")$BroodIDFledged, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N350650")$RingSeason, 2020L)
  expect_equal(subset(MTA_data, IndvID == "N350650")$RingAge, "adult")
  expect_equal(subset(MTA_data, IndvID == "N350650")$PopID, "GUL")

  #Test 6: Individual with not clear sex
  expect_equal(subset(MTA_data, IndvID == "N309315")$Sex_calculated, "C")
  expect_equal(subset(MTA_data, IndvID == "N309315")$BroodIDLaid, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N309315")$BroodIDFledged, NA_character_)
  expect_equal(subset(MTA_data, IndvID == "N309315")$RingSeason, 2017L)
  expect_equal(subset(MTA_data, IndvID == "N309315")$RingAge, "adult")
  expect_equal(subset(MTA_data, IndvID == "N309315")$PopID, "SZE")


})


test_that("Brood_data returns an expected outcome...", {

  #Take a subset of only MTA data
  MTA_data <- dplyr::filter(pipeline_output$Brood_data,
                            PopID %in% c("BAL", "SZE", "VES", "VIL", "GUL"))

  #Test 1: Tit brood where clutch type = first
  #BroodID 2119
  expect_equal(subset(MTA_data, BroodID == "2119")$ClutchType_calculated, "first")
  expect_equal(subset(MTA_data, BroodID == "2119")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, BroodID == "2119")$FemaleID, "N330892")
  expect_equal(subset(MTA_data, BroodID == "2119")$MaleID, "N292444")
  expect_equal(subset(MTA_data, BroodID == "2119")$LocationID, "SZ1")
  expect_equal(subset(MTA_data, BroodID == "2119")$BreedingSeason, 2019L)
  expect_equal(subset(MTA_data, BroodID == "2119")$ClutchSize_observed, 10L)

  #Test 2: Tit brood where clutch type = first
  #BroodID 545
  expect_equal(subset(MTA_data, BroodID == "545")$ClutchType_calculated, "first")
  expect_equal(subset(MTA_data, BroodID == "545")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, BroodID == "545")$FemaleID, "N172694")
  expect_equal(subset(MTA_data, BroodID == "545")$MaleID, "N217696")
  expect_equal(subset(MTA_data, BroodID == "545")$ClutchSize_observed, 11L)
  expect_equal(subset(MTA_data, BroodID == "545")$HatchDate_observed, as.Date("2014-04-15"))

  #Test 3: Tit brood where clutch type = second
  #BroodID 570
  expect_equal(subset(MTA_data, BroodID == "570")$ClutchType_observed, "second")
  expect_equal(subset(MTA_data, BroodID == "570")$ClutchType_calculated, "second")
  expect_equal(subset(MTA_data, BroodID == "570")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, BroodID == "570")$FemaleID, "N217018")
  expect_equal(subset(MTA_data, BroodID == "570")$MaleID, "N217752")
  expect_equal(subset(MTA_data, BroodID == "570")$LayDate_observed, as.Date("2014-05-15"))
  expect_equal(subset(MTA_data, BroodID == "570")$ClutchSize_observed, 8L)
  expect_equal(subset(MTA_data, BroodID == "570")$NumberFledged_observed, 7L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(MTA_data, BroodID == "570")$AvgChickMass, NA_real_)
  expect_equal(subset(MTA_data, BroodID == "570")$AvgTarsus, NA_real_)


  #Test 4: Tit brood where clutch type = replacement (due to past cutoff)
  #BroodID 783
  expect_equal(subset(MTA_data, BroodID == "783")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, BroodID == "783")$ClutchType_calculated, "replacement")
  expect_equal(subset(MTA_data, BroodID == "783")$FemaleID, "N254451")
  expect_equal(subset(MTA_data, BroodID == "783")$MaleID, "N254452")
  expect_equal(subset(MTA_data, BroodID == "783")$LayDate_observed, as.Date("2015-05-03"))
  expect_equal(subset(MTA_data, BroodID == "783")$ClutchSize_observed, 6L)
  expect_equal(subset(MTA_data, BroodID == "783")$NumberFledged_observed, 3L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(MTA_data, BroodID == "783")$AvgChickMass, NA_real_)
  expect_equal(subset(MTA_data, BroodID == "783")$AvgTarsus, NA_real_)


  #Test 5: Tit brood where no parents are known
  #BroodID 1001
  expect_equal(subset(MTA_data, BroodID == "1001")$Species, "PARMAJ")
  expect_equal(subset(MTA_data, BroodID == "1001")$FemaleID, "unringed")
  expect_equal(subset(MTA_data, BroodID == "1001")$MaleID, "unringed")
  expect_equal(subset(MTA_data, BroodID == "1001")$LayDate_observed, as.Date("2016-04-10"))
  expect_equal(subset(MTA_data, BroodID == "1001")$ClutchSize_observed, 12L)
  expect_equal(subset(MTA_data, BroodID == "1001")$NumberFledged_observed, 12L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(MTA_data, BroodID == "1001")$AvgChickMass, NA_real_)
  expect_equal(subset(MTA_data, BroodID == "1001")$AvgTarsus, NA_real_)

})


test_that("Capture_data returns an expected outcome...", {

  #### NEED TO FIX THE CAPTURE DATE AFTER THE RESPONSE FROM DATA OWNERS

  #Take a subset of only MTA data
  MTA_data <- dplyr::filter(pipeline_output$Capture_data,
                            CapturePopID %in% c("BAL", "SZE", "VES", "VIL", "GUL"))

  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(MTA_data, IndvID == "N194407")), 4L)
  #Test the first capture of the female
  expect_equal(subset(MTA_data, IndvID == "N194407")$Sex_observed[1], "F")
  expect_equal(subset(MTA_data, IndvID == "N194407")$LocationID[1], "VI108")
  expect_equal(subset(MTA_data, IndvID == "N194407")$CapturePopID[1], "VIL")
  expect_equal(min(subset(MTA_data, IndvID == "N194407")$CaptureDate, na.rm = TRUE), as.Date("2013-04-01"))

  #Test 2: Male caught as adult
  #Test the male has the correct number of capture records
  expect_equal(nrow(subset(MTA_data, IndvID == "N309711")), 2L)
  #Test the first capture of the male
  expect_equal(subset(MTA_data, IndvID == "N309711")$Sex_observed[1], "M")
  expect_equal(subset(MTA_data, IndvID == "N309711")$LocationID[1], "B14")
  expect_equal(subset(MTA_data, IndvID == "N309711")$CapturePopID[1], "BAL")
  expect_equal(min(subset(MTA_data, IndvID == "N309711")$CaptureDate, na.rm = TRUE), as.Date("2018-04-01"))

  #Test 3: Male caught first time as adult
  #Test the male has the correct number of capture records
  expect_equal(nrow(subset(MTA_data, IndvID == "N172849")), 5L)
  expect_equal(subset(MTA_data, IndvID == "N172849")$Sex_observed[1], "M")
  expect_equal(subset(MTA_data, IndvID == "N172849")$LocationID[1], "VI108")
  expect_equal(subset(MTA_data, IndvID == "N172849")$BreedingSeason[1], 2013L)
  expect_equal(subset(MTA_data, IndvID == "N172849")$CapturePopID[1], "VIL")
  expect_equal(min(subset(MTA_data, IndvID == "N172849")$CaptureDate, na.rm = TRUE), as.Date("2013-04-01"))

  #Test 4: Female caught first time as adult
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(MTA_data, IndvID == "N172861")), 3L)
  #Test the first capture of the male
  expect_equal(subset(MTA_data, IndvID == "N172861")$Sex_observed[1], "F")
  expect_equal(subset(MTA_data, IndvID == "N172861")$LocationID[1], "VI104")
  expect_equal(subset(MTA_data, IndvID == "N172861")$BreedingSeason[1], 2013L)
  expect_equal(subset(MTA_data, IndvID == "N172861")$CapturePopID[1], "VIL")
  expect_equal(min(subset(MTA_data, IndvID == "N172861")$CaptureDate, na.rm = TRUE), as.Date("2013-04-01"))

})


#### ok Location_data

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only MTA data
  MTA_data <- dplyr::filter(pipeline_output$Location_data,
                            PopID %in% c("BAL", "SZE", "VES", "VIL", "GUL"))

  #Test 1: Nestbox evergreen
  #LocationType is as expected
  expect_equal(subset(MTA_data, LocationID == "B61")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(MTA_data, LocationID == "B61")$HabitatType, "urban")
  #Start season
  expect_equal(subset(MTA_data, LocationID == "B61")$StartSeason, 2015L)
  #End season
  expect_equal(subset(MTA_data, LocationID == "B61")$EndSeason, NA_integer_)
  expect_equal(subset(MTA_data, LocationID == "B61")$PopID, "BAL")

  #Test 2: Nestbox
  #LocationType is as expected
  expect_equal(subset(MTA_data, LocationID == "SZ83")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(MTA_data, LocationID == "SZ83")$HabitatType, "deciduous")
  #Start season
  expect_equal(subset(MTA_data, LocationID == "SZ83")$StartSeason, 2014L)
  #End season
  expect_equal(subset(MTA_data, LocationID == "SZ83")$EndSeason, NA_integer_)
  expect_equal(subset(MTA_data, LocationID == "SZ83")$PopID, "SZE")

  #Test 3: Nestbox
  #LocationType is as expected
  expect_equal(subset(MTA_data, LocationID == "V44")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(MTA_data, LocationID == "V44")$HabitatType, "urban")
  #Start season
  expect_equal(subset(MTA_data, LocationID == "V44")$StartSeason, 2013L)
  #End season
  expect_equal(subset(MTA_data, LocationID == "V44")$EndSeason, NA_integer_)
  expect_equal(subset(MTA_data, LocationID == "V44")$PopID, "VES")

  #Test 4: Nestbox
  #LocationType is as expected
  expect_equal(subset(MTA_data, LocationID == "G20")$LocationType, "NB")
  #Habitat is as expected
  expect_equal(subset(MTA_data, LocationID == "G20")$HabitatType, NA_character_)
  #Start season
  expect_equal(subset(MTA_data, LocationID == "G20")$StartSeason, 2019L)
  #End season
  expect_equal(subset(MTA_data, LocationID == "G20")$EndSeason, NA_integer_)
  expect_equal(subset(MTA_data, LocationID == "G20")$PopID, "GUL")

})

