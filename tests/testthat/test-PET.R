testthat::skip_if(!exists("data_path"))

pipeline_output <- format_PET(db = paste0(data_path, "/PET_Petrozavodsk_Russia"))

test_that("PET outputs all files...", {

  expect_true("PET" %in% pipeline_output$Brood_data$PopID)
  expect_true("PET" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("PET" %in% pipeline_output$Individual_data$PopID)
  expect_true("PET" %in% pipeline_output$Location_data$PopID)

})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only PET data
  PET_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "PET")

  #### PARMAJ
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

  #Test 4: Individual great tit with sex unknown
  expect_equal(subset(PET_data, IndvID == "KS87460")$Sex_calculated, NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87460")$Species, "PARMAJ")
  expect_equal(subset(PET_data, IndvID == "KS87460")$BroodIDLaid, "2016_Z_4_13")
  expect_equal(subset(PET_data, IndvID == "KS87460")$BroodIDFledged, "2016_Z_4_13")
  expect_equal(subset(PET_data, IndvID == "KS87460")$RingSeason, 2016)
  expect_equal(subset(PET_data, IndvID == "KS87460")$RingAge, "chick")

  #### FICHYP
  #Test 1: Adult flycatcher female
  expect_equal(subset(PET_data, IndvID == "VK93093")$Sex_calculated, "F")
  expect_equal(subset(PET_data, IndvID == "VK93093")$Species, "FICHYP")
  expect_equal(subset(PET_data, IndvID == "VK93093")$BroodIDLaid, NA_character_)
  expect_equal(subset(PET_data, IndvID == "VK93093")$BroodIDFledged, NA_character_)
  expect_equal(subset(PET_data, IndvID == "VK93093")$RingSeason, 2016L)
  expect_equal(subset(PET_data, IndvID == "VK93093")$RingAge, "adult")

  #Test 2: Adult flycatcher female (there are no males ringed in the dataset)
  expect_equal(subset(PET_data, IndvID == "VT76523")$Sex_calculated, "F")
  expect_equal(subset(PET_data, IndvID == "VT76523")$Species, "FICHYP")
  expect_equal(subset(PET_data, IndvID == "VT76523")$BroodIDLaid, "2016_Z_11A_12")
  expect_equal(subset(PET_data, IndvID == "VT76523")$BroodIDFledged,  "2016_Z_11A_12")
  expect_equal(subset(PET_data, IndvID == "VT76523")$RingSeason, 2016L)
  expect_equal(subset(PET_data, IndvID == "VT76523")$RingAge, "chick")

  #Test 3: Individual flycatcher ringed as chick
  expect_equal(subset(PET_data, IndvID == "VC22118")$Sex_calculated, NA_character_)
  expect_equal(subset(PET_data, IndvID == "VC22118")$Species, "FICHYP")
  expect_equal(subset(PET_data, IndvID == "VC22118")$BroodIDLaid, "2019_Z_1_23")
  expect_equal(subset(PET_data, IndvID == "VC22118")$BroodIDFledged, "2019_Z_1_23")
  expect_equal(subset(PET_data, IndvID == "VC22118")$RingSeason, 2019L)
  expect_equal(subset(PET_data, IndvID == "VC22118")$RingAge, "chick")

  #Test 4: Individual flycatcher with sex unknown
  expect_equal(subset(PET_data, IndvID == "VC22011")$Sex_calculated, NA_character_)
  expect_equal(subset(PET_data, IndvID == "VC22011")$Species, "FICHYP")
  expect_equal(subset(PET_data, IndvID == "VC22011")$BroodIDLaid, "2019_S_2_36")
  expect_equal(subset(PET_data, IndvID == "VC22011")$BroodIDFledged, "2019_S_2_36")
  expect_equal(subset(PET_data, IndvID == "VC22011")$RingSeason, 2019L)
  expect_equal(subset(PET_data, IndvID == "VC22011")$RingAge, "chick")

  #### JYNTOR
  #Test 1: Adult wryneck female
  expect_equal(subset(PET_data, IndvID == "KS87477")$Sex_calculated, "F")
  expect_equal(subset(PET_data, IndvID == "KS87477")$Species, "JYNTOR")
  expect_equal(subset(PET_data, IndvID == "KS87477")$BroodIDLaid, NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87477")$BroodIDFledged, NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87477")$RingSeason, 2016L)
  expect_equal(subset(PET_data, IndvID == "KS87477")$RingAge, "adult")

  #Test 2: Individual wryneck ringed as chick
  expect_equal(subset(PET_data, IndvID == "KS87489")$Sex_calculated, NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87489")$Species, "JYNTOR")
  expect_equal(subset(PET_data, IndvID == "KS87489")$BroodIDLaid, "2016_S_18_51")
  expect_equal(subset(PET_data, IndvID == "KS87489")$BroodIDFledged, "2016_S_18_51")
  expect_equal(subset(PET_data, IndvID == "KS87489")$RingSeason, 2016L)
  expect_equal(subset(PET_data, IndvID == "KS87489")$RingAge, "chick")

  #Test 3: Individual wryneck with sex unknown
  expect_equal(subset(PET_data, IndvID == "XX84867")$Sex_calculated, NA_character_)
  expect_equal(subset(PET_data, IndvID == "XX84867")$Species, "JYNTOR")
  expect_equal(subset(PET_data, IndvID == "XX84867")$BroodIDLaid, "2019_D_2A_26")
  expect_equal(subset(PET_data, IndvID == "XX84867")$BroodIDFledged, "2019_D_2A_26")
  expect_equal(subset(PET_data, IndvID == "XX84867")$RingSeason, 2019L)
  expect_equal(subset(PET_data, IndvID == "XX84867")$RingAge, "chick")

})


test_that("Brood_data returns an expected outcome...", {

  #Take a subset of only PET data
  PET_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "PET")

  #### PARMAJ
  #Test 1: Tit brood where clutch type = first
  expect_equal(subset(PET_data, BroodID == "2016_S_3_28")$ClutchType_calculated, "first")
  expect_equal(subset(PET_data, BroodID == "2016_S_3_28")$Species, "PARMAJ")
  expect_equal(subset(PET_data, BroodID == "2016_S_3_28")$FemaleID, "KS87465")
  expect_equal(subset(PET_data, BroodID == "2016_S_3_28")$MaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2016_S_3_28")$HatchDate_observed, as.Date("2016-06-16"))
  expect_equal(subset(PET_data, BroodID == "2016_S_3_28")$ClutchSize_observed, 11L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2016_S_3_28")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2016_S_3_28")$AvgTarsus, NA_real_)

  #Test 2: Tit brood where clutch type = first
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$ClutchType_calculated, "first")
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$ClutchType_observed, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$Species, "PARMAJ")
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$FemaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$MaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$HatchDate_observed, as.Date("2018-05-27"))
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$ClutchSize_observed, 11L)
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$NumberFledged_observed, 11L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2018_S_14_6")$AvgTarsus, NA_real_)

  #Test 3: Tit brood where clutch type = replacement (due to past cutoff)
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$Species, "PARMAJ")
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$ClutchType_calculated, "replacement")
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$FemaleID, "KS87781")
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$MaleID, "KS87780")
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$LayDate_observed, as.Date("2018-06-21"))
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$HatchDate_observed, as.Date("2018-07-10"))
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$ClutchSize_observed, 8L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2018_S_18_52")$AvgTarsus, NA_real_)


  #### FICHYP
  #Test 1: Brood where clutch type = first
  expect_equal(subset(PET_data, BroodID == "2015_S_16_31")$ClutchType_calculated, "first")
  expect_equal(subset(PET_data, BroodID == "2015_S_16_31")$Species, "FICHYP")
  expect_equal(subset(PET_data, BroodID == "2015_S_16_31")$FemaleID, "VK93207")
  expect_equal(subset(PET_data, BroodID == "2015_S_16_31")$MaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2015_S_16_31")$HatchDate_observed, as.Date("2015-06-15"))
  expect_equal(subset(PET_data, BroodID == "2015_S_16_31")$ClutchSize_observed, 6L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2015_S_16_31")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2015_S_16_31")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = first
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$ClutchType_calculated, "first")
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$ClutchType_observed, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$Species, "FICHYP")
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$FemaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$MaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$HatchDate_observed, as.Date("2015-06-30"))
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$ClutchSize_observed, 4L)
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$NumberFledged_observed, 0L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2015_S_13B_46")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (due to past cutoff)
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$Species, "FICHYP")
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$ClutchType_calculated, "replacement")
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$FemaleID, "VK94297")
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$MaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$LayDate_observed, as.Date("2018-06-17"))
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$HatchDate_observed, as.Date("2018-07-07"))
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$ClutchSize_observed, 6L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2018_S_13_48")$AvgTarsus, NA_real_)


  #### JYNTOR
  #Test 1: Brood where clutch type = first
  expect_equal(subset(PET_data, BroodID == "2016_S_18_51")$ClutchType_calculated, "first")
  expect_equal(subset(PET_data, BroodID == "2016_S_18_51")$Species, "JYNTOR")
  expect_equal(subset(PET_data, BroodID == "2016_S_18_51")$FemaleID, "KS87477")
  expect_equal(subset(PET_data, BroodID == "2016_S_18_51")$MaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2016_S_18_51")$HatchDate_observed, as.Date("2016-07-10"))
  expect_equal(subset(PET_data, BroodID == "2016_S_18_51")$ClutchSize_observed, 10L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2016_S_18_51")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2016_S_18_51")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = first
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$ClutchType_calculated, "first")
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$ClutchType_observed, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$Species, "JYNTOR")
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$FemaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$MaleID, NA_character_)
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$HatchDate_observed, as.Date("2019-06-14"))
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$ClutchSize_observed, 12L)
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$NumberFledged_observed, 8L)
  #AvgChickMass and AvgTarsus should be NA, there were no chicks measured
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$AvgChickMass, NA_real_)
  expect_equal(subset(PET_data, BroodID == "2019_D_2A_26")$AvgTarsus, NA_real_)


})

test_that("Capture_data returns an expected outcome...", {

  #Take a subset of only PET data
  PET_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "PET")

  #### PARMAJ
  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (2)
  expect_equal(nrow(subset(PET_data, IndvID == "KS87496")), 2)
  expect_equal(unique(subset(PET_data, IndvID == "KS87496")$Species), "PARMAJ")
  #Test the first capture of the female
  expect_equal(subset(PET_data, IndvID == "KS87496")$Sex_observed[1], "F")
  expect_equal(subset(PET_data, IndvID == "KS87496")$LocationID[1], "31")
  ## TODO: CHANGE AFTER UPDATE FROM DATA OWNERS
  expect_equal(min(subset(PET_data, IndvID == "KS87496")$CaptureDate, na.rm = TRUE), as.Date("2017-04-01"))

  #Test 2: Male caught as adult
  expect_equal(nrow(subset(PET_data, IndvID == "KS87763")), 3)
  expect_equal(unique(subset(PET_data, IndvID == "KS87763")$Species), "PARMAJ")
  #Test the first capture of the male
  expect_equal(subset(PET_data, IndvID == "KS87763")$Sex_observed[1], "M")
  expect_equal(subset(PET_data, IndvID == "KS87763")$LocationID[1], "7")
  ## TODO: CHANGE AFTER UPDATE FROM DATA OWNERS
  expect_equal(min(subset(PET_data, IndvID == "KS87763")$CaptureDate, na.rm = TRUE), as.Date("2018-04-01"))

  #Test 3: Female caught first time as chick
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(PET_data, IndvID == "KS87645")), 2)
  expect_equal(unique(subset(PET_data, IndvID == "KS87645")$Species), "PARMAJ")
  #Test the first capture of the female
  expect_equal(subset(PET_data, IndvID == "KS87645")$Sex_observed[1], NA_character_)
  expect_equal(subset(PET_data, IndvID == "KS87645")$LocationID[1], "5")
  expect_equal(subset(PET_data, IndvID == "KS87645")$BreedingSeason[1], 2015L)
  expect_equal(min(subset(PET_data, IndvID == "KS87645")$CaptureDate, na.rm = TRUE), as.Date("2015-04-01"))


  #### FICHYP
  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (2)
  expect_equal(nrow(subset(PET_data, IndvID == "VK93089")), 1)
  expect_equal(unique(subset(PET_data, IndvID == "VK93089")$Species), "FICHYP")
  expect_equal(subset(PET_data, IndvID == "VK93089")$Sex_observed[1], "F")
  expect_equal(subset(PET_data, IndvID == "VK93089")$LocationID[1], "7")
  ## TODO: CHANGE AFTER UPDATE FROM DATA OWNERS
  expect_equal(min(subset(PET_data, IndvID == "VK93089")$CaptureDate, na.rm = TRUE), as.Date("2016-04-01"))

  #Test 2: Female caught as chick
  expect_equal(nrow(subset(PET_data, IndvID == "VT76523")), 2)
  expect_equal(unique(subset(PET_data, IndvID == "VT76523")$Species), "FICHYP")
  expect_equal(subset(PET_data, IndvID == "VT76523")$Sex_observed[2], "F")
  expect_equal(subset(PET_data, IndvID == "VT76523")$LocationID[1], "11A")
  ## TODO: CHANGE AFTER UPDATE FROM DATA OWNERS
  expect_equal(min(subset(PET_data, IndvID == "VT76523")$CaptureDate, na.rm = TRUE), as.Date("2016-04-01"))
  expect_equal(subset(PET_data, IndvID == "VT76523")$BreedingSeason[1], 2016L)


  #### JYNTOR
  #Test 1: Female caught as adult
  #Test the female has the correct number of capture records (2)
  expect_equal(nrow(subset(PET_data, IndvID == "KS87477")), 1)
  expect_equal(unique(subset(PET_data, IndvID == "KS87477")$Species), "JYNTOR")
  expect_equal(subset(PET_data, IndvID == "KS87477")$Sex_observed[1], "F")
  expect_equal(subset(PET_data, IndvID == "KS87477")$LocationID[1], "18")
  ## TODO: CHANGE AFTER UPDATE FROM DATA OWNERS
  expect_equal(min(subset(PET_data, IndvID == "KS87477")$CaptureDate, na.rm = TRUE), as.Date("2016-04-01"))

  #Test 2: Individual caught as chick
  expect_equal(nrow(subset(PET_data, IndvID == "XX84871")), 1)
  expect_equal(unique(subset(PET_data, IndvID == "XX84871")$Species), "JYNTOR")
  expect_equal(subset(PET_data, IndvID == "XX84871")$Sex_observed[1], NA_character_)
  expect_equal(subset(PET_data, IndvID == "XX84871")$LocationID[1], "2A")
  ## TODO: CHANGE AFTER UPDATE FROM DATA OWNERS
  expect_equal(min(subset(PET_data, IndvID == "XX84871")$CaptureDate, na.rm = TRUE), as.Date("2019-04-01"))
  expect_equal(subset(PET_data, IndvID == "XX84871")$BreedingSeason[1], 2019L)

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only PET data
  PET_data <- dplyr::filter(pipeline_output$Location_data, PopID == "PET")

  #Test 1: Nestbox
  #LocationType is as expected
  expect_equal(subset(PET_data, NestboxID == "17A")$LocationType, "NB")
  expect_equal(subset(PET_data, NestboxID == "17A")$LocationID, "S")
  #Habitat is as expected
  expect_equal(subset(PET_data, NestboxID == "17A")$HabitatType, "mixed")
  #Start season
  expect_equal(subset(PET_data, NestboxID == "17A")$StartSeason, 2017L)
  #End season
  expect_equal(subset(PET_data, NestboxID == "17A")$EndSeason, NA_integer_)

  #Test 2: Nestbox
  #LocationType is as expected
  expect_equal(subset(PET_data, NestboxID == "10G")$LocationType, "NB")
  expect_equal(subset(PET_data, NestboxID == "10G")$LocationID, "Z")
  #Habitat is as expected
  expect_equal(subset(PET_data, NestboxID == "10G")$HabitatType, "mixed")
  #Start season
  expect_equal(subset(PET_data, NestboxID == "10G")$StartSeason, 2016)
  #End season
  expect_equal(subset(PET_data, NestboxID == "10G")$EndSeason, NA_integer_)

  #Test 3: Nestbox
  #LocationType is as expected
  expect_equal(subset(PET_data, NestboxID == "33")$LocationType, "NB")
  expect_equal(subset(PET_data, NestboxID == "33")$LocationID, "Z")
  #Habitat is as expected
  expect_equal(subset(PET_data, NestboxID == "33")$HabitatType, "mixed")
  #Start season
  expect_equal(subset(PET_data, NestboxID == "33")$StartSeason, 2018)
  #End season
  expect_equal(subset(PET_data, NestboxID == "33")$EndSeason, NA_integer_)

})

## All characters should be ASCII
test_that("No non-ASCII characters are present", {

  ## Brood data
  expect_invisible(for (col in colnames(pipeline_output[[1]])){

    if(is.character(pipeline_output[[1]] %>%
                    pull(col))){

      all(Encoding(pipeline_output[[1]] %>%
                     pull(col)) == "unknown")

    }})


  ## Capture data
  expect_invisible(for (col in colnames(pipeline_output[[2]])){

    if(is.character(pipeline_output[[2]] %>%
                    pull(col))){

      all(Encoding(pipeline_output[[2]] %>%
                     pull(col)) == "unknown")

    }})

  ## Individual data
  expect_invisible(for (col in colnames(pipeline_output[[3]])){

    if(is.character(pipeline_output[[3]] %>%
                    pull(col))){

      all(Encoding(pipeline_output[[3]] %>%
                     pull(col)) == "unknown")

    }})

  ## Location data
  expect_invisible(for (col in colnames(pipeline_output[[4]])){

    if(is.character(pipeline_output[[4]] %>%
                    pull(col))){

      all(Encoding(pipeline_output[[4]] %>%
                     pull(col)) == "unknown")

    }})
})




## General tests (for pipelines formatted to standard protocol version 1.1.0)

test_that("Expected columns are present", {

  ## Will fail if not all the expected columns are present

  ## Brood data: Test that all columns are present
  test_col_present(pipeline_output, "Brood")

  ## Capture data: Test that all columns are present
  test_col_present(pipeline_output, "Capture")

  ## Individual data: Test that all columns are present
  test_col_present(pipeline_output, "Individual")

  ## Location data: Test that all columns are present
  test_col_present(pipeline_output, "Location")

})

test_that("Column classes are as expected", {

  ## Will fail if columns that are shared by the output and the templates have different classes.

  # ## Brood data: Test that all column classes are expected
  # test_col_classes(pipeline_output, "Brood")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location")

})


test_that("ID columns match the expected format for the pipeline", {

  # ## FemaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[:alpha:]{2}[:digit:]{5}$")

  # ## MaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[:alpha:]{2}[:digit:]{5}$")

  # ## IndvID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[:alpha:]{2}[:digit:]{5}$")

  pipeline_output[[2]]$IndvID[which(!stringr::str_detect(pipeline_output[[2]]$IndvID, "^[:alpha:]{2}[:digit:]{5}$"))]

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[:alpha:]{2}[:digit:]{5}$")

  # pipeline_output[[3]]$IndvID[which(!stringr::str_detect(pipeline_output[[2]]$IndvID, "^[:alpha:]{2}[:digit:]{5}$"))]


})


test_that("Key columns only contain unique values", {

  ## BroodID has only unique values

  ## Requires changes made by the data owner.
  test_unique_values(pipeline_output, "BroodID")

  ## CaptureID has only unique values
  test_unique_values(pipeline_output, "CaptureID")

  ## PopID-IndvID has only unique values
  test_unique_values(pipeline_output, "PopID-IndvID")

})


test_that("Key columns in each table do not have NAs", {

  ## Brood
  test_NA_columns(pipeline_output, "Brood")

  ## Capture
  test_NA_columns(pipeline_output, "Capture")

  ## Individual
  test_NA_columns(pipeline_output, "Individual")

  ## Location
  test_NA_columns(pipeline_output, "Location")

})


test_that("Categorical columns do not have unexpected values", {

  ## Brood
  test_category_columns(pipeline_output, "Brood")

  ## Capture
  test_category_columns(pipeline_output, "Capture")

  ## Individual
  test_category_columns(pipeline_output, "Individual")

  ## Location
  test_category_columns(pipeline_output, "Location")

})

