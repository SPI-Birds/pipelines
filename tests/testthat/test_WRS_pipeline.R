context("Run data quality check on Warswaw, Pland, pipeline output")

test_that("WRS outputs all files...", {

  expect_true(all(c("WRS") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("WRS") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("WRS") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("WRS") %in% pipeline_output$Location_data$PopID))

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("WRS"))

  #Individual K7V3532
  expect_equal(subset(WRS_data, IndvID == "K7V3532")$Species, "PARMAJ") # PARMAJ
  expect_equal(subset(WRS_data, IndvID == "K7V3532")$PopID, "WRS") # WRS
  expect_equal(subset(WRS_data, IndvID == "K7V3532")$BroodIDLaid, NA_character_) # NA
  expect_equal(subset(WRS_data, IndvID == "K7V3532")$BroodIDFledged, NA_character_) # NA
  expect_equal(subset(WRS_data, IndvID == "K7V3532")$RingSeason, 2016) # 2016
  expect_equal(subset(WRS_data, IndvID == "K7V3532")$RingAge, "adult") # adult
  expect_equal(subset(WRS_data, IndvID == "K7V3532")$Sex_calculated, "F") # F
  expect_equal(subset(WRS_data, IndvID == "K7V3532")$Sex_genetic, NA_character_) # NA

  #Individual K7V3532
  expect_equal(subset(WRS_data, IndvID == "K7V4627")$Species, "CYACAE") # CYACAE
  expect_equal(subset(WRS_data, IndvID == "K7V4627")$PopID, "WRS") # WRS
  expect_equal(subset(WRS_data, IndvID == "K7V4627")$BroodIDLaid, NA_character_) # NA
  expect_equal(subset(WRS_data, IndvID == "K7V4627")$BroodIDFledged, NA_character_) # NA
  expect_equal(subset(WRS_data, IndvID == "K7V4627")$RingSeason, 2017) # 2017
  expect_equal(subset(WRS_data, IndvID == "K7V4627")$RingAge, "adult") # adult
  expect_equal(subset(WRS_data, IndvID == "K7V4627")$Sex_calculated, "M") # M
  expect_equal(subset(WRS_data, IndvID == "K7V4627")$Sex_genetic, NA_character_) # NA

  #Individual K7V4826
  expect_equal(subset(WRS_data, IndvID == "K7V4826")$Species, "PARMAJ") # PARMAJ
  expect_equal(subset(WRS_data, IndvID == "K7V4826")$PopID, "WRS") # WRS
  expect_equal(!is.na(subset(WRS_data, IndvID == "K7V4826")$BroodIDLaid), TRUE) # TRUE
  expect_equal(!is.na(subset(WRS_data, IndvID == "K7V4826")$BroodIDFledged), TRUE) # TRUE
  expect_equal(subset(WRS_data, IndvID == "K7V4826")$RingSeason, 2017) # 2017
  expect_equal(subset(WRS_data, IndvID == "K7V4826")$RingAge, "chick") # chick
  expect_equal(subset(WRS_data, IndvID == "K7V4826")$Sex_calculated, NA_character_) # NA
  expect_equal(subset(WRS_data, IndvID == "K7V4826")$Sex_genetic, NA_character_) # NA

  #Individual K4Z2244
  expect_equal(subset(WRS_data, IndvID == "K4Z2244")$Species, "CYACAE") # CYACAE
  expect_equal(subset(WRS_data, IndvID == "K4Z2244")$PopID, "WRS") # WRS
  expect_equal(!is.na(subset(WRS_data, IndvID == "K4Z2244")$BroodIDLaid), TRUE) # TRUE
  expect_equal(!is.na(subset(WRS_data, IndvID == "K4Z2244")$BroodIDFledged), TRUE) # TRUE
  expect_equal(subset(WRS_data, IndvID == "K4Z2244")$RingSeason, 2019) # 2019
  expect_equal(subset(WRS_data, IndvID == "K4Z2244")$RingAge, "chick") # chick
  expect_equal(subset(WRS_data, IndvID == "K4Z2244")$Sex_calculated, "M") # M
  expect_equal(subset(WRS_data, IndvID == "K4Z2244")$Sex_genetic, NA_character_) # NA


})

test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("WRS"))

  ## General brood data
  expect_equal(subset(WRS_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$ClutchSize_observed, 10)
  expect_equal(subset(WRS_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$BroodSize_observed, 9)
  expect_equal(subset(WRS_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$NumberFledged_observed, 9)
  expect_equal(subset(WRS_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$LayDate_observed, as.Date("2014-04-25"))
  expect_equal(subset(WRS_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$LayDate_min, as.Date("2014-04-25"))
  expect_equal(subset(WRS_data, BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "704")$LayDate_max, as.Date("2014-05-04"))

  ## Case where there were multiple clutches laid at the same location
  expect_equal(nrow(subset(WRS_data, BreedingSeason == "2019"
                           & PopID == "SAL"
                           & LocationID == "249")), 2)

  ## Brood where clutch type observed = replacement
  expect_equal(subset(WRS_data, BreedingSeason == "2015"
                      & PopID == "SAL"
                      & LocationID == "235" &
                        is.na(LayDate_observed))$ClutchType_observed, "replacement")

  ## Brood where chick weight, but not tarsus is measured
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2018"
                      & PopID == "CAS"
                      & LocationID == "28")$AvgChickMass, 10.1)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2018"
                      & PopID == "CAS"
                      & LocationID == "28")$NumberChicksMass, 8)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2018"
                      & PopID == "CAS"
                      & LocationID == "28")$AvgTarsus, NA_real_)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2018"
                      & PopID == "CAS"
                      & LocationID == "28")$NumberChicksTarsus, NA_integer_)

  ## Case where species is ambiguous, but the species information from the ringing data  was used to assign species for the brood
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2020"
                      & PopID == "SAL"
                      & LocationID == "204")$Species, "PARMAJ")

  ## Case where both FemaleID and MaleID are known
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2020"
                      & PopID == "KEL"
                      & LocationID == "534")$FemaleID, "ACJ2320")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2020"
                      & PopID == "KEL"
                      & LocationID == "534")$MaleID, "ACJ2305")

  ## Case where female had two nests in the same year
  expect_equal(nrow(subset(WRS_data, FemaleID == "TX11502" & BreedingSeason == 2017)), 2)

  ## Cases where dates are in different formats for laying columns in primary data
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "710")$LayDate_observed, as.Date("2014-05-05"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "710")$LayDate_max, as.Date("2014-05-16"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "729")$LayDate_max, as.Date("2014-05-01"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2014"
                      & PopID == "GAR"
                      & LocationID == "724")$LayDate_min, as.Date("2014-05-01"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2015"
                      & PopID == "GAR"
                      & LocationID == "724")$LayDate_min, as.Date("2015-05-08"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2017"
                      & PopID == "SAL"
                      & LocationID == "227")$LayDate_min, as.Date("2017-04-30"))

  ## Check experiment groups
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "KEL"
                      & LocationID == "548")$ExperimentID, "PARENTAGE")


  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "KEL"
                      & LocationID == "550")$ExperimentID, "OTHER")

  expect_equal(subset(WRS_data,
                      BreedingSeason == "2017"
                      & PopID == "SCE"
                      & LocationID == "41")$ExperimentID, "OTHER")

  expect_equal(subset(WRS_data,
                      BreedingSeason == "2018"
                      & PopID == "SCE"
                      & LocationID == "175")$ExperimentID, "OTHER")

  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "SAL"
                      & LocationID == "229")$ExperimentID, "COHORT")



})

test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("CAS", "GAR", "KEL", "SAL", "SCE"))

  ## Case where chick found dead after ringed
  expect_equal(subset(WRS_data, IndvID == "ACJ2314")$ReleaseAlive, FALSE) # Should be False
  expect_equal(subset(WRS_data, IndvID == "ACJ2314")$ReleasePopID, NA_character_) # Should be NA (since dead)
  expect_equal(subset(WRS_data, IndvID == "ACJ2314")$Species, "CYACAE") # Should be blue tit
  expect_equal(subset(WRS_data, IndvID == "ACJ2314")$Mass, 9.42) # Should be 9.42
  expect_equal(subset(WRS_data, IndvID == "ACJ2314")$BreedingSeason, 2020) # Should be 2019
  expect_equal(subset(WRS_data, IndvID == "ACJ2314")$Age_observed, 1) # Should be 1

  # Case where individual recorded at different locations in nest and ringing data
  expect_equal(nrow(subset(WRS_data, IndvID == "S034047" & BreedingSeason <= 2018)), 3) # Three records
  expect_equal(subset(WRS_data, IndvID == "S034047" &
                        BreedingSeason == 2017)$Sex_observed, "F") # Female
  expect_equal(subset(WRS_data, IndvID == "S034047" &
                        CaptureDate == as.Date("2018-05-01"))$LocationID, "65") # LocationID 65

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("CAS", "GAR", "KEL", "SAL", "SCE"))

  ## Nestbox 728 in GAR
  expect_equal(subset(WRS_data, LocationID == "728" &
                        PopID == "GAR")$LocationType, "NB") ## Nestbox
  expect_equal(subset(WRS_data, LocationID == "728" &
                        PopID == "GAR")$HabitatType, "urban") ## Urban
  expect_equal(subset(WRS_data, LocationID == "728" &
                        PopID == "GAR")$StartSeason, 2015) ## 2015
  expect_equal(subset(WRS_data, LocationID == "728" &
                        PopID == "GAR")$EndSeason, NA_integer_) ## NA

  ## Same nestbox number at 3 populations
  #LocationType is as expected
  expect_equal(subset(WRS_data, LocationID == "10")$PopID, c("CAS", "KEL", "SCE"))


})

