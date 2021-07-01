pipeline_output <- format_WRS(db = paste0(data_path, "/WRS_Warsaw_Poland"))

context("Run data quality check on Warswaw, Pland, pipeline output")

test_that("WRS outputs all files...", {

  expect_true(all(c("WRS") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("WRS") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("WRS") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("WRS") %in% pipeline_output$Location_data$PopID))

})

test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("WRS"))

  ## General brood data - Location POL33
  expect_equal(!is.na(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$BroodID), TRUE)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$BreedingSeason, 2016)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$Species, "PARMAJ")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$Plot, "POL")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$FemaleID, NA_character_)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$MaleID, NA_character_)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$LayDate_observed, as.Date("2016-04-24"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$ClutchSize_observed, 5)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$HatchDate_observed, as.Date("2016-05-14"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$BroodSize_observed, 5)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$NumberFledged_observed, 0)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$AvgEggMass, 7.1/4)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "POL33")$NumberEggs, 4)


  ## General brood data - Location KPN46 in 2016 - first brood of two
  expect_equal(!is.na(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$BroodID), TRUE)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$BreedingSeason, 2016)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$Species, "PARMAJ")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$Plot, "KPN")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$FemaleID, "K7V3107")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$MaleID, "K7V3106")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$ClutchSize_observed, 8)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$HatchDate_observed, as.Date("2016-05-10"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$BroodSize_observed, 8)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$NumberFledged_observed, 4)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$AvgEggMass, 6.7/4)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2016"
                      & PopID == "WRS"
                      & LocationID == "KPN46"
                      & LayDate_observed == "2016-04-19")$NumberEggs, 4)


  ## General brood data - Location POL57
  expect_equal(!is.na(subset(WRS_data,
                             BreedingSeason == "2019"
                             & PopID == "WRS"
                             & LocationID == "POL57")$BroodID), TRUE)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$BreedingSeason, 2019)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$Species, "CYACAE")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$Plot, "POL")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$FemaleID, "K7V5321")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$MaleID, "K7V5155")
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$LayDate_observed, as.Date("2019-04-09"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$ClutchSize_observed, 11)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$HatchDate_observed, as.Date("2019-05-03"))
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$BroodSize_observed, 10)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$NumberFledged_observed, 9)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$AvgEggMass, 4.9/4)
  expect_equal(subset(WRS_data,
                      BreedingSeason == "2019"
                      & PopID == "WRS"
                      & LocationID == "POL57")$NumberEggs, 4)


})

test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("WRS"))

  ## K7V3106 caught on 2016-05-19 (and two other occasions)
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$Species, "PARMAJ")
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$Sex_observed, "M")
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$BreedingSeason, 2016)
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$CaptureTime, "11:15")
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$CapturePlot, "KPN")
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$ReleasePopID, "WRS")
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$ReleasePlot, "KPN")
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$Mass, 18.0)
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$Tarsus, 20.5)
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$OriginalTarsusMethod, NA_character_)
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$WingLength, 75.0)
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$Age_observed, 6) # May change depending on age code
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$Age_calculated, 4)
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$ChickAge, NA_integer_)
  expect_equal(subset(WRS_data, IndvID == "K7V3106"
                      & CaptureDate == "2016-05-19")$ExperimentID, "OTHER") # May change depending on experiment labels


  ## K4Z2221 caught on 2016-05-19 as a chick
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$Species, "CYACAE")
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$Sex_observed, NA_character_)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$BreedingSeason, 2019)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$CaptureTime, NA_character_)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$CapturePlot, "CMZ")
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$ReleasePopID, "WRS")
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$ReleasePlot, "CMZ")
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$Mass, 8.5)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$Tarsus, 15.0)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$OriginalTarsusMethod, NA_character_)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$WingLength, NA_real_)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$Age_observed, 1) # May change depending on age code
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$Age_calculated, 1)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$ChickAge, 15)
  expect_equal(subset(WRS_data, IndvID == "K4Z2221"
                      & CaptureDate == "2019-05-19")$ExperimentID, NA_character_) # May change depending on experiment labels


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


test_that("Location_data returns an expected outcome...", {

  #Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("WRS"))

  ## Nestbox BIB42
  expect_equal(subset(WRS_data, LocationID == "BIB42")$NestboxID, "BIB42")
  expect_equal(subset(WRS_data, LocationID == "BIB42")$LocationType, "NB")
  expect_equal(subset(WRS_data, LocationID == "BIB42")$PopID, "WRS")
  expect_equal(subset(WRS_data, LocationID == "BIB42")$Latitude, 52.29715)
  expect_equal(subset(WRS_data, LocationID == "BIB42")$Longitude, 20.951402)
  expect_equal(subset(WRS_data, LocationID == "BIB42")$StartSeason, 2018)
  expect_equal(subset(WRS_data, LocationID == "BIB42")$EndSeason, NA_integer_)
  expect_equal(subset(WRS_data, LocationID == "BIB42")$HabitatType, "urban") # May change depending on input from data owner

  ## Nestbox CMZ22
  expect_equal(subset(WRS_data, LocationID == "CMZ22")$NestboxID, "CMZ22")
  expect_equal(subset(WRS_data, LocationID == "CMZ22")$LocationType, "NB")
  expect_equal(subset(WRS_data, LocationID == "CMZ22")$PopID, "WRS")
  expect_equal(subset(WRS_data, LocationID == "CMZ22")$Latitude, 52.2471)
  expect_equal(subset(WRS_data, LocationID == "CMZ22")$Longitude, 20.97461)
  expect_equal(subset(WRS_data, LocationID == "CMZ22")$StartSeason, 2016)
  expect_equal(subset(WRS_data, LocationID == "CMZ22")$EndSeason, NA_integer_)
  expect_equal(subset(WRS_data, LocationID == "CMZ22")$HabitatType, "urban") # May change depending on input from data owner


})

