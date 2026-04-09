pipeline_output <- format_WRS(db = paste0(data_path, "/WRS_Warsaw_Poland"))

testthat::test_that("WRS outputs all files...", {
  testthat::expect_true(all(c("WRS") %in% pipeline_output$Brood_data$PopID))
  testthat::expect_true(all(c("WRS") %in% pipeline_output$Capture_data$CapturePopID))
  testthat::expect_true(all(c("WRS") %in% pipeline_output$Individual_data$PopID))
  testthat::expect_true(all(c("WRS") %in% pipeline_output$Location_data$PopID))
})

testthat::test_that("Brood_data returns an expected outcome...", {
  ## Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("WRS"))

  ## General brood data - Location POL33
  testthat::expect_equal(!is.na(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$BroodID), TRUE)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$BreedingSeason, 2016)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$Species, "PARMAJ")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$Plot, "POL")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$FemaleID, NA_character_)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$MaleID, NA_character_)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$LayDate_observed, as.Date("2016-04-24"))
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$ClutchSize_observed, 5)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$HatchDate_observed, as.Date("2016-05-14"))
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$BroodSize_observed, 5)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$NumberFledged_observed, 0)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$FledgeDate_observed, lubridate::NA_Date_)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$AvgEggMass, 7.1)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "POL33"
  )$NumberEggs, 4)


  ## General brood data - Location KPN46 in 2016 - first brood of two
  testthat::expect_equal(!is.na(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$BroodID), TRUE)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$BreedingSeason, 2016)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$Species, "PARMAJ")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$Plot, "KPN")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$FemaleID, "K7V3107")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$MaleID, "K7V3106")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$ClutchSize_observed, 8)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$HatchDate_observed, as.Date("2016-05-10"))
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$BroodSize_observed, 8)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$NumberFledged_observed, 4)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$FledgeDate_observed, lubridate::NA_Date_)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$AvgEggMass, 6.7)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2016" &
      PopID == "WRS" &
      LocationID == "KPN46" &
      LayDate_observed == "2016-04-19"
  )$NumberEggs, 4)


  ## General brood data - Location POL57
  testthat::expect_equal(!is.na(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$BroodID), TRUE)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$BreedingSeason, 2019)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$Species, "CYACAE")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$Plot, "POL")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$FemaleID, "K7V5321")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$MaleID, "K7V5155")
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$LayDate_observed, as.Date("2019-04-09"))
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$ClutchSize_observed, 11)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$HatchDate_observed, as.Date("2019-05-03"))
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$BroodSize_observed, 10)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$NumberFledged_observed, 9)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$FledgeDate_observed, lubridate::NA_Date_)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$AvgEggMass, 4.9)
  testthat::expect_equal(subset(
    WRS_data,
    BreedingSeason == "2019" &
      PopID == "WRS" &
      LocationID == "POL57"
  )$NumberEggs, 4)
})

testthat::test_that("Capture_data returns an expected outcome...", {
  # Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("WRS"))

  ## K7V3106 caught on 2016-05-19 (and two other occasions)
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$Species, "PARMAJ")
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$Sex_observed, "M")
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$BreedingSeason, 2016)
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$CaptureTime, "11:15")
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$CapturePlot, "KPN")
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$ReleasePopID, "WRS")
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$ReleasePlot, "KPN")
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$Mass, 18.0)
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$Tarsus, 20.5)
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$OriginalTarsusMethod, NA_character_)
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$WingLength, 75.0)
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$Age_observed, 6) # May change depending on age code
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$Age_calculated, 4)
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$ChickAge, NA_integer_)
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3106" &
                                  CaptureDate == "2016-05-19")$ExperimentID, NA_character_)


  ## K4Z2221 caught on 2016-05-19 as a chick
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$Species, "CYACAE")
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$Sex_observed, NA_character_)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$BreedingSeason, 2019)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$CaptureTime, NA_character_)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$CapturePlot, "CMZ")
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$ReleasePopID, "WRS")
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$ReleasePlot, "CMZ")
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$Mass, 8.5)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$Tarsus, 15.0)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$OriginalTarsusMethod, NA_character_)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$WingLength, NA_real_)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$Age_observed, 1) # May change depending on age code
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$Age_calculated, 1)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$ChickAge, 15)
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2221" &
                                  CaptureDate == "2019-05-19")$ExperimentID, NA_character_) # May change depending on experiment labels
})


testthat::test_that("Individual data returns an expected outcome...", {
  # Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("WRS"))

  # Individual K7V3532
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3532")$Species, "PARMAJ") # PARMAJ
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3532")$PopID, "WRS") # WRS
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3532")$BroodIDLaid, NA_character_) # NA
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3532")$BroodIDFledged, NA_character_) # NA
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3532")$RingSeason, 2016) # 2016
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3532")$RingAge, "adult") # adult
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3532")$Sex_calculated, "F") # F
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V3532")$Sex_genetic, NA_character_) # NA

  # Individual K7V3532
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4627")$Species, "CYACAE") # CYACAE
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4627")$PopID, "WRS") # WRS
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4627")$BroodIDLaid, NA_character_) # NA
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4627")$BroodIDFledged, NA_character_) # NA
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4627")$RingSeason, 2017) # 2017
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4627")$RingAge, "adult") # adult
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4627")$Sex_calculated, "M") # M
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4627")$Sex_genetic, NA_character_) # NA

  # Individual K7V4826
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4826")$Species, "PARMAJ") # PARMAJ
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4826")$PopID, "WRS") # WRS
  testthat::expect_equal(!is.na(subset(WRS_data, IndvID == "K7V4826")$BroodIDLaid), TRUE) # TRUE
  testthat::expect_equal(!is.na(subset(WRS_data, IndvID == "K7V4826")$BroodIDFledged), TRUE) # TRUE
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4826")$RingSeason, 2017) # 2017
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4826")$RingAge, "chick") # chick
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4826")$Sex_calculated, NA_character_) # NA
  testthat::expect_equal(subset(WRS_data, IndvID == "K7V4826")$Sex_genetic, NA_character_) # NA

  # Individual K4Z2244
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2244")$Species, "CYACAE") # CYACAE
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2244")$PopID, "WRS") # WRS
  testthat::expect_equal(!is.na(subset(WRS_data, IndvID == "K4Z2244")$BroodIDLaid), TRUE) # TRUE
  testthat::expect_equal(!is.na(subset(WRS_data, IndvID == "K4Z2244")$BroodIDFledged), TRUE) # TRUE
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2244")$RingSeason, 2019) # 2019
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2244")$RingAge, "chick") # chick
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2244")$Sex_calculated, "M") # M
  testthat::expect_equal(subset(WRS_data, IndvID == "K4Z2244")$Sex_genetic, NA_character_) # NA
})


testthat::test_that("Location_data returns an expected outcome...", {
  # Take a subset of only WRS data
  WRS_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("WRS"))

  ## Nestbox BIB42
  testthat::expect_equal(subset(WRS_data, LocationID == "BIB42")$NestboxID, "BIB42")
  testthat::expect_equal(subset(WRS_data, LocationID == "BIB42")$LocationType, "NB")
  testthat::expect_equal(subset(WRS_data, LocationID == "BIB42")$PopID, "WRS")
  testthat::expect_equal(subset(WRS_data, LocationID == "BIB42")$Latitude, 52.29715)
  testthat::expect_equal(subset(WRS_data, LocationID == "BIB42")$Longitude, 20.9514)
  testthat::expect_equal(subset(WRS_data, LocationID == "BIB42")$StartSeason, 2019)
  testthat::expect_equal(subset(WRS_data, LocationID == "BIB42")$EndSeason, 2025)
  testthat::expect_equal(subset(WRS_data, LocationID == "BIB42")$HabitatType, "urban") # May change depending on input from data owner

  ## Nestbox CMZ22
  testthat::expect_equal(subset(WRS_data, LocationID == "CMZ22")$NestboxID, "CMZ22")
  testthat::expect_equal(subset(WRS_data, LocationID == "CMZ22")$LocationType, "NB")
  testthat::expect_equal(subset(WRS_data, LocationID == "CMZ22")$PopID, "WRS")
  testthat::expect_equal(subset(WRS_data, LocationID == "CMZ22")$Latitude, 52.2471)
  testthat::expect_equal(subset(WRS_data, LocationID == "CMZ22")$Longitude, 20.97461)
  testthat::expect_equal(subset(WRS_data, LocationID == "CMZ22")$StartSeason, 2025)
  testthat::expect_equal(subset(WRS_data, LocationID == "CMZ22")$EndSeason, 2025)
  testthat::expect_equal(subset(WRS_data, LocationID == "CMZ22")$HabitatType, "urban") # May change depending on input from data owner
})


## General tests (for pipelines formatted to standard protocol version 1.1.0)

testthat::test_that("Expected columns are present", {

  ## Will fail if not all the expected columns are present

  ## Brood data: Test that all columns are present
  test_col_present(pipeline_output, "Brood", protocol_version = "1.1.0")

  ## Capture data: Test that all columns are present
  test_col_present(pipeline_output, "Capture", protocol_version = "1.1.0")

  ## Individual data: Test that all columns are present
  test_col_present(pipeline_output, "Individual", protocol_version = "1.1.0")

  ## Location data: Test that all columns are present
  test_col_present(pipeline_output, "Location", protocol_version = "1.1.0")

})

testthat::test_that("Column classes are as expected", {

  ## Will fail if columns that are shared by the output and the templates have different classes.

  ## Brood data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Brood", protocol_version = "1.1.0")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture", protocol_version = "1.1.0")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual", protocol_version = "1.1.0")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location", protocol_version = "1.1.0")

})



testthat::test_that("Key columns only contain unique values", {

  ## BroodID has only unique values
  test_unique_values(pipeline_output, "BroodID")

  ## CaptureID has only unique values
  test_unique_values(pipeline_output, "CaptureID")

  ## PopID-IndvID has only unique values
  test_unique_values(pipeline_output, "IndvID")

})


testthat::test_that("Key columns in each table do not have NAs", {

  ## Brood
  test_NA_columns(pipeline_output, "Brood")

  ## Capture
  test_NA_columns(pipeline_output, "Capture")

  ## Individual
  test_NA_columns(pipeline_output, "Individual")

  ## Location
  test_NA_columns(pipeline_output, "Location")

})


testthat::test_that("Categorical columns do not have unexpected values", {

  ## Brood
  test_category_columns(pipeline_output, "Brood")

  ## Capture
  test_category_columns(pipeline_output, "Capture")

  ## Individual
  test_category_columns(pipeline_output, "Individual")

  ## Location
  test_category_columns(pipeline_output, "Location")

})
