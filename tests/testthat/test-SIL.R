testthat::skip_if(!exists("data_path"))

pipeline_output <- format_SIL(db = paste0(data_path, "/SIL_Bergen_Norway"))

PopID <- c("SIL")

test_that("format_SIL outputs all tables...", {

  expect_true(all(PopID %in% pipeline_output$Brood_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(PopID %in% pipeline_output$Individual_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Location_data$PopID))

})


test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only SIL data
  SIL_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("SIL"))

  ## General brood data
  locA10_2002 <- subset(SIL_data,
                       BreedingSeason == 2002 &
                         PopID == "SIL" &
                         LocationID == "A10")

  expect_equal(!is.na(locA10_2002$BroodID), TRUE)
  expect_equal(locA10_2002$BreedingSeason, 2002)
  expect_equal(locA10_2002$Plot, NA_character_)
  expect_equal(locA10_2002$Species, "CYACAE")
  expect_equal(locA10_2002$FemaleID, "R187563")
  expect_equal(locA10_2002$MaleID, "R187565")
  expect_equal(locA10_2002$LayDate_observed, as.Date("2002-04-10"))
  expect_equal(locA10_2002$ClutchSize_observed, 9)
  expect_equal(locA10_2002$HatchDate_observed, as.Date("2002-05-03"))
  expect_equal(locA10_2002$BroodSize_observed, 9)
  expect_equal(locA10_2002$NumberFledged_observed, NA_integer_)
  expect_equal(locA10_2002$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(locA10_2002$AvgEggMass, NA_real_)
  expect_equal(locA10_2002$NumberEggs, NA_integer_)


  ## General brood data
  locGA02_2017 <- subset(SIL_data,
                       BreedingSeason == 2017
                       & PopID == "SIL"
                       & LocationID == "GA02")

  expect_equal(!is.na(locGA02_2017$BroodID), TRUE)
  expect_equal(locGA02_2017$BreedingSeason, 2017)
  expect_equal(locGA02_2017$Species, "CYACAE")
  expect_equal(locGA02_2017$FemaleID, "S271354")
  expect_equal(locGA02_2017$MaleID, "S271693")
  expect_equal(locGA02_2017$LayDate_observed, as.Date("2017-04-10"))
  expect_equal(locGA02_2017$ClutchSize_observed, 11)
  expect_equal(locGA02_2017$HatchDate_observed, as.Date("2017-05-02"))
  expect_equal(locGA02_2017$BroodSize_observed, 8)
  expect_equal(locGA02_2017$NumberFledged_observed, 3)
  expect_equal(locGA02_2017$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(locGA02_2017$AvgChickMass, NA_real_)
  expect_equal(locGA02_2017$NumberChicksMass, NA_integer_)
  expect_equal(locGA02_2017$AvgTarsus, NA_real_)
  expect_equal(locGA02_2017$NumberChicksTarsus, NA_integer_)
  expect_equal(locGA02_2017$AvgEggMass, NA_real_)
  expect_equal(locGA02_2017$NumberEggs, NA_integer_)


})

test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only SIL data
  SIL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("SIL"))

  ## R187479, female, no capture date
  cap_R187479 <- subset(SIL_data,
                        IndvID == "R187479"
                        & CaptureDate == "2004-05-06")

  expect_equal(cap_R187479$Species, "CYACAE")
  expect_equal(cap_R187479$Sex_observed, "F")
  expect_equal(cap_R187479$BreedingSeason, 2004)
  expect_equal(cap_R187479$CaptureTime, NA_character_)
  expect_equal(cap_R187479$LocationID, "C19")
  expect_equal(cap_R187479$CapturePlot, NA_character_)
  expect_equal(cap_R187479$ReleasePopID, "SIL")
  expect_equal(cap_R187479$ReleasePlot, NA_character_)
  expect_equal(cap_R187479$Mass, 10.0)
  expect_equal(cap_R187479$Tarsus, 16.7)
  expect_equal(cap_R187479$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_R187479$WingLength, NA_real_)
  expect_equal(cap_R187479$Age_observed, 6)
  expect_equal(cap_R187479$Age_calculated, 4)
  expect_equal(cap_R187479$ChickAge, NA_integer_)
  expect_equal(cap_R187479$ExperimentID, NA_character_)


  ## X762970, adult male
  cap_X762970 <- subset(SIL_data,
                        IndvID == "X762970"
                        & CaptureDate == "2012-05-06")

  expect_equal(cap_X762970$Species, "CYACAE")
  expect_equal(cap_X762970$Sex_observed, "M")
  expect_equal(cap_X762970$BreedingSeason, 2012)
  expect_equal(cap_X762970$CaptureTime, NA_character_)
  expect_equal(cap_X762970$LocationID, "F09")
  expect_equal(cap_X762970$CapturePlot, NA_character_)
  expect_equal(cap_X762970$ReleasePopID, "SIL")
  expect_equal(cap_X762970$ReleasePlot, NA_character_)
  expect_equal(cap_X762970$Mass, 10.6)
  expect_equal(cap_X762970$Tarsus, 16.56)
  expect_equal(cap_X762970$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_X762970$WingLength, 65)
  expect_equal(cap_X762970$Age_observed, 6)
  expect_equal(cap_X762970$Age_calculated, 6)
  expect_equal(cap_X762970$ChickAge, NA_integer_)
  expect_equal(cap_X762970$ExperimentID, NA_character_)


  ## Z047886, adult female with more measurements
  cap_Z047886 <- subset(SIL_data,
                        IndvID == "Z047886"
                        & CaptureDate == "2016-05-13")

  expect_equal(cap_Z047886$Species, "CYACAE")
  expect_equal(cap_Z047886$Sex_observed, "F")
  expect_equal(cap_Z047886$BreedingSeason, 2016)
  expect_equal(cap_Z047886$CaptureTime, "06:51")
  expect_equal(cap_Z047886$LocationID, "A13")
  expect_equal(cap_Z047886$CapturePlot, NA_character_)
  expect_equal(cap_Z047886$ReleasePopID, "SIL")
  expect_equal(cap_Z047886$ReleasePlot, NA_character_)
  expect_equal(cap_Z047886$Mass, 11.57)
  expect_equal(cap_Z047886$Tarsus, 15.7)
  expect_equal(cap_Z047886$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_Z047886$WingLength, 60)
  expect_equal(cap_Z047886$Age_observed, 5)
  expect_equal(cap_Z047886$Age_calculated, 4)
  expect_equal(cap_Z047886$ChickAge, NA_integer_)
  expect_equal(cap_Z047886$ExperimentID, NA_character_)


})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only SIL data
  SIL_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("SIL"))

  #Individual R187518, captured 5 times
  ind_R187518 <- subset(SIL_data, IndvID == "R187518")
  expect_equal(ind_R187518$Species, "CYACAE")
  expect_equal(ind_R187518$PopID, "SIL")
  expect_equal(is.na(ind_R187518$BroodIDLaid), TRUE)
  expect_equal(is.na(ind_R187518$BroodIDFledged), TRUE)
  expect_equal(ind_R187518$RingSeason, 2002)
  expect_equal(ind_R187518$RingAge, "adult")
  expect_equal(ind_R187518$Sex_calculated, "F")
  expect_equal(ind_R187518$Sex_genetic, NA_character_)


  #Individual AFD9647
  ind_AFD9647 <- subset(SIL_data, IndvID == "AFD9647")
  expect_equal(ind_AFD9647$Species, "CYACAE")
  expect_equal(ind_AFD9647$PopID, "SIL")
  expect_equal(is.na(ind_AFD9647$BroodIDLaid), TRUE)
  expect_equal(is.na(ind_AFD9647$BroodIDFledged), TRUE)
  expect_equal(ind_AFD9647$RingSeason, 2018)
  expect_equal(ind_AFD9647$RingAge, "adult")
  expect_equal(ind_AFD9647$Sex_calculated, "M")
  expect_equal(ind_AFD9647$Sex_genetic, NA_character_)


  #Individual AND1968, adult captured twice
  ind_AND1968 <- subset(SIL_data, IndvID == "AND1968")
  expect_equal(ind_AND1968$Species, "CYACAE")
  expect_equal(ind_AND1968$PopID, "SIL")
  expect_equal(is.na(ind_AND1968$BroodIDLaid), TRUE)
  expect_equal(is.na(ind_AND1968$BroodIDFledged), TRUE)
  expect_equal(ind_AND1968$RingSeason, 2019)
  expect_equal(ind_AND1968$RingAge, "adult")
  expect_equal(ind_AND1968$Sex_calculated, "F")
  expect_equal(ind_AND1968$Sex_genetic, NA_character_)


})


test_that("Location_data returns an expected outcome...", {

  #Take a subset of only SIL data
  SIL_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("SIL"))

  ## Nestbox A06
  loc_A06 <- subset(SIL_data, LocationID == "A06")
  expect_equal(loc_A06$NestboxID, "A06")
  expect_equal(loc_A06$LocationType, "NB")
  expect_equal(loc_A06$PopID, "SIL")
  expect_equal(loc_A06$Latitude, 51.4119604)
  expect_equal(loc_A06$Longitude, -0.640716188)
  expect_equal(loc_A06$StartSeason, 2002)
  expect_equal(loc_A06$EndSeason, NA_integer_)
  expect_equal(loc_A06$HabitatType, "mixed")

  ## Nestbox H09
  loc_H09 <- subset(SIL_data, LocationID == "H09")
  expect_equal(loc_H09$NestboxID, "H09")
  expect_equal(loc_H09$LocationType, "NB")
  expect_equal(loc_H09$PopID, "SIL")
  expect_equal(loc_H09$Latitude, 51.407604)
  expect_equal(loc_H09$Longitude, -0.639846)
  expect_equal(loc_H09$StartSeason, 2005)
  expect_equal(loc_H09$EndSeason, NA_integer_)
  expect_equal(loc_H09$HabitatType, "mixed")


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

  ## Brood data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Brood")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location")

})


test_that("ID columns match the expected format for the pipeline", {

  ## FemaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[[:alpha:][:digit:]]{3}[:digit:]{4}$")

  ## MaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[[:alpha:][:digit:]]{3}[:digit:]{4}$")

  ## IndvID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[[:alpha:][:digit:]]{3}[:digit:]{4}$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[[:alpha:][:digit:]]{3}[:digit:]{4}$")

})


test_that("Key columns only contain unique values", {

  ## BroodID has only unique values
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

