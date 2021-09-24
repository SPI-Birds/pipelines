testthat::skip_if(!exists("data_path"))

pipeline_output <- format_SAG(db = paste0(data_path, "/SAG_Sagunto_Spain"))

PopID <- c("SAG")

test_that("format_SAG outputs all tables...", {

  expect_true(all(PopID %in% pipeline_output$Brood_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(PopID %in% pipeline_output$Individual_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Location_data$PopID))

})


test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only SAG data
  SAG_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("SAG"))


  ## General brood data
  loc10_2017 <- subset(SAG_data,
                       BreedingSeason == 2017 &
                         PopID == "SAG" &
                         LocationID == "10")

  expect_equal(!is.na(loc10_2017$BroodID), TRUE)
  expect_equal(loc10_2017$BreedingSeason, 2017)
  expect_equal(loc10_2017$Plot, "Mørkevatnet")
  expect_equal(loc10_2017$Species, "PARMAJ")
  expect_equal(loc10_2017$FemaleID, NA_character_)
  expect_equal(loc10_2017$MaleID, NA_character_)
  expect_equal(loc10_2017$LayDate_observed, as.Date("2017-05-07"))
  expect_equal(loc10_2017$ClutchSize_observed, 7)
  expect_equal(loc10_2017$HatchDate_observed, as.Date("2017-05-24"))
  expect_equal(loc10_2017$BroodSize_observed, 6)
  expect_equal(loc10_2017$NumberFledged_observed, 5)
  expect_equal(loc10_2017$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc10_2017$AvgEggMass, NA_real_)
  expect_equal(loc10_2017$NumberEggs, NA_integer_)


  ## Brood with chicks measured, but only a subset measured when between 14-16 days
  loc10_2019 <- subset(SAG_data,
                       BreedingSeason == 2019
                       & PopID == "SAG"
                       & LocationID == "10")

  expect_equal(!is.na(loc10_2019$BroodID), TRUE)
  expect_equal(loc10_2019$BreedingSeason, 2019)
  expect_equal(loc10_2019$Species, "PARMAJ")
  expect_equal(loc10_2019$FemaleID, "EK06640")
  expect_equal(loc10_2019$MaleID, NA_character_)
  expect_equal(loc10_2019$LayDate_observed, as.Date("2019-04-19"))
  expect_equal(loc10_2019$ClutchSize_observed, 9)
  expect_equal(loc10_2019$HatchDate_observed, as.Date("2019-05-10"))
  expect_equal(loc10_2019$BroodSize_observed, 9)
  expect_equal(loc10_2019$NumberFledged_observed, 7)
  expect_equal(loc10_2019$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc10_2019$AvgChickMass, 16.25)
  expect_equal(loc10_2019$NumberChicksMass, 4)
  expect_equal(loc10_2019$AvgTarsus, NA_real_)
  expect_equal(loc10_2019$NumberChicksTarsus, NA_integer_)
  expect_equal(loc10_2019$AvgEggMass, NA_real_)
  expect_equal(loc10_2019$NumberEggs, NA_integer_)

  ## Brood with chicks measured, but only a subset measured when between 14-16 days and both parents captured
  loc35_2019 <- subset(SAG_data,
                       BreedingSeason == 2019
                       & PopID == "SAG"
                       & LocationID == "35")

  expect_equal(!is.na(loc35_2019$BroodID), TRUE)
  expect_equal(loc35_2019$BreedingSeason, 2019)
  expect_equal(loc35_2019$Plot, "Blondehuset")
  expect_equal(loc35_2019$Species, "CYACAE")
  expect_equal(loc35_2019$FemaleID, "HD86415")
  expect_equal(loc35_2019$MaleID, "HF46894")
  expect_equal(loc35_2019$LayDate_observed, as.Date("2019-04-21"))
  expect_equal(loc35_2019$ClutchSize_observed, 9)
  expect_equal(loc35_2019$HatchDate_observed, as.Date("2019-05-14"))
  expect_equal(loc35_2019$BroodSize_observed, 9)
  expect_equal(loc35_2019$NumberFledged_observed, 9)
  expect_equal(loc35_2019$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc35_2019$AvgChickMass, 10.65)
  expect_equal(loc35_2019$NumberChicksMass, 8)
  expect_equal(loc35_2019$AvgTarsus, NA_real_)
  expect_equal(loc35_2019$NumberChicksTarsus, NA_integer_)
  expect_equal(loc35_2019$AvgEggMass, NA_real_)
  expect_equal(loc35_2019$NumberEggs, NA_integer_)


})

test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only SAG data
  SAG_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("SAG"))

  ## EK06579, banded as chick, recruited
  cap_EK06579 <- subset(SAG_data,
                        IndvID == "EK06579"
                        & CaptureDate == "2018-05-29")

  expect_equal(cap_EK06579$Species, "PARMAJ")
  expect_equal(cap_EK06579$Sex_observed, NA_character_)
  expect_equal(cap_EK06579$BreedingSeason, 2018)
  expect_equal(cap_EK06579$CaptureTime, "10:00")
  expect_equal(cap_EK06579$LocationID, "33")
  expect_equal(cap_EK06579$CapturePlot, "Blondehuset")
  expect_equal(cap_EK06579$ReleasePopID, "SAG")
  expect_equal(cap_EK06579$ReleasePlot, "Blondehuset")
  expect_equal(cap_EK06579$Mass, 17.5)
  expect_equal(cap_EK06579$Tarsus, NA_real_)
  expect_equal(cap_EK06579$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_EK06579$WingLength, NA_real_)
  expect_equal(cap_EK06579$Age_observed, 1)
  expect_equal(cap_EK06579$Age_calculated, 1)
  expect_equal(cap_EK06579$ChickAge, 15)
  expect_equal(cap_EK06579$ExperimentID, NA_character_)


  ## HD86461 adult male
  cap_HD86461 <- subset(SAG_data,
                        IndvID == "HD86461"
                        & CaptureDate == "2019-06-02")

  expect_equal(cap_HD86461$Species, "CYACAE")
  expect_equal(cap_HD86461$Sex_observed, "M")
  expect_equal(cap_HD86461$BreedingSeason, 2019)
  expect_equal(cap_HD86461$CaptureTime, "09:27")
  expect_equal(cap_HD86461$LocationID, "37")
  expect_equal(cap_HD86461$CapturePlot, "Blondehuset")
  expect_equal(cap_HD86461$ReleasePopID, "SAG")
  expect_equal(cap_HD86461$ReleasePlot, "Blondehuset")
  expect_equal(cap_HD86461$Mass, 10.9)
  expect_equal(cap_HD86461$Tarsus, NA_real_)
  expect_equal(cap_HD86461$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_HD86461$WingLength, 67.5)
  expect_equal(cap_HD86461$Age_observed, 6)
  expect_equal(cap_HD86461$Age_calculated, 4)
  expect_equal(cap_HD86461$ChickAge, NA_integer_)
  expect_equal(cap_HD86461$ExperimentID, NA_character_)


  ## HD86485 chick with missing mass measurement
  cap_HD86485 <- subset(SAG_data,
                        IndvID == "HD86485"
                        & CaptureDate == "2019-06-09")

  expect_equal(cap_HD86485$Species, "CYACAE")
  expect_equal(cap_HD86485$Sex_observed, NA_character_)
  expect_equal(cap_HD86485$BreedingSeason, 2019)
  expect_equal(cap_HD86485$CaptureTime, "12:00")
  expect_equal(cap_HD86485$LocationID, "13")
  expect_equal(cap_HD86485$CapturePlot, "Mørkevatnet")
  expect_equal(cap_HD86485$ReleasePopID, "SAG")
  expect_equal(cap_HD86485$ReleasePlot, "Mørkevatnet")
  expect_equal(cap_HD86485$Mass, NA_real_)
  expect_equal(cap_HD86485$Tarsus, NA_real_)
  expect_equal(cap_HD86485$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_HD86485$WingLength, NA_real_)
  expect_equal(cap_HD86485$Age_observed, 1)
  expect_equal(cap_HD86485$Age_calculated, 1)
  expect_equal(cap_HD86485$ChickAge, 17)
  expect_equal(cap_HD86485$ExperimentID, NA_character_)


})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only SAG data
  SAG_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("SAG"))

  #Individual EK06602, banded as chick, recruited
  ind_EK06602 <- subset(SAG_data, IndvID == "EK06602")
  expect_equal(ind_EK06602$Species, "PARMAJ")
  expect_equal(ind_EK06602$PopID, "SAG")
  expect_equal(!is.na(ind_EK06602$BroodIDLaid), TRUE)
  expect_equal(!is.na(ind_EK06602$BroodIDFledged), TRUE)
  expect_equal(ind_EK06602$RingSeason, 2018)
  expect_equal(ind_EK06602$RingAge, "chick")
  expect_equal(ind_EK06602$Sex_calculated, "F")
  expect_equal(ind_EK06602$Sex_genetic, NA_character_)

  #Individual HH96002, adult captured twice
  ind_HH96002 <- subset(SAG_data, IndvID == "HH96002")
  expect_equal(ind_HH96002$Species, "CYACAE")
  expect_equal(ind_HH96002$PopID, "SAG")
  expect_equal(is.na(ind_HH96002$BroodIDLaid), TRUE)
  expect_equal(is.na(ind_HH96002$BroodIDFledged), TRUE)
  expect_equal(ind_HH96002$RingSeason, 2017)
  expect_equal(ind_HH96002$RingAge, "adult")
  expect_equal(ind_HH96002$Sex_calculated, "F")
  expect_equal(ind_HH96002$Sex_genetic, NA_character_)


  #Individual HD86406, adult captured twice
  ind_HD86406 <- subset(SAG_data, IndvID == "HD86406")
  expect_equal(ind_HD86406$Species, "PERATE")
  expect_equal(ind_HD86406$PopID, "SAG")
  expect_equal(is.na(ind_HD86406$BroodIDLaid), TRUE)
  expect_equal(is.na(ind_HD86406$BroodIDFledged), TRUE)
  expect_equal(ind_HD86406$RingSeason, 2019)
  expect_equal(ind_HD86406$RingAge, "adult")
  expect_equal(ind_HD86406$Sex_calculated, "M")
  expect_equal(ind_HD86406$Sex_genetic, NA_character_)


})


test_that("Location_data returns an expected outcome...", {

  #Take a subset of only SAG data
  SAG_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("SAG"))

  ## Nestbox 29
  loc_29 <- subset(SAG_data, LocationID == "29")
  expect_equal(loc_29$NestboxID, "29")
  expect_equal(loc_29$LocationType, "NB")
  expect_equal(loc_29$PopID, "SAG")
  expect_equal(loc_29$Latitude, 60.25)
  expect_equal(loc_29$Longitude, 5.26)
  expect_equal(loc_29$StartSeason, 2017)
  expect_equal(loc_29$EndSeason, NA_integer_)
  expect_equal(loc_29$HabitatType, "evergreen")

  ## Nestbox 29
  loc_2 <- subset(SAG_data, LocationID == "2")
  expect_equal(loc_2$NestboxID, "2")
  expect_equal(loc_2$LocationType, "NB")
  expect_equal(loc_2$PopID, "SAG")
  expect_equal(loc_2$Latitude, 60.25)
  expect_equal(loc_2$Longitude, 5.26)
  expect_equal(loc_2$StartSeason, 2018)
  expect_equal(loc_2$EndSeason, NA_integer_)
  expect_equal(loc_2$HabitatType, "deciduous")


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
  test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[[:alpha:][:digit:]]{2}[:digit:]{5}$")

  ## MaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[[:alpha:][:digit:]]{2}[:digit:]{5}$")

  ## IndvID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[[:alpha:][:digit:]]{2}[:digit:]{5}$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[[:alpha:][:digit:]]{2}[:digit:]{5}$")

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
