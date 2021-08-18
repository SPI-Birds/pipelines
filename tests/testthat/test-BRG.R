testthat::skip_if(!exists("data_path"))

data_path <- "/Users/tyson/Documents/academia/institutions/NIOO/SPI-Birds/my_pipelines/MAR/data/"

pipeline_output <- format_MAR(db = paste0(data_path, "/MAR_Mariola_Spain"))


PopID <- c("MAR")

test_that("format_MAR outputs all tables...", {

  expect_true(all(PopID %in% pipeline_output$Brood_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(PopID %in% pipeline_output$Individual_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Location_data$PopID))

})


test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only MAR data
  MAR_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("MAR"))

  ## General brood data
  loc4_2002 <- subset(MAR_data,
                       BreedingSeason == 2002 &
                         PopID == "MAR" &
                         Plot == "Mariola" &
                         LocationID == "4")


  expect_equal(!is.na(loc4_2002$BroodID), TRUE)
  expect_equal(loc4_2002$BreedingSeason, 2002)
  expect_equal(loc4_2002$Plot, "Mariola")
  expect_equal(loc4_2002$Species, "PARMAJ")
  expect_equal(loc4_2002$FemaleID, NA_character_)
  expect_equal(loc4_2002$MaleID, NA_character_)
  expect_equal(loc4_2002$LayDate_observed, as.Date("2002-04-25"))
  expect_equal(loc4_2002$ClutchSize_observed, NA_integer_)
  expect_equal(loc4_2002$HatchDate_observed, as.Date("2002-05-18"))
  expect_equal(loc4_2002$BroodSize_observed, NA_integer_)
  expect_equal(loc4_2002$NumberFledged_observed, NA_integer_)
  expect_equal(loc4_2002$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc4_2002$AvgChickMass, 17.9)
  expect_equal(loc4_2002$NumberChicksMass, 5)
  expect_equal(loc4_2002$AvgTarsus, 19.55)
  expect_equal(loc4_2002$NumberChicksTarsus, 5)
  expect_equal(loc4_2002$AvgEggMass, NA_real_)
  expect_equal(loc4_2002$NumberEggs, NA_integer_)


  ## Brood with chicks measured, two clutches, parents known
  loc99_2007 <- subset(MAR_data,
                       BreedingSeason == 2007
                       & PopID == "MAR"
                       & LocationID == "99"
                       & is.na(ClutchType_observed))

  expect_equal(!is.na(loc99_2007$BroodID), TRUE)
  expect_equal(loc99_2007$BreedingSeason, 2007)
  expect_equal(loc4_2002$Plot, "Mariola")
  expect_equal(loc99_2007$Species, "LOPCRI")
  expect_equal(loc99_2007$FemaleID, "EN9310")
  expect_equal(loc99_2007$MaleID, "EN9311")
  expect_equal(loc99_2007$LayDate_observed, as.Date("2007-05-02"))
  expect_equal(loc99_2007$ClutchSize_observed, 5)
  expect_equal(loc99_2007$HatchDate_observed, as.Date("2007-05-20"))
  expect_equal(loc99_2007$BroodSize_observed, 5)
  # expect_equal(loc99_2007$NumberFledged_observed, 5)
  expect_equal(loc99_2007$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc99_2007$AvgChickMass, 12.1)
  expect_equal(loc99_2007$NumberChicksMass, 5)
  expect_equal(loc99_2007$AvgTarsus, 18.11)
  expect_equal(loc99_2007$NumberChicksTarsus, 5)
  expect_equal(loc99_2007$AvgEggMass, NA_real_)
  expect_equal(loc99_2007$NumberEggs, NA_integer_)

  ## Nest with three clutches
  loc65_2_2013 <- subset(MAR_data,
                       BreedingSeason == 2013
                       & PopID == "MAR"
                       & LocationID == "65"
                       & ClutchType_observed == "second")

  expect_equal(!is.na(loc65_2_2013$BroodID), TRUE)
  expect_equal(loc65_2_2013$BreedingSeason, 2013)
  expect_equal(loc4_2002$Plot, "Mariola")
  expect_equal(loc65_2_2013$Species, "PARMAJ")
  expect_equal(loc65_2_2013$FemaleID, NA_character_)
  expect_equal(loc65_2_2013$MaleID, NA_character_)
  expect_equal(loc65_2_2013$LayDate_observed, as.Date("2013-06-09"))
  expect_equal(loc65_2_2013$ClutchSize_observed, 3)
  expect_equal(loc65_2_2013$HatchDate_observed, as.Date("2013-06-25"))
  expect_equal(loc65_2_2013$BroodSize_observed, 0)
  expect_equal(loc65_2_2013$NumberFledged_observed, 0)
  expect_equal(loc65_2_2013$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc65_2_2013$AvgChickMass, NA_real_)
  expect_equal(loc65_2_2013$NumberChicksMass, NA_integer_)
  expect_equal(loc65_2_2013$AvgTarsus, NA_real_)
  expect_equal(loc65_2_2013$NumberChicksTarsus, NA_integer_)
  expect_equal(loc65_2_2013$AvgEggMass, NA_real_)
  expect_equal(loc65_2_2013$NumberEggs, NA_integer_)


})

test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only MAR data
  MAR_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("MAR"))

  ## 2A324111, banded as chick, recruited
  cap_2A324111 <- subset(MAR_data,
                        IndvID == "2A324111"
                        & CaptureDate == "2013-05-15")

  expect_equal(cap_2A324111$Species, "PARMAJ")
  expect_equal(cap_2A324111$Sex_observed, "F")
  expect_equal(cap_2A324111$BreedingSeason, 2013)
  expect_equal(cap_2A324111$CaptureTime, NA_character_)
  expect_equal(cap_2A324111$LocationID, "100")
  expect_equal(cap_2A324111$CapturePlot, "Mariola")
  expect_equal(cap_2A324111$ReleasePopID, "MAR")
  expect_equal(cap_2A324111$ReleasePlot, "Mariola")
  expect_equal(cap_2A324111$Mass, 17.4)
  expect_equal(cap_2A324111$Tarsus, 19.74)
  expect_equal(cap_2A324111$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_2A324111$WingLength, 70.5)
  expect_equal(cap_2A324111$Age_observed, 6)
  expect_equal(cap_2A324111$Age_calculated, 8)
  expect_equal(cap_2A324111$ChickAge, NA_integer_)
  expect_equal(cap_2A324111$ExperimentID, NA_character_)


  ## 2A003311 unknown sex
  cap_2A003311 <- subset(MAR_data,
                        IndvID == "2A003311"
                        & CaptureDate == "2007-06-22")

  expect_equal(cap_2A003311$Species, "PARMAJ")
  expect_equal(cap_2A003311$Sex_observed, NA_character_)
  expect_equal(cap_2A003311$BreedingSeason, 2007)
  expect_equal(cap_2A003311$CaptureTime, NA_character_)
  expect_equal(cap_2A003311$LocationID, "97")
  expect_equal(cap_2A003311$CapturePlot, "Mariola")
  expect_equal(cap_2A003311$ReleasePopID, "MAR")
  expect_equal(cap_2A003311$ReleasePlot, "Mariola")
  expect_equal(cap_2A003311$Mass, 16.6)
  expect_equal(cap_2A003311$Tarsus, NA_real_)
  expect_equal(cap_2A003311$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_2A003311$WingLength, NA_real_)
  expect_equal(cap_2A003311$Age_observed, 6)
  expect_equal(cap_2A003311$Age_calculated, 9)
  expect_equal(cap_2A003311$ChickAge, NA_integer_)
  expect_equal(cap_2A003311$ExperimentID, NA_character_)


  ## L639119 chick without locationID
  cap_L639119 <- subset(MAR_data,
                        IndvID == "L639119"
                        & CaptureDate == "2002-05-24")

  expect_equal(cap_L639119$Species, "PERATE")
  expect_equal(cap_L639119$Sex_observed, NA_character_)
  expect_equal(cap_L639119$BreedingSeason, 2002)
  expect_equal(cap_L639119$CaptureTime, NA_character_)
  expect_equal(cap_L639119$LocationID, NA_character_)
  expect_equal(cap_L639119$CapturePlot, "Font Roja")
  expect_equal(cap_L639119$ReleasePopID, "MAR")
  expect_equal(cap_L639119$ReleasePlot, "Font Roja")
  expect_equal(cap_L639119$Mass, 7.4)
  expect_equal(cap_L639119$Tarsus, NA_real_)
  expect_equal(cap_L639119$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_L639119$WingLength, NA_real_)
  expect_equal(cap_L639119$Age_observed, 1)
  expect_equal(cap_L639119$Age_calculated, 1)
  expect_equal(cap_L639119$ChickAge, NA_integer_)
  expect_equal(cap_L639119$ExperimentID, NA_character_)


})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only MAR data
  MAR_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("MAR"))

  #Individual 2A003311, banded as chick, recruited
  ind_2A003311 <- subset(MAR_data, IndvID == "2A003311")
  expect_equal(ind_2A003311$Species, "PARMAJ")
  expect_equal(ind_2A003311$PopID, "MAR")
  expect_equal(!is.na(ind_2A003311$BroodIDLaid), TRUE)
  expect_equal(!is.na(ind_2A003311$BroodIDFledged), TRUE)
  expect_equal(ind_2A003311$RingSeason, 2004)
  expect_equal(ind_2A003311$RingAge, "chick")
  expect_equal(ind_2A003311$Sex_calculated, NA_character_)
  expect_equal(ind_2A003311$Sex_genetic, NA_character_)

  #Individual 3N96383, chick with band entered incorrectly, should still be in the data with a BroodID
  ind_3N96383 <- subset(MAR_data, IndvID == "3N96383")
  expect_equal(ind_3N96383$Species, "PERATE")
  expect_equal(ind_3N96383$PopID, "MAR")
  expect_equal(!is.na(ind_3N96383$BroodIDLaid), TRUE)
  expect_equal(!is.na(ind_3N96383$BroodIDFledged), TRUE)
  expect_equal(ind_3N96383$RingSeason, 2013)
  expect_equal(ind_3N96383$RingAge, "chick")
  expect_equal(ind_3N96383$Sex_calculated, NA_character_)
  expect_equal(ind_3N96383$Sex_genetic, NA_character_)


  #Individual N792022, adult
  ind_N792022 <- subset(MAR_data, IndvID == "N792022")
  expect_equal(ind_N792022$Species, "CYACAE")
  expect_equal(ind_N792022$PopID, "MAR")
  expect_equal(is.na(ind_N792022$BroodIDLaid), TRUE)
  expect_equal(is.na(ind_N792022$BroodIDFledged), TRUE)
  expect_equal(ind_N792022$RingSeason, 2012)
  expect_equal(ind_N792022$RingAge, "adult")
  expect_equal(ind_N792022$Sex_calculated, NA_character_)
  expect_equal(ind_N792022$Sex_genetic, NA_character_)


})


test_that("Location_data returns an expected outcome...", {

  #Take a subset of only MAR data
  MAR_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("MAR"))

  ## Nestbox FR1 (Font Roja site)
  loc_FR1 <- subset(MAR_data, LocationID == "FR1")
  expect_equal(loc_FR1$NestboxID, "FR1")
  expect_equal(loc_FR1$LocationType, "NB")
  expect_equal(loc_FR1$PopID, "MAR")
  expect_equal(loc_FR1$Latitude, 38.66063, tolerance=1e-1)
  expect_equal(loc_FR1$Longitude, -0.5176163)
  expect_equal(loc_FR1$StartSeason, 2005)
  expect_equal(loc_FR1$EndSeason, NA_integer_)
  expect_equal(loc_FR1$HabitatType, "mixed")

  ## Nestbox 64 (Mariola site)
  loc_64 <- subset(MAR_data, LocationID == "64")
  expect_equal(loc_64$NestboxID, "64")
  expect_equal(loc_64$LocationType, "NB")
  expect_equal(loc_64$PopID, "MAR")
  expect_equal(loc_64$Latitude, 38.73705, tolerance=1e-1)
  expect_equal(loc_64$Longitude, -0.5633904)
  expect_equal(loc_64$StartSeason, 2004)
  expect_equal(loc_64$EndSeason, NA_integer_)
  expect_equal(loc_64$HabitatType, "mixed")


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
  test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[:alnum:]{6,9}$")

  ## MaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[:alnum:]{6,9}$")

  ## IndvID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[:alnum:]{6,9}$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[:alnum:]{6,9}$")

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
