testthat::skip_if(!exists("data_path"))

pipeline_output <- format_MAL(db = paste0(data_path, "/MAL_Malmo_Sweden"))

PopID <- c("MAL")

test_that("format_MAL outputs all tables...", {

  expect_true(all(PopID %in% pipeline_output$Brood_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(PopID %in% pipeline_output$Individual_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Location_data$PopID))

})


test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only MAL data
  MAL_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("MAL"))


  ## General brood data, old format - Plot S, Location 11
  plotS_loc11 <- subset(MAL_data,
                        BreedingSeason == "2015"
                        & PopID == "MAL"
                        & Plot == "S"
                        & LocationID == "11")

  expect_equal(!is.na(plotS_loc11$BroodID), TRUE)
  expect_equal(plotS_loc11$BreedingSeason, 2015)
  expect_equal(plotS_loc11$Species, "PARMAJ")
  expect_equal(plotS_loc11$FemaleID, "2KS92538")
  expect_equal(plotS_loc11$MaleID, "2KS91871")
  expect_equal(plotS_loc11$LayDate_observed, as.Date("2015-04-22"))
  expect_equal(plotS_loc11$ClutchSize_observed, 7)
  expect_equal(plotS_loc11$HatchDate_observed, as.Date("2015-05-13"))
  expect_equal(plotS_loc11$BroodSize_observed, NA_integer_)
  expect_equal(plotS_loc11$NumberFledged_observed, NA_integer_)
  expect_equal(plotS_loc11$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(plotS_loc11$AvgEggMass, NA_real_)
  expect_equal(plotS_loc11$NumberEggs, NA_integer_)


  ## General brood data, new format - Plot P, Location 88
  plotP_loc88 <- subset(MAL_data,
                        BreedingSeason == "2018"
                        & PopID == "MAL"
                        & Plot == "P"
                        & LocationID == "88")

  expect_equal(!is.na(plotP_loc88$BroodID), TRUE)
  expect_equal(plotP_loc88$BreedingSeason, 2018)
  expect_equal(plotP_loc88$Species, "CYACAE")
  expect_equal(plotP_loc88$FemaleID, "1GA01636")
  expect_equal(plotP_loc88$MaleID, "1GA01635")
  expect_equal(plotP_loc88$LayDate_observed, as.Date("2018-05-08"))
  expect_equal(plotP_loc88$ClutchSize_observed, 7)
  expect_equal(plotP_loc88$HatchDate_observed, as.Date("2018-05-26"))
  expect_equal(plotP_loc88$BroodSize_observed, 7)
  expect_equal(plotP_loc88$NumberFledged_observed, NA_integer_)
  expect_equal(plotP_loc88$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(plotP_loc88$AvgChickMass, 10.6)
  expect_equal(plotP_loc88$NumberChicksMass, 6)
  expect_equal(plotP_loc88$AvgTarsus, 19.2)
  expect_equal(plotP_loc88$NumberChicksTarsus, 6)
  expect_equal(plotP_loc88$AvgEggMass, NA_real_)
  expect_equal(plotP_loc88$NumberEggs, NA_integer_)

})

test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only MAL data
  MAL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("MAL"))

  ## 2KS92418, chick caught on two occasions
  cap_2KS92418 <- subset(MAL_data,
                         IndvID == "2KS92418"
                         & CaptureDate == "2015-06-12")

  expect_equal(cap_2KS92418$Species, "PARMAJ")
  expect_equal(cap_2KS92418$Sex_observed, NA_character_)
  expect_equal(cap_2KS92418$BreedingSeason, 2015)
  expect_equal(cap_2KS92418$CaptureTime, "10:10")
  expect_equal(cap_2KS92418$CapturePlot, "S")
  expect_equal(cap_2KS92418$ReleasePopID, "MAL")
  expect_equal(cap_2KS92418$ReleasePlot, "S")
  expect_equal(cap_2KS92418$Mass, 14.9)
  expect_equal(cap_2KS92418$Tarsus, 21.5)
  expect_equal(cap_2KS92418$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_2KS92418$WingLength, 48.0)
  expect_equal(cap_2KS92418$Age_observed, 1)
  expect_equal(cap_2KS92418$Age_calculated, 1)
  expect_equal(cap_2KS92418$ChickAge, NA_integer_)
  expect_equal(cap_2KS92418$ExperimentID, NA_character_)


  ## 1ET87101 adult female
  cap_1ET87101 <- subset(MAL_data,
                         IndvID == "1ET87101"
                         & CaptureDate == "2017-05-26")

  expect_equal(cap_1ET87101$Species, "CYACAE")
  expect_equal(cap_1ET87101$Sex_observed, "F")
  expect_equal(cap_1ET87101$BreedingSeason, 2017)
  expect_equal(cap_1ET87101$CaptureTime, "18:15")
  expect_equal(cap_1ET87101$LocationID, "128")
  expect_equal(cap_1ET87101$CapturePlot, "P")
  expect_equal(cap_1ET87101$ReleasePopID, "MAL")
  expect_equal(cap_1ET87101$ReleasePlot, "P")
  expect_equal(cap_1ET87101$Mass, 11.4)
  expect_equal(cap_1ET87101$Tarsus, 20.4)
  expect_equal(cap_1ET87101$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_1ET87101$WingLength, 64.5)
  expect_equal(cap_1ET87101$Age_observed, 5)
  expect_equal(cap_1ET87101$Age_calculated, 4)
  expect_equal(cap_1ET87101$ChickAge, NA_integer_)
  expect_equal(cap_1ET87101$ExperimentID, NA_character_)


  ## 1ET87101 adult female, captured multiple times
  cap_2KS91300 <- subset(MAL_data,
                         IndvID == "2KS91300"
                         & CaptureDate == "2019-06-10")

  expect_equal(cap_2KS91300$Species, "PARMAJ")
  expect_equal(cap_2KS91300$Sex_observed, "F")
  expect_equal(cap_2KS91300$BreedingSeason, 2019)
  expect_equal(cap_2KS91300$CaptureTime, "12:00")
  expect_equal(cap_2KS91300$LocationID, "117")
  expect_equal(cap_2KS91300$CapturePlot, "S")
  expect_equal(cap_2KS91300$ReleasePopID, "MAL")
  expect_equal(cap_2KS91300$ReleasePlot, "S")
  expect_equal(cap_2KS91300$Mass, 18.10)
  expect_equal(cap_2KS91300$Tarsus, 23.3)
  expect_equal(cap_2KS91300$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_2KS91300$WingLength, 79.0)
  expect_equal(cap_2KS91300$Age_observed, 5)
  expect_equal(cap_2KS91300$Age_calculated, 16)
  expect_equal(cap_2KS91300$ChickAge, NA_integer_)
  expect_equal(cap_2KS91300$ExperimentID, NA_character_)


})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only MAL data
  MAL_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("MAL"))

  #Individual 2KV75337
  expect_equal(subset(MAL_data, IndvID == "2KV75337")$Species, "PARMAJ")
  expect_equal(subset(MAL_data, IndvID == "2KV75337")$PopID, "MAL")
  expect_equal(subset(MAL_data, IndvID == "2KV75337")$BroodIDLaid, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "2KV75337")$BroodIDFledged, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "2KV75337")$RingSeason, 2019)
  expect_equal(subset(MAL_data, IndvID == "2KV75337")$RingAge, "chick")
  expect_equal(subset(MAL_data, IndvID == "2KV75337")$Sex_calculated, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "2KV75337")$Sex_genetic, NA_character_)


  #Individual 2KR03683
  expect_equal(subset(MAL_data, IndvID == "2KR03683")$Species, "PARMAJ")
  expect_equal(subset(MAL_data, IndvID == "2KR03683")$PopID, "MAL")
  expect_equal(subset(MAL_data, IndvID == "2KR03683")$BroodIDLaid, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "2KR03683")$BroodIDFledged, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "2KR03683")$RingSeason, 2013)
  expect_equal(subset(MAL_data, IndvID == "2KR03683")$RingAge, "adult")
  expect_equal(subset(MAL_data, IndvID == "2KR03683")$Sex_calculated, "M")
  expect_equal(subset(MAL_data, IndvID == "2KR03683")$Sex_genetic, NA_character_)


  #Individual 2KS91300
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$Species, "PARMAJ")
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$PopID, "MAL")
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$BroodIDLaid, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$BroodIDFledged, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$RingSeason, 2013)
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$RingAge, "adult")
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$Sex_calculated, "F")
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$Sex_genetic, NA_character_)


  #Individual 1ET84725
  expect_equal(subset(MAL_data, IndvID == "1ET84725")$Species, "CYACAE")
  expect_equal(subset(MAL_data, IndvID == "1ET84725")$PopID, "MAL")
  expect_equal(subset(MAL_data, IndvID == "1ET84725")$BroodIDLaid, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "1ET84725")$BroodIDFledged, NA_character_)
  expect_equal(subset(MAL_data, IndvID == "1ET84725")$RingSeason, 2018)
  expect_equal(subset(MAL_data, IndvID == "1ET84725")$RingAge, "adult")
  expect_equal(subset(MAL_data, IndvID == "1ET84725")$Sex_calculated, "F")
  expect_equal(subset(MAL_data, IndvID == "1ET84725")$Sex_genetic, NA_character_)

})


test_that("Location_data returns an expected outcome...", {

  #Take a subset of only MAL data
  MAL_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("MAL"))

  ## Nestbox S94
  expect_equal(subset(MAL_data, LocationID == "S94")$NestboxID, "S94")
  expect_equal(subset(MAL_data, LocationID == "S94")$LocationType, "NB")
  expect_equal(subset(MAL_data, LocationID == "S94")$PopID, "MAL")
  expect_equal(subset(MAL_data, LocationID == "S94")$Latitude, 55.59954604)
  expect_equal(subset(MAL_data, LocationID == "S94")$Longitude, 12.98640504)
  expect_equal(subset(MAL_data, LocationID == "S94")$StartSeason, 2018)
  expect_equal(subset(MAL_data, LocationID == "S94")$EndSeason, NA_integer_)
  expect_equal(subset(MAL_data, LocationID == "S94")$HabitatType, "urban")

  ## Nestbox S63
  expect_equal(subset(MAL_data, LocationID == "S63")$NestboxID, "S63")
  expect_equal(subset(MAL_data, LocationID == "S63")$LocationType, "NB")
  expect_equal(subset(MAL_data, LocationID == "S63")$PopID, "MAL")
  expect_equal(subset(MAL_data, LocationID == "S63")$Latitude, 55.60013998)
  expect_equal(subset(MAL_data, LocationID == "S63")$Longitude, 12.99065298)
  expect_equal(subset(MAL_data, LocationID == "S63")$StartSeason, 2017)
  expect_equal(subset(MAL_data, LocationID == "S63")$EndSeason, 2019)
  expect_equal(subset(MAL_data, LocationID == "S63")$HabitatType, "urban") # May change depending on input from data owner

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
  # test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[:digit:]+$")
  #
  # ## MaleID format is as expected
  # test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[:digit:]+$")

  # ## IndvID format in Capture data  is as expected
  # test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[:digit:]+$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[:digit:]+$")

})


test_that("Key columns only contain unique values", {

  # ## BroodID has only unique values
  # test_unique_values(pipeline_output, "BroodID")

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
