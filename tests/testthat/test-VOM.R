testthat::skip_if(!exists("data_path"))

pipeline_output <- format_VOM(db = paste0(data_path, "/VOM_VombFure_Sweden"))

PopID <- c("VOM")

test_that("format_VOM outputs all tables...", {

  expect_true(all(PopID %in% pipeline_output$Brood_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(PopID %in% pipeline_output$Individual_data$PopID))
  expect_true(all(PopID %in% pipeline_output$Location_data$PopID))

})


test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only VOM data
  VOM_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("VOM"))


  ## General brood data
  loc361_2018 <- subset(VOM_data,
                        BreedingSeason == "2018"
                        & PopID == "VOM"
                        & LocationID == "361")

  expect_equal(!is.na(loc361_2018$BroodID), TRUE)
  expect_equal(loc361_2018$BreedingSeason, 2018)
  expect_equal(loc361_2018$Species, "CYACAE")
  expect_equal(loc361_2018$FemaleID, NA_character_)
  expect_equal(loc361_2018$MaleID, NA_character_)
  expect_equal(loc361_2018$LayDate_observed, lubridate::NA_Date_)
  expect_equal(loc361_2018$ClutchSize_observed, 8)
  expect_equal(loc361_2018$HatchDate_observed, as.Date("2018-04-22"))
  expect_equal(loc361_2018$BroodSize_observed, 8)
  expect_equal(loc361_2018$NumberFledged_observed, NA_integer_)
  expect_equal(loc361_2018$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc361_2018$AvgEggMass, NA_real_)
  expect_equal(loc361_2018$NumberEggs, NA_integer_)


  ## General brood data
  loc171_2019 <- subset(VOM_data,
                        BreedingSeason == "2019"
                        & PopID == "VOM"
                        & LocationID == "171")

  expect_equal(!is.na(loc171_2019$BroodID), TRUE)
  expect_equal(loc171_2019$BreedingSeason, 2019)
  expect_equal(loc171_2019$Species, "PARMAJ")
  expect_equal(loc171_2019$FemaleID, "2KV76825")
  expect_equal(loc171_2019$MaleID, "2KV76835")
  expect_equal(loc171_2019$LayDate_observed, as.Date("2019-04-22"))
  expect_equal(loc171_2019$ClutchSize_observed, 10)
  expect_equal(loc171_2019$HatchDate_observed, as.Date("2019-05-15"))
  expect_equal(loc171_2019$BroodSize_observed, 10)
  expect_equal(loc171_2019$NumberFledged_observed, NA_integer_)
  expect_equal(loc171_2019$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc171_2019$AvgEggMass, NA_real_)
  expect_equal(loc171_2019$NumberEggs, NA_integer_)

  ## General brood data
  loc109_2018 <- subset(VOM_data,
                        BreedingSeason == "2018"
                        & PopID == "VOM"
                        & LocationID == "109")

  expect_equal(!is.na(loc109_2018$BroodID), TRUE)
  expect_equal(loc109_2018$BreedingSeason, 2018)
  expect_equal(loc109_2018$Species, "PARMAJ")
  expect_equal(loc109_2018$FemaleID, NA_character_)
  expect_equal(loc109_2018$MaleID, NA_character_)
  expect_equal(loc109_2018$LayDate_observed, as.Date("2018-04-24"))
  expect_equal(loc109_2018$ClutchSize_observed, 9)
  expect_equal(loc109_2018$HatchDate_observed, as.Date("2018-05-16"))
  expect_equal(loc109_2018$BroodSize_observed, 5)
  expect_equal(loc109_2018$NumberFledged_observed, 5)
  expect_equal(loc109_2018$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(loc109_2018$AvgEggMass, NA_real_)
  expect_equal(loc109_2018$NumberEggs, NA_integer_)

})

test_that("Capture_data returns an expected outcome...", {

  #Take a subset of only VOM data
  VOM_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("VOM"))

  ## 2KV76845
  cap_2KV76845 <- subset(VOM_data,
                         IndvID == "2KV76845"
                         & BreedingSeason == 2019)

  expect_equal(cap_2KV76845$Species, "PARMAJ")
  expect_equal(cap_2KV76845$Sex_observed, "M")
  expect_equal(cap_2KV76845$BreedingSeason, 2019)
  expect_equal(cap_2KV76845$CaptureTime, NA_character_)
  expect_equal(cap_2KV76845$LocationID, "307")
  expect_equal(cap_2KV76845$CapturePlot, NA_character_)
  expect_equal(cap_2KV76845$ReleasePopID, "VOM")
  expect_equal(cap_2KV76845$ReleasePlot, NA_character_)
  expect_equal(cap_2KV76845$Mass, NA_real_)
  expect_equal(cap_2KV76845$Tarsus, NA_real_)
  expect_equal(cap_2KV76845$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_2KV76845$WingLength, NA_real_)
  expect_equal(cap_2KV76845$Age_observed, NA_integer_)
  expect_equal(cap_2KV76845$Age_calculated, NA_integer_)
  expect_equal(cap_2KV76845$ChickAge, NA_integer_)
  expect_equal(cap_2KV76845$ExperimentID, NA_character_)

  ## 2KV06196
  cap_2KV06196 <- subset(VOM_data,
                         IndvID == "2KV06196"
                         & BreedingSeason == 2018)

  expect_equal(cap_2KV06196$Species, "PARMAJ")
  expect_equal(cap_2KV06196$Sex_observed, "F")
  expect_equal(cap_2KV06196$BreedingSeason, 2018)
  expect_equal(cap_2KV06196$CaptureTime, NA_character_)
  expect_equal(cap_2KV06196$LocationID, "13")
  expect_equal(cap_2KV06196$CapturePlot, NA_character_)
  expect_equal(cap_2KV06196$ReleasePopID, "VOM")
  expect_equal(cap_2KV06196$ReleasePlot, NA_character_)
  expect_equal(cap_2KV06196$Mass, NA_real_)
  expect_equal(cap_2KV06196$Tarsus, NA_real_)
  expect_equal(cap_2KV06196$OriginalTarsusMethod, NA_character_)
  expect_equal(cap_2KV06196$WingLength, NA_real_)
  expect_equal(cap_2KV06196$Age_observed, NA_integer_)
  expect_equal(cap_2KV06196$Age_calculated, NA_integer_)
  expect_equal(cap_2KV06196$ChickAge, NA_integer_)
  expect_equal(cap_2KV06196$ExperimentID, NA_character_)


})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only VOM data
  VOM_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("VOM"))

  #Individual 2KR97725
  expect_equal(subset(VOM_data, IndvID == "2KR97725")$Species, "PARMAJ")
  expect_equal(subset(VOM_data, IndvID == "2KR97725")$PopID, "VOM")
  expect_equal(subset(VOM_data, IndvID == "2KR97725")$BroodIDLaid, NA_character_)
  expect_equal(subset(VOM_data, IndvID == "2KR97725")$BroodIDFledged, NA_character_)
  expect_equal(subset(VOM_data, IndvID == "2KR97725")$RingSeason, 2013)
  expect_equal(subset(VOM_data, IndvID == "2KR97725")$RingAge, "adult")
  expect_equal(subset(VOM_data, IndvID == "2KR97725")$Sex_calculated, "M")
  expect_equal(subset(VOM_data, IndvID == "2KR97725")$Sex_genetic, NA_character_)


  #Individual 2KP22118
  expect_equal(subset(VOM_data, IndvID == "2KP22118")$Species, "PARMAJ")
  expect_equal(subset(VOM_data, IndvID == "2KP22118")$PopID, "VOM")
  expect_equal(subset(VOM_data, IndvID == "2KP22118")$BroodIDLaid, NA_character_)
  expect_equal(subset(VOM_data, IndvID == "2KP22118")$BroodIDFledged, NA_character_)
  expect_equal(subset(VOM_data, IndvID == "2KP22118")$RingSeason, 2014)
  expect_equal(subset(VOM_data, IndvID == "2KP22118")$RingAge, "adult")
  expect_equal(subset(VOM_data, IndvID == "2KP22118")$Sex_calculated, "F")
  expect_equal(subset(VOM_data, IndvID == "2KP22118")$Sex_genetic, NA_character_)


  #Individual 2KT47389
  expect_equal(subset(VOM_data, IndvID == "2KT47389")$Species, "PARMAJ")
  expect_equal(subset(VOM_data, IndvID == "2KT47389")$PopID, "VOM")
  expect_equal(subset(VOM_data, IndvID == "2KT47389")$BroodIDLaid, NA_character_)
  expect_equal(subset(VOM_data, IndvID == "2KT47389")$BroodIDFledged, NA_character_)
  expect_equal(subset(VOM_data, IndvID == "2KT47389")$RingSeason, 2015)
  expect_equal(subset(VOM_data, IndvID == "2KT47389")$RingAge, "adult")
  expect_equal(subset(VOM_data, IndvID == "2KT47389")$Sex_calculated, "M")
  expect_equal(subset(VOM_data, IndvID == "2KT47389")$Sex_genetic, NA_character_)


})


test_that("Location_data returns an expected outcome...", {

  #Take a subset of only VOM data
  VOM_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("VOM"))

  ## Nestbox 361
  expect_equal(subset(VOM_data, LocationID == "361")$NestboxID, "361")
  expect_equal(subset(VOM_data, LocationID == "361")$LocationType, "NB")
  expect_equal(subset(VOM_data, LocationID == "361")$PopID, "VOM")
  expect_equal(subset(VOM_data, LocationID == "361")$Latitude, 55.67)
  expect_equal(subset(VOM_data, LocationID == "361")$Longitude, 13.55)
  expect_equal(subset(VOM_data, LocationID == "361")$StartSeason, 2015)
  expect_equal(subset(VOM_data, LocationID == "361")$HabitatType, "mixed")

  ## Nestbox 94
  expect_equal(subset(VOM_data, LocationID == "94")$NestboxID, "94")
  expect_equal(subset(VOM_data, LocationID == "94")$LocationType, "NB")
  expect_equal(subset(VOM_data, LocationID == "94")$PopID, "VOM")
  expect_equal(subset(VOM_data, LocationID == "94")$Latitude, 55.67)
  expect_equal(subset(VOM_data, LocationID == "94")$Longitude, 13.55)
  expect_equal(subset(VOM_data, LocationID == "94")$StartSeason, 2013)
  expect_equal(subset(VOM_data, LocationID == "94")$HabitatType, "mixed")

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
  test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^2{1}[:alpha:]{2}[:digit:]{5}$")

  ## MaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^2{1}[:alpha:]{2}[:digit:]{5}$")

  ## IndvID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^2{1}[:alpha:]{2}[:digit:]{5}$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^2{1}[:alpha:]{2}[:digit:]{5}$")

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
