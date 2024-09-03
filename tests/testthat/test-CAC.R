testthat::skip_if(!exists("data_path"))
#first step: get format_CAC in the environment (either run full script or import function using dget)

#pipeline_output <- format_CAC(db = paste0(data_path, "/CAC_Senar_Spain"))
pipeline_output <- format_CAC(db=choose_directory())#warnings:  Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.

test_that("CAC outputs all files...", {

  expect_true(all("CAC" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("CAC" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("CAC" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("CAC" %in% pipeline_output$Location_data$PopID))

})


### Test Individual data

test_that("Individual data returns an expected outcome...", {

  #Open CAC data
  CAC_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "CAC")


  ## Great tit female ringed as adult
  expect_equal(subset(CAC_data, IndvID == "2690100")$Sex_calculated, "F")
  expect_equal(subset(CAC_data, IndvID == "2690100")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "2690100")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "2690100")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2690100")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2690100")$RingSeason, 1998)

  # Great tit male ringed as adult
  expect_equal(subset(CAC_data, IndvID == "N308473")$Sex_calculated, "M")
  expect_equal(subset(CAC_data, IndvID == "N308473")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "N308473")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "N308473")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "N308473")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "N308473")$RingSeason, 2007)

  #Great tit male caught first as chick
  expect_equal(subset(CAC_data, IndvID == "L950261")$Sex_calculated, "M")
  expect_equal(subset(CAC_data, IndvID == "L950261")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "L950261")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "L950261")$BroodIDLaid, "113_2004_PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "L950261")$BroodIDFledged, "113_2004_PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "L950261")$RingSeason, 2004)
  expect_equal(subset(CAC_data, IndvID == "L950261")$RingAge, "chick")

  ## Blue tit female ringed as adult
  expect_equal(subset(CAC_data, IndvID == "2Z06557")$Sex_calculated, "F")
  expect_equal(subset(CAC_data, IndvID == "2Z06557")$Species, "CYACAE")
  expect_equal(subset(CAC_data, IndvID == "2Z06557")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "2Z06557")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2Z06557")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2Z06557")$RingSeason, 2020)

  # Blue tit male ringed as adult
  expect_equal(subset(CAC_data, IndvID == "Z85300")$Sex_calculated, "M")
  expect_equal(subset(CAC_data, IndvID == "Z85300")$Species, "CYACAE")
  expect_equal(subset(CAC_data, IndvID == "Z85300")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "Z85300")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "Z85300")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "Z85300")$RingSeason, 2015)

  #Blue tit female caught first as chick
  expect_equal(subset(CAC_data, IndvID == "HK5315")$Sex_calculated, "F")
  expect_equal(subset(CAC_data, IndvID == "HK5315")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "HK5315")$Species, "CYACAE")
  expect_equal(subset(CAC_data, IndvID == "HK5315")$BroodIDLaid, "104_2011_CYACAE")
  expect_equal(subset(CAC_data, IndvID == "HK5315")$BroodIDFledged, "104_2011_CYACAE")
  expect_equal(subset(CAC_data, IndvID == "HK5315")$RingSeason, 2011)
  expect_equal(subset(CAC_data, IndvID == "HK5315")$RingAge, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  #Open CAC data
  CAC_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "CAC")

  ## PARMAJ nest 59 in 2020
  expect_equal(subset(CAC_data,
                      LocationID == "59" &
                        BreedingSeason == 2020 &
                        ClutchType_observed=="first")$FemaleID, "2KA77251")
  expect_equal(subset(CAC_data,
                      LocationID == "59" &
                        BreedingSeason == 2020 &
                        ClutchType_observed=="first")$Species, "PARMAJ")
  expect_equal(subset(CAC_data,
                      LocationID == "59" &
                        BreedingSeason == 2020 &
                        ClutchType_observed=="first")$MaleID, NA_character_)
  expect_equal(subset(CAC_data,
                      LocationID == "59" &
                        BreedingSeason == 2020 &
                        ClutchType_observed=="first")$LayDate_observed, as.Date("2020-04-14"))
  expect_equal(subset(CAC_data,
                      LocationID == "59" &
                        BreedingSeason == 2020 &
                        ClutchType_observed=="first")$HatchDate_observed, as.Date("2020-05-09"))
  expect_equal(subset(CAC_data,
                      LocationID == "59" &
                        BreedingSeason == 2020 &
                        ClutchType_observed=="first")$ClutchSize_observed, 7)
  expect_equal(subset(CAC_data,
                      LocationID == "59" &
                        BreedingSeason == 2020 &
                        ClutchType_observed=="first")$NumberFledged_observed, 5)
  expect_equal(subset(CAC_data,
                      LocationID == "59" &
                        BreedingSeason == 2020 &
                        ClutchType_observed=="first")$FledgeDate_observed, as.Date("2020-05-30"))

  ## PARMAJ nest 53 in 2004
  expect_equal(subset(CAC_data,
                      BroodID == "53a_2004_PARMAJ" &
                      BreedingSeason == 2004)$ClutchType_observed, "first")
  expect_equal(subset(CAC_data,
                      BroodID == "53a_2004_PARMAJ" &
                        BreedingSeason == 2004)$Species, "PARMAJ")
  expect_equal(subset(CAC_data,
                      BroodID == "53a_2004_PARMAJ" &
                        BreedingSeason == 2004)$FemaleID, "2997453")
  expect_equal(subset(CAC_data,
                      BroodID == "53a_2004_PARMAJ" &
                        BreedingSeason == 2004)$MaleID, "2997546")
  expect_equal(subset(CAC_data,
                      BroodID == "53a_2004_PARMAJ" &
                        BreedingSeason == 2004)$LayDate_observed, as.Date("2004-04-04"))
  expect_equal(subset(CAC_data,
                      BroodID == "53a_2004_PARMAJ" &
                        BreedingSeason == 2004)$HatchDate_observed, as.Date("2004-04-27"))
  expect_equal(subset(CAC_data,
                      BroodID == "53a_2004_PARMAJ" &
                        BreedingSeason == 2004)$ClutchSize_observed, 8)
  expect_equal(subset(CAC_data,
                      BroodID == "53a_2004_PARMAJ" &
                        BreedingSeason == 2004)$NumberFledged_observed, 0)
  ## PARMAJ nest 14 in 2022
  expect_equal(subset(CAC_data,
                      LocationID == "14" &
                        BreedingSeason == 2022 &
                        ClutchType_observed=="second")$ExperimentID, NA_character_)
  expect_equal(subset(CAC_data,
                      LocationID == "14" &
                        BreedingSeason == 2022 &
                        ClutchType_observed=="second")$FemaleID, "2KA77267")
  expect_equal(subset(CAC_data,
                      LocationID == "14" &
                        BreedingSeason == 2022 &
                        ClutchType_observed=="second")$MaleID, "C002466")
  expect_equal(subset(CAC_data,
                      LocationID == "14" &
                        BreedingSeason == 2022 &
                        ClutchType_observed=="second")$LayDate_observed, as.Date("2022-05-30"))
  expect_equal(subset(CAC_data,
                      LocationID == "14" &
                        BreedingSeason == 2022 &
                        ClutchType_observed=="second")$HatchDate_observed, as.Date("2022-06-16"))
  expect_equal(subset(CAC_data,
                      LocationID == "14" &
                        BreedingSeason == 2022 &
                        ClutchType_observed=="second")$ClutchSize_observed, 6)
  expect_equal(subset(CAC_data,
                      LocationID == "14" &
                        BreedingSeason == 2022 &
                        ClutchType_observed=="second")$NumberFledged_observed, 2)
  expect_equal(subset(CAC_data,
                      LocationID == "14" &
                        BreedingSeason == 2022 &
                        ClutchType_observed=="second")$FledgeDate_observed, as.Date("2022-07-04"))

  ## PARMAJ nest 28 in 2015
  expect_equal(subset(CAC_data,
                      LocationID == "28" &
                        BreedingSeason == 2015)$Species, "CYACAE")
  expect_equal(subset(CAC_data,
                      LocationID == "28" &
                        BreedingSeason == 2015)$FemaleID, NA_character_)
  expect_equal(subset(CAC_data,
                      LocationID == "28" &
                        BreedingSeason == 2015)$MaleID, NA_character_)
  expect_equal(subset(CAC_data,
                      LocationID == "28" &
                        BreedingSeason == 2015)$LayDate_observed, as.Date("2015-04-23"))
  expect_equal(subset(CAC_data,
                      LocationID == "28" &
                        BreedingSeason == 2015)$HatchDate_observed, as.Date("2015-05-13"))
  expect_equal(subset(CAC_data,
                      LocationID == "28" &
                        BreedingSeason == 2015)$ClutchSize_observed, 8)
  expect_equal(subset(CAC_data,
                      LocationID == "28" &
                        BreedingSeason == 2015)$NumberFledged_observed, 7)
  expect_equal(subset(CAC_data,
                      LocationID == "28" &
                        BreedingSeason == 2015)$FledgeDate_observed, as.Date("2015-06-01"))
})

test_that("Capture_data returns an expected outcome...", {

  #Open CAC data
  CAC_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "CAC")

  ##blue tit 891511 caught several times
  expect_equal(unique(subset(CAC_data,
                             IndvID == "891511")$BreedingSeason), c(1997,1998))
  expect_equal(unique(subset(CAC_data,
                             IndvID == "891511")$Species), "CYACAE")
  expect_equal(unique(subset(CAC_data,
                             IndvID == "891511")$Sex_observed), c(NA,"F"))
  expect_equal(unique(subset(CAC_data,
                             IndvID == "891511")$LocationID), c(NA,"117"))
  expect_equal(nrow(subset(CAC_data,
                           IndvID == "891511")), 3)

  ##great tit 1KA34008 ringed as chick
  expect_equal(subset(CAC_data,
                      IndvID == "1KA34008")$BreedingSeason, 2015)
  expect_equal(subset(CAC_data,
                      IndvID == "1KA34008")$CaptureDate, as.Date("2015-05-14"))
  expect_equal(subset(CAC_data,
                      IndvID == "1KA34008")$ChickAge, 14)
  expect_equal(subset(CAC_data,
                      IndvID == "1KA34008")$Mass, 15.1)
  expect_equal(subset(CAC_data,
                      IndvID == "1KA34008")$Tarsus, 19.3)
  expect_equal(subset(CAC_data,
                      IndvID == "1KA34008")$LocationID, "84")


  ## female 2601411
  expect_equal(unique(subset(CAC_data,
                      IndvID == "2601411")$BreedingSeason), c(2000,2001,2002,2003,2004))
  expect_equal(unique(subset(CAC_data,
                      IndvID == "2601411")$Sex_observed), "F")
  expect_equal(unique(subset(CAC_data,
                      IndvID == "2601411")$LocationID), c(NA,"5","51","53","154"))
  expect_equal(unique(subset(CAC_data,
                      IndvID == "2601411")$Tarsus), c(NA,19.4,19.6,19.0,19.3))
  expect_equal(unique(subset(CAC_data,
                      IndvID == "2601411")$WingLength), c(NA,72.0,73.0,75.5,75.0))

})

test_that("Location_data returns an expected outcome...", {

  #Open CAC data
  CAC_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "CAC")

  expect_equal(unique(subset(CAC_data,
                             LocationID == "52")$Latitude),41.467601003 )
  expect_equal(unique(subset(CAC_data,
                             LocationID == "52")$Longitude), 2.147169517)
  expect_equal(unique(subset(CAC_data,
                             LocationID == "52")$StartSeason), 1998)

  expect_equal(unique(subset(CAC_data,
                             LocationID == "175")$Latitude),41.468254349 )
  expect_equal(unique(subset(CAC_data,
                             LocationID == "175")$Longitude),2.146151032 )
  expect_equal(unique(subset(CAC_data,
                             LocationID == "175")$StartSeason), 2008)

  expect_equal(unique(subset(CAC_data,
                             LocationID == "185")$Latitude), 41.462856906)
  expect_equal(unique(subset(CAC_data,
                             LocationID == "185")$Longitude), 2.14061869)
  expect_equal(unique(subset(CAC_data,
                             LocationID == "185")$StartSeason), 2013)

})


### General tests (for pipelines formatted to standard protocol version 1.1.0): ALL passed

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
  test_col_classes(pipeline_output, "Brood")

  ## Capture data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Capture")

  ## Individual data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Individual")

  ## Location data: Test that all column classes are expected
  test_col_classes(pipeline_output, "Location")

})


test_that("ID columns match the expected format for the pipeline", {

  # ## FemaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[:alnum:]{6,8}$")

  # ## MaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[:alnum:]{6,8}$")

  # ## IndvID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[:alnum:]{6,8}$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[:alnum:]{6,8}$")

})


test_that("Key columns only contain unique values", {

  # ## BroodID has only unique values
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

