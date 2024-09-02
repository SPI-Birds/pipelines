testthat::skip_if(!exists("data_path"))
#first step: get format_CAC in the environment (either run full script or import function using dget)

#pipeline_output <- format_CAC(db = paste0(data_path, "/CAC_Senar_Spain"))
pipeline_output <- format_CAC(db=choose.dir())#warnings:  Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.

test_that("CAC outputs all files...", {

  expect_true(all("CAC" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("CAC" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("CAC" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("CAC" %in% pipeline_output$Location_data$PopID))

})


### Test Capture data
##TODO: adapt the script to CAC

test_that("Individual data returns an expected outcome...", {

  #Open SAG data
  SAG_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "SAG")


  ## Great tit female ringed as adult
  expect_equal(subset(SAG_data, IndvID == "20012100")$Sex_calculated, "F")
  expect_equal(subset(SAG_data, IndvID == "20012100")$Species, "PARMAJ")
  expect_equal(subset(SAG_data, IndvID == "20012100")$RingAge, "adult")
  expect_equal(subset(SAG_data, IndvID == "20012100")$Sex_genetic, NA_character_)
  expect_equal(subset(SAG_data, IndvID == "20012100")$BroodIDLaid, NA_character_)
  expect_equal(subset(SAG_data, IndvID == "20012100")$RingSeason, 2022)

  # Great tit male ringed as adult
  expect_equal(subset(SAG_data, IndvID == "20012145")$Sex_calculated, "M")
  expect_equal(subset(SAG_data, IndvID == "20012145")$Species, "PARMAJ")
  expect_equal(subset(SAG_data, IndvID == "20012145")$RingAge, "adult")
  expect_equal(subset(SAG_data, IndvID == "20012145")$Sex_genetic, NA_character_)
  expect_equal(subset(SAG_data, IndvID == "20012145")$BroodIDLaid, NA_character_)
  expect_equal(subset(SAG_data, IndvID == "20012145")$RingSeason, 2022)

  #Caught first as chick
  expect_equal(subset(SAG_data, IndvID == "2716306")$Sex_calculated, "M")
  expect_equal(subset(SAG_data, IndvID == "2716306")$Sex_genetic, NA_character_)
  expect_equal(subset(SAG_data, IndvID == "2716306")$Species, "PARMAJ")
  expect_equal(subset(SAG_data, IndvID == "2716306")$BroodIDLaid, "10514")
  expect_equal(subset(SAG_data, IndvID == "2716306")$BroodIDFledged, "10514")
  expect_equal(subset(SAG_data, IndvID == "2716306")$RingSeason, 2014)
  expect_equal(subset(SAG_data, IndvID == "2716306")$RingAge, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  #Open SAG data
  SAG_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "SAG")

  ## PARMAJ nest 304_2013 in 2013
  expect_equal(subset(SAG_data,
                      LocationID == "304_2013" &
                        BreedingSeason == 2013 &
                        ClutchType_observed=="first")$FemaleID, "2A421114")
  expect_equal(subset(SAG_data,
                      LocationID == "304_2013" &
                        BreedingSeason == 2013 &
                        ClutchType_observed=="first")$MaleID, "2A396226")#did not work
  expect_equal(subset(SAG_data,
                      LocationID == "304_2013" &
                        BreedingSeason == 2013 &
                        ClutchType_observed=="first")$LayDate_observed, as.Date("2013-04-09"))
  expect_equal(subset(SAG_data,
                      LocationID == "304_2013" &
                        BreedingSeason == 2013 &
                        ClutchType_observed=="first")$HatchDate_observed, as.Date("2013-04-29"))
  expect_equal(subset(SAG_data,
                      LocationID == "304_2013" &
                        BreedingSeason == 2013 &
                        ClutchType_observed=="first")$ClutchSize_observed, 9)
  expect_equal(subset(SAG_data,
                      LocationID == "304_2013" &
                        BreedingSeason == 2013 &
                        ClutchType_observed=="first")$BroodSize_observed, 8)
  expect_equal(subset(SAG_data,
                      LocationID == "304_2013" &
                        BreedingSeason == 2013 &
                        ClutchType_observed=="first")$NumberFledged_observed, 6)

  ## PARMAJ nest 125_1988 in 1988
  expect_equal(subset(SAG_data,
                      LocationID == "125_1988" &
                        BreedingSeason == 1988)$ClutchType_observed, "second")
  expect_equal(subset(SAG_data,
                      LocationID == "125_1988" &
                        BreedingSeason == 1988)$FemaleID, NA_character_)
  expect_equal(subset(SAG_data,
                      LocationID == "125_1988" &
                        BreedingSeason == 1988)$MaleID, NA_character_)
  expect_equal(subset(SAG_data,
                      LocationID == "125_1988" &
                        BreedingSeason == 1988)$LayDate_observed, as.Date("1988-05-20"))
  expect_equal(subset(SAG_data,
                      LocationID == "125_1988" &
                        BreedingSeason == 1988)$HatchDate_observed, as.Date(NA))
  expect_equal(subset(SAG_data,
                      LocationID == "125_1988" &
                        BreedingSeason == 1988)$ClutchSize_observed, 8)
  expect_equal(subset(SAG_data,
                      LocationID == "125_1988" &
                        BreedingSeason == 1988)$BroodSize_observed, 8)
  expect_equal(subset(SAG_data,
                      LocationID == "125_1988" &
                        BreedingSeason == 1988)$NumberFledged_observed, 3)
  ## PARMAJ nest 37C_1992 in 2022
  expect_equal(subset(SAG_data,
                      LocationID == "37C_1992" &
                        BreedingSeason == 1992 &
                        ClutchType_observed=="first")$ExperimentID, "PHENOLOGY")
  expect_equal(subset(SAG_data,
                      LocationID == "37C_1992" &
                        BreedingSeason == 1992 &
                        ClutchType_observed=="first")$FemaleID, NA_character_)
  expect_equal(subset(SAG_data,
                      LocationID == "37C_1992" &
                        BreedingSeason == 1992 &
                        ClutchType_observed=="first")$MaleID, NA_character_)
  expect_equal(subset(SAG_data,
                      LocationID == "37C_1992" &
                        BreedingSeason == 1992 &
                        ClutchType_observed=="first")$LayDate_observed, as.Date("1992-04-18"))
  expect_equal(subset(SAG_data,
                      LocationID == "37C_1992" &
                        BreedingSeason == 1992 &
                        ClutchType_observed=="first")$HatchDate_observed, as.Date("1992-05-07"))
  expect_equal(subset(SAG_data,
                      LocationID == "37C_1992" &
                        BreedingSeason == 1992 &
                        ClutchType_observed=="first")$ClutchSize_observed, 8)
  expect_equal(subset(SAG_data,
                      LocationID == "37C_1992" &
                        BreedingSeason == 1992 &
                        ClutchType_observed=="first")$BroodSize_observed, 8)
  expect_equal(subset(SAG_data,
                      LocationID == "37C_1992" &
                        BreedingSeason == 1992 &
                        ClutchType_observed=="first")$NumberFledged_observed, 0)

  ## PARMAJ nest  154_2022 in 2022
  expect_equal(subset(SAG_data,
                      LocationID == "154_2022" &
                        BreedingSeason == 2022)$FemaleID, "2A511899")
  expect_equal(subset(SAG_data,
                      LocationID == "154_2022" &
                        BreedingSeason == 2022)$MaleID, "20012004")
  expect_equal(subset(SAG_data,
                      LocationID == "154_2022" &
                        BreedingSeason == 2022)$LayDate_observed, as.Date("2022-04-17"))
  expect_equal(subset(SAG_data,
                      LocationID == "154_2022" &
                        BreedingSeason == 2022)$HatchDate_observed, as.Date("2022-05-06"))
  expect_equal(subset(SAG_data,
                      LocationID == "154_2022" &
                        BreedingSeason == 2022)$ClutchSize_observed, 8)
  expect_equal(subset(SAG_data,
                      LocationID == "154_2022" &
                        BreedingSeason == 2022)$BroodSize_observed, 8)
  expect_equal(subset(SAG_data,
                      LocationID == "154_2022" &
                        BreedingSeason == 2022)$NumberFledged_observed, 8)
})

test_that("Capture_data returns an expected outcome...", {

  #Open SAG data
  SAG_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "SAG")

  ##female 2315428 caught several times
  expect_equal(unique(subset(SAG_data,
                             IndvID == "2315428")$BreedingSeason), c(1992,1993,1994,1995,1996))
  expect_equal(unique(subset(SAG_data,
                             IndvID == "2315428")$Species), "PARMAJ")
  expect_equal(unique(subset(SAG_data,
                             IndvID == "2315428")$Sex_observed), "F")
  expect_equal(unique(subset(SAG_data,
                             IndvID == "2315428")$LocationID), c("4D_1992","77_1993" , "30A_1994", "61_1995","45D_1996"))
  expect_equal(nrow(subset(SAG_data,
                           IndvID == "2315428")), 8)

  ##male 20012090 ringed as chick
  expect_equal(subset(SAG_data,
                      IndvID == "20012090")$BreedingSeason, 2022)
  expect_equal(subset(SAG_data,
                      IndvID == "20012090")$Sex_observed, "M")
  expect_equal(subset(SAG_data,
                      IndvID == "20012090")$CaptureDate, as.Date("2022-05-25"))
  expect_equal(subset(SAG_data,
                      IndvID == "20012090")$ChickAge, 14)
  expect_equal(subset(SAG_data,
                      IndvID == "20012090")$Mass, 14.4)
  expect_equal(subset(SAG_data,
                      IndvID == "20012090")$Tarsus, 19.1)
  expect_equal(subset(SAG_data,
                      IndvID == "20012090")$WingLength, as.numeric(NA))
  expect_equal(subset(SAG_data,
                      IndvID == "20012090")$LocationID, "145_2022")


  ## female 2817040
  expect_equal(unique(subset(SAG_data,
                      IndvID == "2A141732")$BreedingSeason), c(2006,2008,2009))
  expect_equal(unique(subset(SAG_data,
                      IndvID == "2A141732")$Sex_observed), "F")
  expect_equal(unique(subset(SAG_data,
                      IndvID == "2A141732")$LocationID), c("635_2006","517_2008","20_2009"))
  expect_equal(unique(subset(SAG_data,
                      IndvID == "2A141732")$CaptureDate), as.Date(c("2006-06-25","2008-05-12","2009-05-16")))
  expect_equal(unique(subset(SAG_data,
                      IndvID == "2A141732")$Tarsus), c(20.12,20.40,19.7))
  expect_equal(unique(subset(SAG_data,
                      IndvID == "2A141732")$WingLength), c(NA,73,75))

})

test_that("Location_data returns an expected outcome...", {

  #Open SAG data
  SAG_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "SAG")
  ##Cannot do it without coordinates


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

