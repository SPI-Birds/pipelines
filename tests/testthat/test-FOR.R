testthat::skip_if(!exists("data_path"))

pipeline_output <- format_FOR(db = paste0(data_path, "/FOR_ForstenriederPark_Germany"))

test_that("FOR outputs all files...", {

  expect_true(all("FOR" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("FOR" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("FOR" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("FOR" %in% pipeline_output$Location_data$PopID))

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only FOR data
  FOR_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "FOR")

  ## Blue tit female ringed as adult
  expect_equal(subset(FOR_data, IndvID == "B5K1456")$Sex_calculated, "F")
  expect_equal(subset(FOR_data, IndvID == "B5K1456")$Species, "CYACAE")
  expect_equal(subset(FOR_data, IndvID == "B5K1456")$RingAge, "adult")
  expect_equal(subset(FOR_data, IndvID == "B5K1456")$Sex_genetic, NA_character_)
  expect_equal(subset(FOR_data, IndvID == "B5K1456")$BroodIDLaid, NA_character_)
  expect_equal(subset(FOR_data, IndvID == "B5K1456")$RingSeason, 2023)

  # Great male ringed as adult
  expect_equal(subset(FOR_data, IndvID == "C3F7191")$Sex_calculated, "M")
  expect_equal(subset(FOR_data, IndvID == "C3F7191")$Species, "PARMAJ")
  expect_equal(subset(FOR_data, IndvID == "C3F7191")$RingAge, "adult")
  expect_equal(subset(FOR_data, IndvID == "C3F7191")$Sex_genetic, NA_character_)
  expect_equal(subset(FOR_data, IndvID == "C3F7191")$BroodIDLaid, NA_character_)
  expect_equal(subset(FOR_data, IndvID == "C3F7191")$RingSeason, 2020)


  #Caught first as chick
  expect_equal(subset(FOR_data, IndvID == "B5G6103")$Sex_calculated, "F")
  expect_equal(subset(FOR_data, IndvID == "B5G6103")$Sex_genetic, NA_character_)
  expect_equal(subset(FOR_data, IndvID == "B5G6103")$Species, "CYACAE")
  expect_equal(subset(FOR_data, IndvID == "B5G6103")$BroodIDLaid, "1533")
  expect_equal(subset(FOR_data, IndvID == "B5G6103")$BroodIDFledged, "1533")
  expect_equal(subset(FOR_data, IndvID == "B5G6103")$RingSeason, 2022L)
  expect_equal(subset(FOR_data, IndvID == "B5G6103")$RingAge, "chick")

})

test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only FOR data
  FOR_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "FOR")

  ## PARMAJ nest B53 in 2019
  expect_equal(subset(FOR_data,
                      LocationID == "B53_2019" &
                        BreedingSeason == 2019 &
                        ClutchType_observed=="second")$FemaleID, "C3F7169")
  expect_equal(subset(FOR_data,
                      LocationID == "B53_2019" &
                        BreedingSeason == 2019 &
                        ClutchType_observed=="second")$MaleID, "C3F6823")
  expect_equal(subset(FOR_data,
                      LocationID == "B53_2019" &
                        BreedingSeason == 2019 &
                        ClutchType_observed=="second")$LayDate_observed, as.Date("2019-05-31"))
  expect_equal(subset(FOR_data,
                      LocationID == "B53_2019" &
                        BreedingSeason == 2019 &
                        ClutchType_observed=="second")$HatchDate_observed, as.Date("2019-06-17"))
  expect_equal(subset(FOR_data,
                      LocationID == "B53_2019" &
                        BreedingSeason == 2019 &
                        ClutchType_observed=="second")$ClutchSize_observed, 6)
  expect_equal(subset(FOR_data,
                      LocationID == "B53_2019" &
                        BreedingSeason == 2019 &
                        ClutchType_observed=="second")$BroodSize_observed, 4)
  expect_equal(subset(FOR_data,
                      LocationID == "B53_2019" &
                        BreedingSeason == 2019 &
                        ClutchType_observed=="second")$NumberFledged_observed, 3)

  ## CYACAE nest E15 in 2021
  expect_equal(subset(FOR_data,
                      LocationID == "E15_2021" &
                        BreedingSeason == 2021)$FemaleID, "B4P1449")
  expect_equal(subset(FOR_data,
                      LocationID == "E15_2021" &
                        BreedingSeason == 2021)$MaleID, "B4P1450")
  expect_equal(subset(FOR_data,
                      LocationID == "E15_2021" &
                        BreedingSeason == 2021)$LayDate_observed, as.Date("2021-04-15"))
  expect_equal(subset(FOR_data,
                      LocationID == "E15_2021" &
                        BreedingSeason == 2021)$HatchDate_observed, as.Date("2021-05-11"))
  expect_equal(subset(FOR_data,
                      LocationID == "E15_2021" &
                        BreedingSeason == 2021)$ClutchSize_observed, 9)
  expect_equal(subset(FOR_data,
                      LocationID == "E15_2021" &
                        BreedingSeason == 2021)$BroodSize_observed, 8)
  expect_equal(subset(FOR_data,
                      LocationID == "E15_2021" &
                        BreedingSeason == 2021)$NumberFledged_observed, 8)
  ## PARMAJ nest D50 in 2022
  expect_equal(subset(FOR_data,
                      LocationID == "D50_2022" &
                        BreedingSeason == 2022 &
                        Species=="PARMAJ")$FemaleID, NA_character_)
  expect_equal(subset(FOR_data,
                      LocationID == "D50_2022" &
                        BreedingSeason == 2022 &
                        Species=="PARMAJ")$MaleID, NA_character_)
  expect_equal(subset(FOR_data,
                      LocationID == "D50_2022" &
                        BreedingSeason == 2022 &
                        Species=="PARMAJ")$LayDate_observed, as.Date("2022-04-30"))
  expect_equal(subset(FOR_data,
                      LocationID == "D50_2022" &
                        BreedingSeason == 2022 &
                        Species=="PARMAJ")$HatchDate_observed, as.Date(NA))
  expect_equal(subset(FOR_data,
                      LocationID == "D50_2022" &
                        BreedingSeason == 2022 &
                        Species=="PARMAJ")$ClutchSize_observed, 9)
  expect_equal(subset(FOR_data,
                      LocationID == "D50_2022" &
                        BreedingSeason == 2022 &
                        Species=="PARMAJ")$BroodSize_observed, 0)
  expect_equal(subset(FOR_data,
                      LocationID == "D50_2022" &
                        BreedingSeason == 2022 &
                        Species=="PARMAJ")$NumberFledged_observed, 0)

  ## CYACAE nest  C44 in 2023
  expect_equal(subset(FOR_data,
                      LocationID == "C44_2023" &
                        BreedingSeason == 2023)$FemaleID, NA_character_)
  expect_equal(subset(FOR_data,
                      LocationID == "C44_2023" &
                        BreedingSeason == 2023)$MaleID, NA_character_)
  expect_equal(subset(FOR_data,
                      LocationID == "C44_2023" &
                        BreedingSeason == 2023)$LayDate_observed, as.Date("2023-04-20"))
  expect_equal(subset(FOR_data,
                      LocationID == "C44_2023" &
                        BreedingSeason == 2023)$HatchDate_observed, as.Date("2023-05-16"))
  expect_equal(subset(FOR_data,
                      LocationID == "C44_2023" &
                        BreedingSeason == 2023)$ClutchSize_observed, 12)
  expect_equal(subset(FOR_data,
                      LocationID == "C44_2023" &
                        BreedingSeason == 2023)$BroodSize_observed, 9)
  expect_equal(subset(FOR_data,
                      LocationID == "C44_2023" &
                        BreedingSeason == 2023)$NumberFledged_observed, 0)
})

test_that("Capture_data returns an expected outcome...", {


  ## Take a subset of only FOR data
  FOR_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "FOR")

  ## PARMAJ individual C3F7095 caught in three years with conflicting sex
  expect_equal(subset(FOR_data,
                      IndvID == "C3F7095")$BreedingSeason, c(2019,2020, 2023))
  expect_equal(subset(FOR_data,
                      IndvID == "C3F7095")$Sex_observed, c(NA,"F", "M"))
  expect_equal(subset(FOR_data,
                      IndvID == "C3F7095")$LocationID, c("C20_2019","B62_2020", "C27_2023"))
  expect_equal(nrow(subset(FOR_data,
                      IndvID == "C3F7095")), 3)

  ## PARMAJ individual C5H7348 caught twice in same year 2023
  expect_equal(unique(subset(FOR_data,
                             IndvID == "C5H7348")$BreedingSeason), 2023)
  expect_equal(unique(subset(FOR_data,
                             IndvID == "C5H7348")$Species), "PARMAJ")
  expect_equal(unique(subset(FOR_data,
                             IndvID == "C5H7348")$Sex_observed), "M")
  expect_equal(unique(subset(FOR_data,
                             IndvID == "C5H7348")$LocationID), "C69_2023")
  expect_equal(nrow(subset(FOR_data,
                           IndvID == "C5H7348")), 2)

  ## PARMAJ individual C5A0710
  expect_equal(subset(FOR_data,
                      IndvID == "C5A0710")$BreedingSeason, 2020)
  expect_equal(subset(FOR_data,
                      IndvID == "C5A0710")$Sex_observed, NA_character_)
  expect_equal(subset(FOR_data,
                      IndvID == "C5A0710")$CaptureDate, as.Date("2020-05-14"))
  expect_equal(subset(FOR_data,
                      IndvID == "C5A0710")$ChickAge, 14)
  expect_equal(subset(FOR_data,
                      IndvID == "C5A0710")$Mass, 13.57)
  expect_equal(subset(FOR_data,
                            IndvID == "C5A0710")$WingLength, 42)
  expect_equal(subset(FOR_data,
                      IndvID == "C5A0710")$Species, "PARMAJ")
  expect_equal(subset(FOR_data,
                      IndvID == "C5A0710")$LocationID, "B53_2020")


  ## CYACAE individual 33471
  expect_equal(subset(FOR_data,
                      IndvID == "B5K1466")$BreedingSeason, 2023)
  expect_equal(subset(FOR_data,
                      IndvID == "B5K1466")$Sex_observed, "F")
  expect_equal(subset(FOR_data,
                      IndvID == "B5K1466")$Species, "CYACAE")
  expect_equal(subset(FOR_data,
                      IndvID == "B5K1466")$LocationID, "F47a_2023")
  expect_equal(subset(FOR_data,
                      IndvID == "B5K1466")$CaptureDate, as.Date("2023-06-09"))
  expect_equal(subset(FOR_data,
                            IndvID == "B5K1466")$Tarsus, 16.8)
  expect_equal(subset(FOR_data,
                      IndvID == "B5K1466")$WingLength, 64)

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only FOR data
  FOR_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "FOR")

  # All should have the same latitude equal to 50.06
  expect_equal(unique(subset(FOR_data, LocationID == "C12_2019")$Latitude), 48.08268302)

  # All should have the same longitude equal to 20.25
  expect_equal(unique(subset(FOR_data, LocationID == "C12_2019")$Longitude), 11.46394498)

  # StartSeason for nestbox C12 is 2019
  expect_equal(subset(FOR_data, LocationID == "C12_2019")$StartSeason, 2019)

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
  test_ID_format(pipeline_output, ID_col = "FemaleID", ID_format = "^[[:digit:][:alpha:]]{7}$")

  # ## MaleID format is as expected
  test_ID_format(pipeline_output, ID_col = "MaleID", ID_format = "^[[:digit:][:alpha:]]{7}$")

  # ## IndvID format in Capture data  is as expected
  test_ID_format(pipeline_output, ID_col = "C-IndvID", ID_format = "^[[:digit:][:alpha:]]{7}$")

  ## IndvID format in Individual data is as expected
  test_ID_format(pipeline_output, ID_col = "I-IndvID", ID_format = "^[[:digit:][:alpha:]]{7}$")

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

