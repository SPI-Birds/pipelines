testthat::skip_if(!exists("data_path"))

pipeline_output <- format_GRO(db = paste0(data_path, "/GRO_GroblaNiepolomice_Poland"))

test_that("GRO outputs all files...", {

  expect_true(all("GRO" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("GRO" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("GRO" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("GRO" %in% pipeline_output$Location_data$PopID))

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only GRO data
  GRO_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "GRO")

  ## Blue tit female ringed as adult
  expect_equal(subset(GRO_data, IndvID == "K1L7518")$Sex_calculated, "F")
  expect_equal(subset(GRO_data, IndvID == "K1L7518")$Species, "CYACAE")
  expect_equal(subset(GRO_data, IndvID == "K1L7518")$RingAge, "adult")
  expect_equal(subset(GRO_data, IndvID == "K1L7518")$Sex_genetic, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "K1L7518")$BroodIDLaid, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "K1L7518")$RingSeason, 2015)

  # Collared flycatcher male ringed as adult
  expect_equal(subset(GRO_data, IndvID == "LE31427")$Sex_calculated, "M")
  expect_equal(subset(GRO_data, IndvID == "LE31427")$Species, "FICALB")
  expect_equal(subset(GRO_data, IndvID == "LE31427")$RingAge, "adult")
  expect_equal(subset(GRO_data, IndvID == "LE31427")$Sex_genetic, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "LE31427")$BroodIDLaid, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "LE31427")$RingSeason, 1999)

  # Great tit female ringed as adult
  expect_equal(subset(GRO_data, IndvID == "K2B9426")$Sex_calculated, "F")
  expect_equal(subset(GRO_data, IndvID == "K2B9426")$Species, "PARMAJ")
  expect_equal(subset(GRO_data, IndvID == "K2B9426")$RingAge, "adult")
  expect_equal(subset(GRO_data, IndvID == "K2B9426")$Sex_genetic, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "K2B9426")$BroodIDLaid, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "K2B9426")$RingSeason, 2010)

  # Individual captured twice in different years
  expect_equal(subset(GRO_data, IndvID == "LE31410")$Sex_calculated, "F")
  expect_equal(subset(GRO_data, IndvID == "LE31410")$Species, "FICALB")
  expect_equal(subset(GRO_data, IndvID == "LE31410")$RingAge, "adult")
  expect_equal(subset(GRO_data, IndvID == "LE31410")$Sex_genetic, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "LE31410")$BroodIDLaid, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "LE31410")$RingSeason, 1999)

  # Individual with conflicting sex
  expect_equal(subset(GRO_data, IndvID == "KP76626")$Sex_calculated, "C")
  expect_equal(subset(GRO_data, IndvID == "KP76626")$Species, "PARMAJ")
  expect_equal(subset(GRO_data, IndvID == "KP76626")$RingAge, "adult")
  expect_equal(subset(GRO_data, IndvID == "KP76626")$Sex_genetic, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "KP76626")$BroodIDLaid, NA_character_)
  expect_equal(subset(GRO_data, IndvID == "KP76626")$RingSeason, 1999)

})

test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only GRO data
  GRO_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "GRO")

  ## PARMAJ nest H41 in 1999
  expect_equal(subset(GRO_data,
                      LocationID == "H41" &
                      BreedingSeason == 1999)$FemaleID, "KP46859")
  expect_equal(subset(GRO_data,
                      LocationID == "H41" &
                      BreedingSeason == 1999)$MaleID, NA_character_)
  expect_equal(subset(GRO_data,
                      LocationID == "H41" &
                      BreedingSeason == 1999)$LayDate_observed, as.Date("1999-04-17"))
  expect_equal(subset(GRO_data,
                      LocationID == "H41" &
                      BreedingSeason == 1999)$HatchDate_observed, as.Date("1999-11-10"))
  expect_equal(subset(GRO_data,
                      LocationID == "H41" &
                      BreedingSeason == 1999)$ClutchSize_observed, 12)
  expect_equal(subset(GRO_data,
                      LocationID == "H41" &
                        BreedingSeason == 1999)$BroodSize_observed, 12)
  expect_equal(subset(GRO_data,
                      LocationID == "H41" &
                        BreedingSeason == 1999)$NumberFledged_observed, NA_integer_)

  ## PARMAJ nest E67 in 2018
  expect_equal(subset(GRO_data,
                      LocationID == "E67" &
                        BreedingSeason == 2018)$FemaleID, NA_character_)
  expect_equal(subset(GRO_data,
                      LocationID == "E67" &
                        BreedingSeason == 2018)$MaleID, NA_character_)
  expect_equal(subset(GRO_data,
                      LocationID == "E67" &
                        BreedingSeason == 2018)$LayDate_observed, as.Date("2018-04-13"))
  expect_equal(subset(GRO_data,
                      LocationID == "E67" &
                        BreedingSeason == 2018)$HatchDate_observed, as.Date("2018-05-06"))
  expect_equal(subset(GRO_data,
                      LocationID == "E67" &
                        BreedingSeason == 2018)$ClutchSize_observed, NA_integer_)
  expect_equal(subset(GRO_data,
                      LocationID == "E67" &
                          BreedingSeason == 2018)$BroodSize_observed, 10)
  expect_equal(subset(GRO_data,
                      LocationID == "E67" &
                        BreedingSeason == 2018)$NumberFledged_observed, 10)

  ## FICALB nest I42 in 1998
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "FICALB" &
                        BreedingSeason == 1998)$FemaleID, NA_character_)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "FICALB" &
                        BreedingSeason == 1998)$MaleID, NA_character_)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "FICALB" &
                        BreedingSeason == 1998)$LayDate_observed, as.Date("1998-05-30"))
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "FICALB" &
                        BreedingSeason == 1998)$HatchDate_observed, lubridate::NA_Date_)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "FICALB" &
                        BreedingSeason == 1998)$ClutchSize_observed, 4)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "FICALB" &
                        BreedingSeason == 1998)$BroodSize_observed, 3)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "FICALB" &
                        BreedingSeason == 1998)$NumberFledged_observed, 0)

  ## PARMAJ nest I42 in 1998
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "PARMAJ" &
                        BreedingSeason == 1998)$FemaleID, NA_character_)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "PARMAJ" &
                        BreedingSeason == 1998)$MaleID, NA_character_)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "PARMAJ" &
                        BreedingSeason == 1998)$LayDate_observed, lubridate::NA_Date_)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "PARMAJ" &
                        BreedingSeason == 1998)$HatchDate_observed, as.Date("1998-05-03"))
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "PARMAJ" &
                        BreedingSeason == 1998)$ClutchSize_observed, 13)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "PARMAJ" &
                        BreedingSeason == 1998)$BroodSize_observed, 12)
  expect_equal(subset(GRO_data,
                      LocationID == "I42" &
                        Species == "PARMAJ" &
                        BreedingSeason == 1998)$NumberFledged_observed, 12)

  ## CYACAE nest N02 in 2010
  expect_equal(subset(GRO_data,
                      LocationID == "N02" &
                        Species == "CYACAE" &
                        BreedingSeason == 2010)$FemaleID, "K9B8683")
  expect_equal(subset(GRO_data,
                      LocationID == "N02" &
                        Species == "CYACAE" &
                        BreedingSeason == 2010)$MaleID, "K9B8684")
  expect_equal(subset(GRO_data,
                      LocationID == "N02" &
                        Species == "CYACAE" &
                        BreedingSeason == 2010)$LayDate_observed, as.Date("2010-04-16"))
  expect_equal(subset(GRO_data,
                      LocationID == "N02" &
                        Species == "CYACAE" &
                        BreedingSeason == 2010)$HatchDate_observed, as.Date("2010-05-08"))
  expect_equal(subset(GRO_data,
                      LocationID == "N02" &
                        Species == "CYACAE" &
                        BreedingSeason == 2010)$ClutchSize_observed, 11)
  expect_equal(subset(GRO_data,
                      LocationID == "N02" &
                        Species == "CYACAE" &
                        BreedingSeason == 2010)$BroodSize_observed, 11)
  expect_equal(subset(GRO_data,
                      LocationID == "N02" &
                        Species == "CYACAE" &
                        BreedingSeason == 2010)$NumberFledged_observed, 8)

})

test_that("Capture_data returns an expected outcome...", {


  ## Take a subset of only GRO data
  GRO_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "GRO")

  ## FICALB individual LE31523 caught in two years with conflicting sex
  expect_equal(subset(GRO_data,
                      IndvID == "LE31523")$BreedingSeason, c(1999, 2002))
  expect_equal(subset(GRO_data,
                      IndvID == "LE31523")$Sex_observed, c("M", "F"))
  expect_equal(subset(GRO_data,
                      IndvID == "LE31523")$LocationID, c("I01", "S19"))


  ## PARMAJ individual KP46833 caught twice in same year
  expect_equal(unique(subset(GRO_data,
                             IndvID == "KP46833")$BreedingSeason), 1999)
  expect_equal(unique(subset(GRO_data,
                             IndvID == "KP46833")$Species), "PARMAJ")
  expect_equal(unique(subset(GRO_data,
                             IndvID == "KP46833")$Mass), NA_real_)
  expect_equal(unique(subset(GRO_data,
                             IndvID == "KP46833")$WingLength), NA_real_)
  expect_equal(unique(subset(GRO_data,
                             IndvID == "KP46833")$CaptureAlive), TRUE)
  expect_equal(unique(subset(GRO_data,
                             IndvID == "KP46833")$Sex_observed), "F")
  expect_equal(unique(subset(GRO_data,
                             IndvID == "KP46833")$LocationID), "I19")


  ## CYACAE individual K1L7504
  expect_equal(subset(GRO_data,
                      IndvID == "K1L7504")$BreedingSeason, 2015)
  expect_equal(subset(GRO_data,
                      IndvID == "K1L7504")$Sex_observed, "F")
  expect_equal(subset(GRO_data,
                      IndvID == "K1L7504")$Species, "CYACAE")
  expect_equal(subset(GRO_data,
                      IndvID == "K1L7504")$Mass, NA_real_)
  expect_equal(subset(GRO_data,
                      IndvID == "K1L7504")$WingLength, NA_real_)
  expect_equal(subset(GRO_data,
                      IndvID == "K1L7504")$CaptureAlive, TRUE)
  expect_equal(subset(GRO_data,
                      IndvID == "K1L7504")$LocationID, "H63")


  ## FICALB individual LE31496
  expect_equal(subset(GRO_data,
                      IndvID == "LE31496")$BreedingSeason, 2002)
  expect_equal(subset(GRO_data,
                      IndvID == "LE31496")$Sex_observed, "F")
  expect_equal(subset(GRO_data,
                      IndvID == "LE31496")$Species, "FICALB")
  expect_equal(subset(GRO_data,
                      IndvID == "LE31496")$Mass, NA_real_)
  expect_equal(subset(GRO_data,
                      IndvID == "LE31496")$WingLength, NA_real_)
  expect_equal(subset(GRO_data,
                      IndvID == "LE31496")$CaptureAlive, TRUE)
  expect_equal(subset(GRO_data,
                      IndvID == "LE31496")$LocationID, "I37")
  expect_equal(subset(GRO_data,
                      IndvID == "LE31496")$CaptureDate, as.Date("2002-05-28"))

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only GRO data
  GRO_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "GRO")

  # All should have the same latitude equal to 50.06
  expect_equal(unique(subset(GRO_data)$Latitude), 50.06)

  # All should have the same longitude equal to 20.25
  expect_equal(unique(subset(GRO_data)$Longitude), 20.25)

  # StartSeason for nestbox C12 is 1997
  expect_equal(subset(GRO_data, LocationID == "C12")$StartSeason, 1997)

  # StartSeason for nestbox C12 is 1999 (nest box occurs in FICALB and PARMAJ data)
  expect_equal(subset(GRO_data, LocationID == "I04")$StartSeason, 1999)



})

