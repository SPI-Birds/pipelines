context("Run data quality check on Can Cata, Spain pipeline output")

test_that("CAC outputs all files...", {

  expect_true(all("CAC" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("CAC" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("CAC" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("CAC" %in% pipeline_output$Location_data$PopID))

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only CAC data
  CAC_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "CAC")

  ## Great tit female
  expect_equal(subset(CAC_data, IndvID == "2649419")$Sex_calculated, "F")
  expect_equal(subset(CAC_data, IndvID == "2649419")$Sex_observed, "F")
  expect_equal(subset(CAC_data, IndvID == "2649419")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "2649419")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "2649419")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2649419")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2649419")$RingSeason, 2001)

  # Great tit male
  expect_equal(subset(CAC_data, IndvID == "L743224")$Sex_calculated, "M")
  expect_equal(subset(CAC_data, IndvID == "2649419")$Sex_observed, "M")
  expect_equal(subset(CAC_data, IndvID == "L743224")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "L743224")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "L743224")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "L743224")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "L743224")$RingSeason, 2013)

  # Grea tit male captured in many years
  expect_equal(subset(CAC_data, IndvID == "2A001084")$Sex_calculated, "M")
  expect_equal(subset(CAC_data, IndvID == "2A001084")$Sex_observed, "M")
  expect_equal(subset(CAC_data, IndvID == "2A001084")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "2A001084")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "2A001084")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2A001084")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2A001084")$RingSeason, 2012)


})

test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only CAC data
  CAC_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "CAC")

  ## Nest 111 in 2013
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 2013)$FemaleID, NA_character_)
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 1999)$MaleID, NA_character_)
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 1999)$LayDate_observed, as.Date("2013-04-13"))
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 1999)$HatchDate_observed, lubridate::NA_Date_)
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 1999)$ClutchSize_observed, 7)
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 1999)$BroodSize_observed, 12)
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 1999)$NumberFledged_observed, 0)

  ## Nest 34 in 2007
  expect_equal(subset(CAC_data,
                      LocationID == "34" &
                        BreedingSeason == 2007)$FemaleID, "L909875")
  expect_equal(subset(CAC_data,
                      LocationID == "34" &
                        BreedingSeason == 2007)$MaleID, "L950391")
  expect_equal(subset(CAC_data,
                      LocationID == "34" &
                        BreedingSeason == 2007)$LayDate_observed, as.Date("2007-04-10"))
  expect_equal(subset(CAC_data,
                      LocationID == "34" &
                        BreedingSeason == 2007)$HatchDate_observed, as.Date("2007-05-02"))
  expect_equal(subset(CAC_data,
                      LocationID == "34" &
                        BreedingSeason == 2007)$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(subset(CAC_data,
                      LocationID == "34" &
                        BreedingSeason == 2007)$ClutchSize_observed, 9)
  expect_equal(subset(CAC_data,
                      LocationID == "34" &
                        BreedingSeason == 2007)$BroodSize_observed, NA_integer_)
  expect_equal(subset(CAC_data,
                      LocationID == "34" &
                        BreedingSeason == 2007)$NumberFledged_observed, 4)

  ## Nest 54 in 2010, second clutch
  expect_equal(subset(CAC_data,
                      LocationID == "54" &
                        BreedingSeason == 2010)$FemaleID, "N931841")
  expect_equal(subset(CAC_data,
                      LocationID == "54" &
                        BreedingSeason == 2010)$MaleID, "L743204")
  expect_equal(subset(CAC_data,
                      LocationID == "54" &
                        BreedingSeason == 2010)$LayDate_observed, as.Date("2010-05-24"))
  expect_equal(subset(CAC_data,
                      LocationID == "54" &
                        BreedingSeason == 2010)$HatchDate_observed, as.Date("2010-05-27"))
  expect_equal(subset(CAC_data,
                      LocationID == "54" &
                        BreedingSeason == 2010)$FledgeDate_observed, lubridate::NA_Date_)
  expect_equal(subset(CAC_data,
                      LocationID == "54" &
                        BreedingSeason == 2010)$ClutchSize_observed, 5)
  expect_equal(subset(CAC_data,
                      LocationID == "54" &
                        BreedingSeason == 2010)$BroodSize_observed, NA_integer_)
  expect_equal(subset(CAC_data,
                      LocationID == "54" &
                        BreedingSeason == 2010)$NumberFledged_observed, 5)

})

test_that("Capture_data returns an expected outcome...", {


  ## Take a subset of only CAC data
  CAC_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "CAC")

  ## FICALB individual LE31523 caught in two years with conflicting sex
  expect_equal(subset(CAC_data,
                      IndvID == "LE31523")$BreedingSeason, c(1999, 2002))
  expect_equal(subset(CAC_data,
                      IndvID == "LE31523")$Sex_observed, c("M", "F"))
  expect_equal(subset(CAC_data,
                      IndvID == "LE31523")$LocationID, c("I01", "S19"))


  ## PARMAJ individual KP46833 caught twice in same year
  expect_equal(unique(subset(CAC_data,
                             IndvID == "KP46833")$BreedingSeason), 1999)
  expect_equal(unique(subset(CAC_data,
                             IndvID == "KP46833")$Species), "PARMAJ")
  expect_equal(unique(subset(CAC_data,
                             IndvID == "KP46833")$Mass), NA_real_)
  expect_equal(unique(subset(CAC_data,
                             IndvID == "KP46833")$WingLength), NA_real_)
  expect_equal(unique(subset(CAC_data,
                             IndvID == "KP46833")$CaptureAlive), TRUE)
  expect_equal(unique(subset(CAC_data,
                             IndvID == "KP46833")$Sex_observed), "F")
  expect_equal(unique(subset(CAC_data,
                             IndvID == "KP46833")$LocationID), "I19")


  ## CYACAE individual K1L7504
  expect_equal(subset(CAC_data,
                      IndvID == "K1L7504")$BreedingSeason, 2015)
  expect_equal(subset(CAC_data,
                      IndvID == "K1L7504")$Sex_observed, "F")
  expect_equal(subset(CAC_data,
                      IndvID == "K1L7504")$Species, "CYACAE")
  expect_equal(subset(CAC_data,
                      IndvID == "K1L7504")$Mass, NA_real_)
  expect_equal(subset(CAC_data,
                      IndvID == "K1L7504")$WingLength, NA_real_)
  expect_equal(subset(CAC_data,
                      IndvID == "K1L7504")$CaptureAlive, TRUE)
  expect_equal(subset(CAC_data,
                      IndvID == "K1L7504")$LocationID, "H63")


  ## FICALB individual LE31496
  expect_equal(subset(CAC_data,
                      IndvID == "LE31496")$BreedingSeason, 2002)
  expect_equal(subset(CAC_data,
                      IndvID == "LE31496")$Sex_observed, "F")
  expect_equal(subset(CAC_data,
                      IndvID == "LE31496")$Species, "FICALB")
  expect_equal(subset(CAC_data,
                      IndvID == "LE31496")$Mass, NA_real_)
  expect_equal(subset(CAC_data,
                      IndvID == "LE31496")$WingLength, NA_real_)
  expect_equal(subset(CAC_data,
                      IndvID == "LE31496")$CaptureAlive, TRUE)
  expect_equal(subset(CAC_data,
                      IndvID == "LE31496")$LocationID, "I37")
  expect_equal(subset(CAC_data,
                      IndvID == "LE31496")$CaptureDate, as.Date("2002-04-28"))

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only CAC data
  CAC_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "CAC")

  # All should have the same latitude equal to 50.06
  expect_equal(unique(subset(CAC_data)$Latitude), 50.06)

  # All should have the same longitude equal to 20.25
  expect_equal(unique(subset(CAC_data)$Longitude), 20.25)

  # StartSeason for nestbox C12 is 1997
  expect_equal(subset(CAC_data, LocationID == "C12")$StartSeason, 1997)

  # StartSeason for nestbox C12 is 1999 (nest box occurs in FICALB and PARMAJ data)
  expect_equal(subset(CAC_data, LocationID == "I04")$StartSeason, 1999)



})

