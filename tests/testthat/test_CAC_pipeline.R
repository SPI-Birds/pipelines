context("Run data quality check on Can Cata, Spain pipeline output")

test_that("CAC outputs all files...", {

  expect_true(all("CAC" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("CAC" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("CAC" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("CAC" %in% pipeline_output$Location_data$PopID))

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
                        BreedingSeason == 2013)$MaleID, NA_character_)
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 2013)$LayDate_observed, as.Date("2013-04-21"))
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 2013)$HatchDate_observed, lubridate::NA_Date_)
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 2013)$ClutchSize_observed, 7)
  expect_equal(subset(CAC_data,
                      LocationID == "111" &
                        BreedingSeason == 2013)$NumberFledged_observed, 0)

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

  # Great tit male captured in many years
  expect_equal(subset(CAC_data, IndvID == "2A001084" &
                        BreedingSeason == 2013)$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "2A001084" &
                        BreedingSeason == 2015)$Sex_observed, "M")
  expect_equal(subset(CAC_data, IndvID == "2A001084" &
                        BreedingSeason == 2016)$LocationID, "169")
  expect_equal(subset(CAC_data, IndvID == "2A001084" &
                        BreedingSeason == 2017)$Age_calculated, c(14,14))
  expect_equal(subset(CAC_data, IndvID == "2A001084" &
                        BreedingSeason == 2017)$LocationID, c("170","170A"))
  expect_equal(subset(CAC_data, IndvID == "2A001084" &
                        BreedingSeason == 2018)$ExperimentID, c(NA_character_,NA_character_))

  # Great tit female captured in two years
  expect_equal(subset(CAC_data, IndvID == "2N78773" &
                        BreedingSeason == 2011)$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "2N78773"  &
                        BreedingSeason == 2011)$LocationID, "107")
  expect_equal(subset(CAC_data, IndvID == "2N78773" &
                        BreedingSeason == 2013)$LocationID, "103")
  expect_equal(subset(CAC_data, IndvID == "2N78773" &
                        BreedingSeason == 2013)$Age_observed, 6)
  expect_equal(subset(CAC_data, IndvID == "2N78773" &
                        BreedingSeason == 2013)$Age_calculated, 8)
  expect_equal(subset(CAC_data, IndvID == "2N78773" &
                        BreedingSeason == 2013)$ExperimentID, NA_character_)

  # Great tit female captured in one year without an age
  expect_equal(subset(CAC_data, IndvID == "2KA77226" &
                        BreedingSeason == 2020)$Age_observed, NA_integer_)
  expect_equal(subset(CAC_data, IndvID == "2KA77226"  &
                        BreedingSeason == 2020)$LocationID, "109")
  expect_equal(subset(CAC_data, IndvID == "2KA77226"  &
                        BreedingSeason == 2020)$Sex_observed, "F")

})


test_that("Individual data returns an expected outcome...", {

  #Take a subset of only CAC data
  CAC_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "CAC")

  ## Great tit female
  expect_equal(subset(CAC_data, IndvID == "2649419")$Sex_calculated, "F")
  expect_equal(subset(CAC_data, IndvID == "2649419")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "2649419")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "2649419")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2649419")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2649419")$RingSeason, 2001)

  # Great tit male
  expect_equal(subset(CAC_data, IndvID == "L743224")$Sex_calculated, "M")
  expect_equal(subset(CAC_data, IndvID == "L743224")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "L743224")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "L743224")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "L743224")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "L743224")$RingSeason, 2010)

  # Great tit male captured in many years
  expect_equal(subset(CAC_data, IndvID == "2A001084")$Sex_calculated, "M")
  expect_equal(subset(CAC_data, IndvID == "2A001084")$Species, "PARMAJ")
  expect_equal(subset(CAC_data, IndvID == "2A001084")$RingAge, "adult")
  expect_equal(subset(CAC_data, IndvID == "2A001084")$Sex_genetic, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2A001084")$BroodIDLaid, NA_character_)
  expect_equal(subset(CAC_data, IndvID == "2A001084")$RingSeason, 2012)

})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only CAC data
  CAC_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "CAC")

  # All should have the same latitude equal to 45.27
  expect_equal(unique(subset(CAC_data)$Latitude), 45.27)

  # All should have the same longitude equal to 2.8
  expect_equal(unique(subset(CAC_data)$Longitude), 2,8)

  # StartSeason for nestbox 47A is 2017
  expect_equal(subset(CAC_data, LocationID == "47A")$StartSeason, 2017)

  # StartSeason for nestbox R111 is 2000
  expect_equal(subset(CAC_data, LocationID == "R111")$StartSeason, 2000)

})

