context("Run data quality check on Montpellier pipeline output")

test_that("MON outputs all files...", {

  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("MUR", "PIR", "ROU", "MON", "MTV", "MIS") %in% pipeline_output$Location_data$PopID))

})

test_that("Brood_data returns an expected outcome...", {

  #We want to run tests for all possible outcomes of ClutchType_calculated

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Brood where clutch type = first
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$Species, "CYACAE")
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$ClutchType_calculated, "first")
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$LayDate, as.Date("1984-05-02"))
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$ClutchSize, 5)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$BroodSize, 4)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$NumberFledged, 0)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1984_pir_35_1")$AvgTarsus, NA_real_)

  #Test 2: Brood where clutch type = replacement (because first is known to have failed)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$Species, "CYACAE")
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$ClutchType_calculated, "replacement")
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$LayDate, as.Date("1980-05-30"))
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$ClutchSize, 5)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$BroodSize, 5)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$NumberFledged, 5)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1980_pir_36_3")$AvgTarsus, NA_real_)

  #Test 3: Brood where clutch type = replacement (past the cutoff)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$Species, "CYACAE")
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$ClutchType_calculated, "replacement")
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$LayDate, as.Date("1984-06-10"))
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$ClutchSize, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$BroodSize, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$NumberFledged, 7)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1984_pir_47_3")$AvgTarsus, NA_real_)

  #Test 4: Brood where clutch type = second
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$Species, "PARMAJ")
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$ClutchType_calculated, "second")
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$LayDate, as.Date("1992-05-20"))
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$ClutchSize, 8)
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$BroodSize, 2)
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$NumberFledged, 1)
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$AvgChickMass, NA_real_)
  expect_equal(subset(MON_data, BroodID == "1992_rou_90_2")$AvgTarsus, NA_real_)

})

test_that("Individual data returns an expected outcome...", {

  #We want to run a test for each sex for individuals caught as adults and chicks

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Male caught first as adult
  #Individual C044309 should be listed as a male coal tit
  expect_equal(subset(MON_data, IndvID == "2221101")$Sex, "M")
  expect_equal(subset(MON_data, IndvID == "2221101")$Species, "PERATE")
  #They should have no BroodIDLaid or Fledged because she was never caught as a chick
  expect_equal(subset(MON_data, IndvID == "2221101")$BroodIDLaid, NA_character_)
  expect_equal(subset(MON_data, IndvID == "2221101")$BroodIDFledged, NA_character_)
  #Her ring season should be 1991 with a RingAge of 'adult'
  expect_equal(subset(MON_data, IndvID == "2221101")$RingSeason, 1991)
  expect_equal(subset(MON_data, IndvID == "2221101")$RingAge, "adult")

  #Test 2: Female caught first as adult
  #Individual 2546616 should be listed as a female blue tit
  expect_equal(subset(MON_data, IndvID == "2546616")$Sex, "F")
  expect_equal(subset(MON_data, IndvID == "2546616")$Species, "CYACAE")
  #She should have no BroodIDLaid or Fledged because this individual was caught as an adult
  expect_equal(subset(MON_data, IndvID == "2546616")$BroodIDLaid, NA_character_)
  expect_equal(subset(MON_data, IndvID == "2546616")$BroodIDFledged, NA_character_)
  #Her ring season should be 2003 with a RingAge of 'chick'
  expect_equal(subset(MON_data, IndvID == "2546616")$RingSeason, 1981)
  expect_equal(subset(MON_data, IndvID == "2546616")$RingAge, "adult")

  #Test 3: Caught as chick not cross-fostered
  #Individual 8189538 should be listed as a female great tit
  expect_equal(subset(MON_data, IndvID == "8189538")$Sex, "F")
  expect_equal(subset(MON_data, IndvID == "8189538")$Species, "PARMAJ")
  #Check that BroodIDLaid/Fledged are as expected
  #This individual was not cross-fostered, so they should be the same
  expect_equal(subset(MON_data, IndvID == "8189538")$BroodIDLaid, "2017_rou_28_2")
  expect_equal(subset(MON_data, IndvID == "8189538")$BroodIDFledged, "2017_rou_28_2")
  #Ring season should be 2017 with a RingAge of 'chick'
  expect_equal(subset(MON_data, IndvID == "8189538")$RingSeason, 2017)
  expect_equal(subset(MON_data, IndvID == "8189538")$RingAge, "chick")

  #Test 4: Caught as chick and cross-fostered
  #Check sex and species are as expected
  #Sex should be NA because it was never caught as an adult
  expect_equal(subset(MON_data, IndvID == "2221172")$Sex, NA_character_)
  expect_equal(subset(MON_data, IndvID == "2221172")$Species, "CYACAE")
  #Check that BroodIDLaid/Fledged are as expected
  expect_equal(subset(MON_data, IndvID == "2221172")$BroodIDLaid, "1991_pir_103_1")
  expect_equal(subset(MON_data, IndvID == "2221172")$BroodIDFledged, "1991_pir_89_1")
  #RingSeason and Age are as expected
  expect_equal(subset(MON_data, IndvID == "2221172")$RingSeason, 1991)
  expect_equal(subset(MON_data, IndvID == "2221172")$RingAge, "chick")

})

test_that("Capture data returns an expected outcome...", {

  #We want to run tests for captures as both chicks, males, and females
  #Currently we have no chick data, so we can only test adults

  #Take a subset of only MON data
  MON_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("MUR", "PIR", "ROU", "MON", "MTV", "MIS"))

  #Test 1: Individual ringed as a chick
  #Test the female has the correct number of capture records
  expect_equal(nrow(subset(MON_data, IndvID == "2709339")), 6)
  #Test that the first capture of the female is as expected (01/01/1982)
  #This will return an error. The chick capture has no date so will currently be NA. We need to estimate
  #capture date from hatch date.
  expect_equal(subset(MON_data, IndvID == "2709339")$CaptureDate[1], as.Date("1982-01-01"))
  #Test that the 6th capture of the female is as expcted (1987-06-08)
  expect_equal(subset(MON_data, IndvID == "2709339")$CaptureDate[6], as.Date("1987-06-08"))
  #Test that age observed is as expected on first capture
  #Test that age observed is as expected on 6th capture
  expect_equal(subset(MON_data, IndvID == "2709339")$Age_observed[1], 1L)
  expect_equal(subset(MON_data, IndvID == "2709339")$Age_observed[6], 13L)
  #Test that age calculated is correct on first capture and last capture
  expect_equal(subset(MON_data, IndvID == "2709339")$Age_calculated[1], 1L)
  expect_equal(subset(MON_data, IndvID == "2709339")$Age_calculated[6], 13L)

  #Test 2: Female caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, IndvID == "3672638")), 16)
  #Test that the first capture is as expected
  expect_equal(subset(MON_data, IndvID == "3672638")$CaptureDate[1], as.Date("1992-01-20"))
  #Test that the 16th capture is as expected
  expect_equal(subset(MON_data, IndvID == "3672638")$CaptureDate[16], as.Date("1997-01-28"))
  #Test that first and last age observed is as expected
  expect_equal(subset(MON_data, IndvID == "3672638")$Age_observed[1], 5L)
  expect_equal(subset(MON_data, IndvID == "3672638")$Age_observed[16], 15L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(MON_data, IndvID == "3672638")$Age_calculated[1], 4L)
  expect_equal(subset(MON_data, IndvID == "3672638")$Age_calculated[16], 14L)

  #Test 3: Male caught only as adult
  #Test it has the correct number of capture records
  expect_equal(nrow(subset(MON_data, IndvID == "4208517")), 15)
  #Test that the first capture is as expected
  expect_equal(subset(MON_data, IndvID == "4208517")$CaptureDate[1], as.Date("1993-05-14"))
  #Test that the 15th capture is as expected
  expect_equal(subset(MON_data, IndvID == "4208517")$CaptureDate[15], as.Date("1998-01-27"))
  #Test that first and last age observed is as expected
  expect_equal(subset(MON_data, IndvID == "4208517")$Age_observed[1], 6L)
  expect_equal(subset(MON_data, IndvID == "4208517")$Age_observed[15], 16L)
  #Test that first and last age calculated is as expected
  expect_equal(subset(MON_data, IndvID == "4208517")$Age_calculated[1], 4L)
  expect_equal(subset(MON_data, IndvID == "4208517")$Age_calculated[15], 14L)


})
