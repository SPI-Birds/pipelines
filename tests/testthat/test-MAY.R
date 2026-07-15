pipeline_output <- format_MAY(db = paste0(data_path, "/MAY_Mayachino_Russia"))

test_that("MAY outputs all files...", {
  expect_true(all("MAY" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("MAY" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("MAY" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("MAY" %in% pipeline_output$Location_data$PopID))
  expect_true(pipeline_output$protocol_version == "1.1.0")
})

test_that("Brood_data returns an expected outcome...", {
  # Take a subset of only MAY data
  MAY_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "MAY")

  # Test 1: Great tit first clutch with both parents known
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$Species, "PARMAJ")
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$Plot, "B")
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$LocationID, "B_47B")
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$FemaleID, "XA593689")
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$MaleID, "XA593797")
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$ClutchType_calculated, "first")
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$LayDate_observed, as.Date("1979-05-29"))
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$ClutchSize_observed, 10L)
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$BroodSize_observed, 10L)
  expect_equal(subset(MAY_data, BroodID == "1979_B_47B_4")$NumberFledged_observed, 10L)

  # Test 2: Great tit second clutch (laid late in the season)
  expect_equal(subset(MAY_data, BroodID == "1980_M_82_7")$Species, "PARMAJ")
  expect_equal(subset(MAY_data, BroodID == "1980_M_82_7")$FemaleID, "XA712423")
  expect_equal(subset(MAY_data, BroodID == "1980_M_82_7")$ClutchType_observed, "second")
  expect_equal(subset(MAY_data, BroodID == "1980_M_82_7")$ClutchType_calculated, "second")
  expect_equal(subset(MAY_data, BroodID == "1980_M_82_7")$LayDate_observed, as.Date("1980-07-06"))
  expect_equal(subset(MAY_data, BroodID == "1980_M_82_7")$ClutchSize_observed, 8L)
  expect_equal(subset(MAY_data, BroodID == "1980_M_82_7")$BroodSize_observed, 7L)
  expect_equal(subset(MAY_data, BroodID == "1980_M_82_7")$NumberFledged_observed, 7L)

  # Test 3: Pied flycatcher first clutch (brood in which 530579 was laid)
  expect_equal(subset(MAY_data, BroodID == "1981_L_17_155")$Species, "FICHYP")
  expect_equal(subset(MAY_data, BroodID == "1981_L_17_155")$Plot, "L")
  expect_equal(subset(MAY_data, BroodID == "1981_L_17_155")$ClutchType_calculated, "first")
  expect_equal(subset(MAY_data, BroodID == "1981_L_17_155")$LayDate_observed, as.Date("1981-05-26"))
  expect_equal(subset(MAY_data, BroodID == "1981_L_17_155")$ClutchSize_observed, 7L)
  expect_equal(subset(MAY_data, BroodID == "1981_L_17_155")$BroodSize_observed, 7L)
  expect_equal(subset(MAY_data, BroodID == "1981_L_17_155")$NumberFledged_observed, 7L)
})

test_that("Individual data returns an expected outcome...", {
  # Take a subset of only MAY data
  MAY_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "MAY")

  # Test 1: Pied flycatcher ringed as chick, recruited as breeding adult (female)
  expect_equal(subset(MAY_data, IndvID == "530579")$Species, "FICHYP")
  expect_equal(subset(MAY_data, IndvID == "530579")$Sex_calculated, "F")
  expect_equal(subset(MAY_data, IndvID == "530579")$BroodIDLaid, "1981_L_17_155")
  expect_equal(subset(MAY_data, IndvID == "530579")$BroodIDFledged, "1981_L_17_155")
  expect_equal(subset(MAY_data, IndvID == "530579")$RingSeason, 1981L)
  expect_equal(subset(MAY_data, IndvID == "530579")$RingAge, "chick")

  # Test 2: Great tit first caught as breeding adult (female), no brood of origin
  expect_equal(subset(MAY_data, IndvID == "XA593689")$Species, "PARMAJ")
  expect_equal(subset(MAY_data, IndvID == "XA593689")$Sex_calculated, "F")
  expect_equal(subset(MAY_data, IndvID == "XA593689")$BroodIDLaid, NA_character_)
  expect_equal(subset(MAY_data, IndvID == "XA593689")$BroodIDFledged, NA_character_)
  expect_equal(subset(MAY_data, IndvID == "XA593689")$RingSeason, 1979L)
  expect_equal(subset(MAY_data, IndvID == "XA593689")$RingAge, "adult")

  # Test 3: Great tit second-clutch mother, first caught as adult
  expect_equal(subset(MAY_data, IndvID == "XA712423")$Species, "PARMAJ")
  expect_equal(subset(MAY_data, IndvID == "XA712423")$Sex_calculated, "F")
  expect_equal(subset(MAY_data, IndvID == "XA712423")$RingSeason, 1980L)
  expect_equal(subset(MAY_data, IndvID == "XA712423")$RingAge, "adult")
})

test_that("Capture data returns an expected outcome...", {
  # Take a subset of only MAY data
  MAY_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "MAY")

  # Test 1: Individual ringed as a chick, recaptured as a breeding adult 2 years later
  expect_equal(nrow(subset(MAY_data, IndvID == "530579")), 2)
  expect_equal(subset(MAY_data, IndvID == "530579")$CaptureID, c("530579_1", "530579_2"))
  expect_equal(subset(MAY_data, IndvID == "530579")$CaptureDate[1], as.Date("1981-06-02"))
  expect_equal(subset(MAY_data, IndvID == "530579")$CaptureDate[2], as.Date("1983-05-28"))
  # First capture is as a chick (age observed 1), then as an adult
  expect_equal(subset(MAY_data, IndvID == "530579")$Age_observed, c(1L, 6L))
  expect_equal(subset(MAY_data, IndvID == "530579")$Age_calculated, c(1L, 7L))

  # Test 2: Adult female caught once in her ringing season
  expect_equal(nrow(subset(MAY_data, IndvID == "XA593689")), 1)
  expect_equal(subset(MAY_data, IndvID == "XA593689")$CaptureID, "XA593689_1")
  expect_equal(subset(MAY_data, IndvID == "XA593689")$Species, "PARMAJ")
  expect_equal(subset(MAY_data, IndvID == "XA593689")$Sex_observed, "F")
  expect_equal(subset(MAY_data, IndvID == "XA593689")$CaptureDate, as.Date("1979-06-08"))
  expect_equal(subset(MAY_data, IndvID == "XA593689")$LocationID, "B_47B")
  expect_equal(subset(MAY_data, IndvID == "XA593689")$Age_observed, 5L)
})

test_that("Location_data returns an expected outcome...", {
  # Take a subset of only MAY data
  MAY_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "MAY")

  # Test 1: Nestbox check
  expect_true(subset(MAY_data, LocationID == "L_17")$LocationType == "NB")
  # Expect LocationID and NestboxID are the same
  expect_true(subset(MAY_data, LocationID == "L_17")$NestboxID == "L_17")
  expect_equal(subset(MAY_data, LocationID == "L_17")$PopID, "MAY")
  expect_equal(subset(MAY_data, LocationID == "L_17")$StartSeason, 1979L)

  # Test 2: A brood LocationID resolves in Location_data
  expect_true("B_47B" %in% pipeline_output$Location_data$LocationID)
  expect_true(subset(MAY_data, LocationID == "B_47B")$LocationType == "NB")

  # NOTE: coordinates and habitat type are not yet available in the primary data,
  # so they are currently NA for all boxes (pending confirmation with the data owner).
  expect_true(all(is.na(MAY_data$Latitude)))
  expect_true(all(is.na(MAY_data$HabitatType)))
})

### Test protocol compliance
test_protocol_compliance(pipeline_output)
