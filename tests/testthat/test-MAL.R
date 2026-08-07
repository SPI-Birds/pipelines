pipeline_output <- format_MAL(db = paste0(data_path, "/MAL_Malmo_Sweden"))

test_that("MAL outputs all files...", {
  expect_true("MAL" %in% pipeline_output$Brood_data$PopID)
  expect_true("MAL" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("MAL" %in% pipeline_output$Individual_data$PopID)
  expect_true("MAL" %in% pipeline_output$Location_data$PopID)
  expect_true(pipeline_output$protocol_version == "1.1.0")
})

test_that("Individual_data returns an expected outcome...", {
  MAL_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "MAL")

  # Adult great tit female ringed 2013 (raw Sex = 'F')
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$Sex_calculated, "F")
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$Species, "PARMAJ")
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$RingAge, "adult")
  expect_equal(subset(MAL_data, IndvID == "2KS91300")$RingSeason, 2013L)

  # Adult blue tit male ringed 2013 (raw Sex = 'M')
  expect_equal(subset(MAL_data, IndvID == "1EP63904")$Sex_calculated, "M")
  expect_equal(subset(MAL_data, IndvID == "1EP63904")$Species, "CYACAE")
  expect_equal(subset(MAL_data, IndvID == "1EP63904")$RingAge, "adult")
  expect_equal(subset(MAL_data, IndvID == "1EP63904")$RingSeason, 2013L)

  # Great tit chick ringed at K25 in 2013 — BroodIDLaid must link to that brood
  expect_equal(subset(MAL_data, IndvID == "2KS92212")$RingAge, "chick")
  expect_equal(subset(MAL_data, IndvID == "2KS92212")$Species, "PARMAJ")
  expect_equal(
    subset(MAL_data, IndvID == "2KS92212")$BroodIDLaid,
    subset(pipeline_output$Brood_data, LocationID == "K25" & BreedingSeason == 2013)$BroodID
  )
})

test_that("Brood_data returns an expected outcome...", {
  MAL_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "MAL")

  # First clutch PARMAJ at K25 in 2013
  expect_equal(subset(MAL_data, LocationID == "K25" & BreedingSeason == 2013)$Species, "PARMAJ")
  expect_equal(subset(MAL_data, LocationID == "K25" & BreedingSeason == 2013)$ClutchType_calculated, "first")
  expect_equal(subset(MAL_data, LocationID == "K25" & BreedingSeason == 2013)$LayDate_observed, as.Date("2013-05-06"))
  expect_equal(subset(MAL_data, LocationID == "K25" & BreedingSeason == 2013)$HatchDate_observed, as.Date("2013-05-26"))
  expect_equal(subset(MAL_data, LocationID == "K25" & BreedingSeason == 2013)$ClutchSize_observed, 8L)
  expect_equal(subset(MAL_data, LocationID == "K25" & BreedingSeason == 2013)$NumberFledged_observed, 7L)

  # Failed brood at P12 in 2017: _max must be NA (protocol: _max cannot be 0)
  expect_equal(subset(MAL_data, LocationID == "P12" & BreedingSeason == 2017)$ClutchSize_observed, 8L)
  expect_equal(subset(MAL_data, LocationID == "P12" & BreedingSeason == 2017)$BroodSize_observed, 0L)
  expect_equal(subset(MAL_data, LocationID == "P12" & BreedingSeason == 2017)$NumberFledged_observed, 0L)
  expect_true(is.na(subset(MAL_data, LocationID == "P12" & BreedingSeason == 2017)$NumberFledged_max))
})

test_that("Capture_data returns an expected outcome...", {
  MAL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "MAL")

  # Individual with 4 captures across multiple years
  expect_equal(nrow(subset(MAL_data, IndvID == "2KS91300")), 4L)
  expect_equal(min(subset(MAL_data, IndvID == "2KS91300")$CaptureDate, na.rm = TRUE), as.Date("2013-06-01"))
  expect_equal(unique(subset(MAL_data, IndvID == "2KS91300")$Sex_observed), "F")
  expect_equal(unique(subset(MAL_data, IndvID == "2KS91300")$Species), "PARMAJ")
  expect_equal(unique(subset(MAL_data, IndvID == "2KS91300")$CaptureAlive), TRUE)

  # Individual with a single capture
  expect_equal(nrow(subset(MAL_data, IndvID == "1EP63904")), 1L)
  expect_equal(subset(MAL_data, IndvID == "1EP63904")$CaptureDate, as.Date("2013-06-02"))
  expect_equal(subset(MAL_data, IndvID == "1EP63904")$Sex_observed, "M")
  expect_equal(subset(MAL_data, IndvID == "1EP63904")$Species, "CYACAE")
  expect_equal(subset(MAL_data, IndvID == "1EP63904")$CaptureAlive, TRUE)
})

test_that("Location_data returns an expected outcome...", {
  MAL_data <- dplyr::filter(pipeline_output$Location_data, PopID == "MAL")

  # Nestbox K1
  expect_equal(subset(MAL_data, LocationID == "K1")$LocationType, "NB")
  expect_equal(subset(MAL_data, LocationID == "K1")$NestboxID, "K1")
  expect_equal(subset(MAL_data, LocationID == "K1")$StartSeason, 2017L)
  expect_true(is.na(subset(MAL_data, LocationID == "K1")$EndSeason))
  expect_equal(subset(MAL_data, LocationID == "K1")$Latitude, 55.602979)
  expect_equal(subset(MAL_data, LocationID == "K1")$Longitude, 12.989224)
})

## Test protocol compliance
test_protocol_compliance(pipeline_output)
