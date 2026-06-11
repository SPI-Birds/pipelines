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

  # Test 1: Adult great tit female
  # TODO: Replace IndvID with a known female PARMAJ from the primary data
  # expect_equal(subset(MAL_data, IndvID == "???")$Sex_calculated, "F")
  # expect_equal(subset(MAL_data, IndvID == "???")$Species, "PARMAJ")
  # expect_equal(subset(MAL_data, IndvID == "???")$RingAge, "adult")
  # expect_equal(subset(MAL_data, IndvID == "???")$RingSeason, ???)

  # Test 2: Adult great tit male
  # TODO: Replace IndvID with a known male PARMAJ from the primary data
  # expect_equal(subset(MAL_data, IndvID == "???")$Sex_calculated, "M")
  # expect_equal(subset(MAL_data, IndvID == "???")$Species, "PARMAJ")

  # Test 3: Adult blue tit female
  # TODO: Replace IndvID with a known female CYACAE from the primary data
  # expect_equal(subset(MAL_data, IndvID == "???")$Sex_calculated, "F")
  # expect_equal(subset(MAL_data, IndvID == "???")$Species, "CYACAE")

  # Test 4: Adult blue tit male
  # TODO: Replace IndvID with a known male CYACAE from the primary data
  # expect_equal(subset(MAL_data, IndvID == "???")$Sex_calculated, "M")
  # expect_equal(subset(MAL_data, IndvID == "???")$Species, "CYACAE")

  # Test 5: Individual ringed as chick — check BroodIDLaid is filled
  # TODO: Replace IndvID with a known chick from the primary data
  # expect_equal(subset(MAL_data, IndvID == "???")$RingAge, "chick")
  # expect_false(is.na(subset(MAL_data, IndvID == "???")$BroodIDLaid))
})

test_that("Brood_data returns an expected outcome...", {
  MAL_data <- dplyr::filter(pipeline_output$Brood_data, PopID == "MAL")

  # Test 1: First clutch great tit
  # TODO: Replace BroodID with a known first-clutch PARMAJ brood
  # expect_equal(subset(MAL_data, BroodID == "MAL-???")$Species, "PARMAJ")
  # expect_equal(subset(MAL_data, BroodID == "MAL-???")$ClutchType_calculated, "first")
  # expect_equal(subset(MAL_data, BroodID == "MAL-???")$LayDate_observed, as.Date("????-??-??"))
  # expect_equal(subset(MAL_data, BroodID == "MAL-???")$ClutchSize_observed, ???L)
  # expect_equal(subset(MAL_data, BroodID == "MAL-???")$BroodSize_observed, ???L)
  # expect_equal(subset(MAL_data, BroodID == "MAL-???")$NumberFledged_observed, ???L)

  # Test 2: Second clutch or replacement
  # TODO: Replace BroodID with a known second/replacement clutch
  # expect_equal(subset(MAL_data, BroodID == "MAL-???")$ClutchType_calculated, "second")
})

test_that("Capture_data returns an expected outcome...", {
  MAL_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID == "MAL")

  # Test 1: Individual with multiple captures — check capture count and dates
  # TODO: Replace IndvID with a known individual captured multiple times
  # expect_equal(nrow(subset(MAL_data, IndvID == "???")), ???)
  # expect_equal(min(subset(MAL_data, IndvID == "???")$CaptureDate, na.rm = TRUE), as.Date("????-??-??"))

  # Test 2: Check Age_calculated increments correctly across years
  # TODO: Replace IndvID with a known adult with captures in multiple years
  # expect_equal(subset(MAL_data, IndvID == "???")$Age_calculated[1], 4L)
})

test_that("Location_data returns an expected outcome...", {
  MAL_data <- dplyr::filter(pipeline_output$Location_data, PopID == "MAL")

  # Test 1: Nestbox check
  # TODO: Replace LocationID with a known nestbox from the primary data
  # expect_equal(subset(MAL_data, LocationID == "???")$LocationType, "NB")
  # expect_equal(subset(MAL_data, LocationID == "???")$LocationID,
  #              subset(MAL_data, LocationID == "???")$NestboxID)
  # expect_equal(subset(MAL_data, LocationID == "???")$StartSeason, ???L)
})

## Test protocol compliance
test_protocol_compliance(pipeline_output)
