pipeline_output <- format_ZER(db = paste0(data_path, "/ZER_ZeromskiPark_Poland"))

test_that("ZER outputs all files...", {
  # This pipeline covers two sites: Zeromski Park (ZER) and Kowale (KOW)
  expect_true(all(c("ZER", "KOW") %in% pipeline_output$Brood_data$PopID))
  expect_true(all(c("ZER", "KOW") %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all(c("ZER", "KOW") %in% pipeline_output$Individual_data$PopID))
  expect_true(all(c("ZER", "KOW") %in% pipeline_output$Location_data$PopID))
  expect_true(pipeline_output$protocol_version == "1.1.0")

  # Single-species study: only Common Blackbird
  expect_equal(unique(pipeline_output$Brood_data$Species), "TURMER")
  # No clutch-type legend received, so ClutchType_observed is NA throughout
  expect_true(all(is.na(pipeline_output$Brood_data$ClutchType_observed)))
})

test_that("Brood_data returns an expected outcome...", {
  ZER_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% c("ZER", "KOW"))

  # Test 1: First clutch at ZER with both parents known
  expect_equal(subset(ZER_data, BroodID == "1998-10")$PopID, "ZER")
  expect_equal(subset(ZER_data, BroodID == "1998-10")$Species, "TURMER")
  expect_equal(subset(ZER_data, BroodID == "1998-10")$LocationID, "1998-4")
  expect_equal(subset(ZER_data, BroodID == "1998-10")$FemaleID, "HC92428")
  expect_equal(subset(ZER_data, BroodID == "1998-10")$MaleID, "HC99418")
  expect_equal(subset(ZER_data, BroodID == "1998-10")$ClutchType_calculated, "first")
  expect_equal(subset(ZER_data, BroodID == "1998-10")$LayDate_observed, as.Date("1998-04-20"))
  expect_equal(subset(ZER_data, BroodID == "1998-10")$ClutchSize_observed, 5L)
  expect_equal(subset(ZER_data, BroodID == "1998-10")$HatchDate_observed, as.Date("1998-05-06"))
  expect_equal(subset(ZER_data, BroodID == "1998-10")$BroodSize_observed, 5L)
  expect_equal(subset(ZER_data, BroodID == "1998-10")$NumberFledged_observed, 5L)

  # Test 2: Second clutch (calculated), same season
  expect_equal(subset(ZER_data, BroodID == "1998-9,1")$ClutchType_calculated, "second")
  expect_equal(subset(ZER_data, BroodID == "1998-9,1")$FemaleID, "HC92520")
  expect_equal(subset(ZER_data, BroodID == "1998-9,1")$LayDate_observed, as.Date("1998-05-27"))
  expect_equal(subset(ZER_data, BroodID == "1998-9,1")$ClutchSize_observed, 3L)

  # Test 3: Failed brood - 0 fledglings is stored as NA (protocol: min value is 1),
  # and the _max value must also be NA (cannot be 0)
  expect_equal(subset(ZER_data, BroodID == "1998-05")$ClutchSize_observed, 4L)
  expect_equal(subset(ZER_data, BroodID == "1998-05")$BroodSize_observed, 3L)
  expect_true(is.na(subset(ZER_data, BroodID == "1998-05")$NumberFledged_observed))
  expect_true(is.na(subset(ZER_data, BroodID == "1998-05")$NumberFledged_min))
  expect_true(is.na(subset(ZER_data, BroodID == "1998-05")$NumberFledged_max))

  # Test 4: A brood at the second site (Kowale)
  expect_equal(subset(ZER_data, BroodID == "1998-13-1k")$PopID, "KOW")
  expect_equal(subset(ZER_data, BroodID == "1998-13-1k")$LayDate_observed, as.Date("1998-05-04"))
  expect_equal(subset(ZER_data, BroodID == "1998-13-1k")$ClutchSize_observed, 5L)
})

test_that("Individual data returns an expected outcome...", {
  ZER_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% c("ZER", "KOW"))

  # Test 1: Adult female (parent of brood 1998-10), first caught earlier in 1996
  expect_equal(subset(ZER_data, IndvID == "HC92428")$Species, "TURMER")
  expect_equal(subset(ZER_data, IndvID == "HC92428")$PopID, "ZER")
  expect_equal(subset(ZER_data, IndvID == "HC92428")$Sex_calculated, "F")
  expect_equal(subset(ZER_data, IndvID == "HC92428")$RingSeason, 1996L)
  expect_equal(subset(ZER_data, IndvID == "HC92428")$RingAge, "adult")
  expect_equal(subset(ZER_data, IndvID == "HC92428")$BroodIDLaid, NA_character_)

  # Test 2: Ringed as a chick, recruited as a breeding adult (male)
  expect_equal(subset(ZER_data, IndvID == "HE83515")$Species, "TURMER")
  expect_equal(subset(ZER_data, IndvID == "HE83515")$Sex_calculated, "M")
  expect_equal(subset(ZER_data, IndvID == "HE83515")$RingSeason, 2012L)
  expect_equal(subset(ZER_data, IndvID == "HE83515")$RingAge, "chick")
  # BroodIDFledged is assumed equal to BroodIDLaid (blackbirds don't move before fledging)
  expect_equal(subset(ZER_data, IndvID == "HE83515")$BroodIDLaid, "2012-40")
  expect_equal(subset(ZER_data, IndvID == "HE83515")$BroodIDFledged, "2012-40")
})

test_that("Capture data returns an expected outcome...", {
  ZER_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% c("ZER", "KOW"))

  # No mortality info is recorded, so every capture is alive on capture and release
  expect_true(all(ZER_data$CaptureAlive))
  expect_true(all(ZER_data$ReleaseAlive))

  # Test 1: Chick recruit - ringed as chick in 2012, recaptured as adult in 2013
  expect_equal(nrow(subset(ZER_data, IndvID == "HE83515")), 2)
  expect_equal(subset(ZER_data, IndvID == "HE83515")$CaptureDate[1], as.Date("2012-07-21"))
  expect_equal(subset(ZER_data, IndvID == "HE83515")$CaptureDate[2], as.Date("2013-11-23"))
  # First capture is a chick (age observed 1) with a recorded chick age
  expect_equal(subset(ZER_data, IndvID == "HE83515")$Age_observed[1], 1L)
  expect_equal(subset(ZER_data, IndvID == "HE83515")$ChickAge[1], 5L)
  # Age calculated: 1 (chick) then 5 (at least one year old)
  expect_equal(subset(ZER_data, IndvID == "HE83515")$Age_calculated, c(1L, 5L))

  # Test 2: Adult female, single capture; age not observed but calculated as adult (4)
  expect_equal(nrow(subset(ZER_data, IndvID == "HC92428")), 1)
  expect_equal(subset(ZER_data, IndvID == "HC92428")$Sex_observed, "F")
  expect_equal(subset(ZER_data, IndvID == "HC92428")$CaptureDate, as.Date("1996-05-21"))
  expect_equal(subset(ZER_data, IndvID == "HC92428")$Age_observed, NA_integer_)
  expect_equal(subset(ZER_data, IndvID == "HC92428")$Age_calculated, 4L)
})

test_that("Location_data returns an expected outcome...", {
  ZER_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% c("ZER", "KOW"))

  # All captures assumed to be by mist net
  expect_true(all(ZER_data$LocationType == "MN"))

  # Test 1: A ZER nest location, carrying the ZER site-level coordinates
  expect_equal(subset(ZER_data, LocationID == "1998-4")$PopID, "ZER")
  expect_equal(subset(ZER_data, LocationID == "1998-4")$LocationType, "MN")
  expect_equal(subset(ZER_data, LocationID == "1998-4")$Latitude, 53.260)
  expect_equal(subset(ZER_data, LocationID == "1998-4")$Longitude, 14.3342)

  # Test 2: A KOW nest location, carrying the KOW site-level coordinates
  expect_equal(subset(ZER_data, LocationID == "1998-7")$PopID, "KOW")
  expect_equal(subset(ZER_data, LocationID == "1998-7")$Latitude, 53.273)
  expect_equal(subset(ZER_data, LocationID == "1998-7")$Longitude, 14.3250)
})

test_that("pop argument restricts output to a single site...", {
  # Requesting only ZER should drop all KOW records
  zer_only <- format_ZER(db = paste0(data_path, "/ZER_ZeromskiPark_Poland"), pop = "ZER")
  expect_equal(unique(zer_only$Brood_data$PopID), "ZER")
  expect_equal(unique(zer_only$Capture_data$CapturePopID), "ZER")
  expect_false("KOW" %in% zer_only$Location_data$PopID)
})

### Test protocol compliance
test_protocol_compliance(pipeline_output)
