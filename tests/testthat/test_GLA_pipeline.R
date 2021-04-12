context("Run data quality check on Glasgow, Scotland, pipeline output")

test_that("GLA outputs all files...", {

  expect_true("GLA" %in% pipeline_output$Brood_data$PopID)
  expect_true("GLA" %in% pipeline_output$Capture_data$CapturePopID)
  expect_true("GLA" %in% pipeline_output$Individual_data$PopID)
  expect_true("GLA" %in% pipeline_output$Location_data$PopID)

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only GLA data
  GLA_data <- dplyr::filter(pipeline_output$Individual_data, PopID == "")

  #Individual ACJ2064
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$Sex_calculated, "F") # Should be female
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$Species, "CYACAE") # Should be a blue tit
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$BroodIDLaid, 203)
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$BroodIDFledged, NA_character_)
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$RingSeason, 2020)
  expect_equal(subset(GLA_data, IndvID == "ACJ2064")$RingAge, "adult")



})

test_that("Brood_data returns an expected outcome...", {


})

test_that("Capture_data returns an expected outcome...", {



})

test_that("Location_data returns an expected outcome...", {


})

