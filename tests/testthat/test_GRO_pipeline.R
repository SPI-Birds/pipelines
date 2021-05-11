context("Run data quality check on Grobla, Poland, pipeline output")

test_that("GRO outputs all files...", {

  expect_true(all("GRO" %in% pipeline_output$Brood_data$PopID))
  expect_true(all("GRO" %in% pipeline_output$Capture_data$CapturePopID))
  expect_true(all("GRO" %in% pipeline_output$Individual_data$PopID))
  expect_true(all("GRO" %in% pipeline_output$Location_data$PopID))

})

test_that("Individual data returns an expected outcome...", {

  #Take a subset of only GRO data
  GRO_data <- dplyr::filter(pipeline_output$Individual_data, PopID %in% "GRO")




})

test_that("Brood_data returns an expected outcome...", {

  ## Take a subset of only GRO data
  GRO_data <- dplyr::filter(pipeline_output$Brood_data, PopID %in% "GRO")




})

test_that("Capture_data returns an expected outcome...", {


  #Take a subset of only GRO data
  GRO_data <- dplyr::filter(pipeline_output$Capture_data, CapturePopID %in% "GRO")



})

test_that("Location_data returns an expected outcome...", {

  #Take a subset of only GRO data
  GRO_data <- dplyr::filter(pipeline_output$Location_data, PopID %in% "GRO")


})

