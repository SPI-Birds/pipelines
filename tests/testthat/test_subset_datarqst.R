context("Test that subset_datarqst() functions as expected...")

test_PopIDs <- c('CHO', 'BAN', 'VEL', 'SSQ', 'HOC', 'PIL', 'EDM')
test_data <- run_pipelines(PopID = test_PopIDs, save = FALSE)

request_PopIDs <- c('CHO', 'HOC', 'EDM')
request_Species <- c('PARMAJ', 'FICHYP')
request_filter <- paste(expand.grid(request_PopIDs, request_Species)[,1], expand.grid(request_PopIDs, request_Species)[,2], sep = '_')

request_data <- subset_datarqst(PopID = request_PopIDs, Species = request_Species, test = TRUE)


test_that("All 4 tables are retained...", {

  expect_true(exists("Brood_data", where = request_data))
  expect_true(exists("Capture_data", where = request_data))
  expect_true(exists("Individual_data", where = request_data))
  expect_true(exists("Location_data", where = request_data))

})


test_that("All tables contain correct columns..."){
  expect_identical(colnames(request_data$Brood_data), colnames(test_data$Brood_data))
  expect_identical(colnames(request_data$Capture_data), colnames(test_data$Capture_data))
  expect_identical(colnames(request_data$Individual_data), colnames(test_data$Individual_data))
  expect_identical(colnames(request_data$Location_data), colnames(test_data$Location_data))
}


test_that("Subset output is identical when 'PopID' and 'Species' or 'filter' is used..."){

  request_data_filter <- subset_datarqst(filter = request_filter, test = TRUE)

  expect_identical(request_data$Brood_data, request_data_filter$Brood_data)
  expect_identical(request_data$Capture_data, request_data_filter$Capture_data)
  expect_identical(request_data$Individual_data, request_data_filter$Individual_data)
  expect_identical(request_data$Location_data, request_data_filter$Location_data)

}


