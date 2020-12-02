context("Test that subset_datarqst() functions as expected...")

request_PopIDs <- c('CHO', 'HOC', 'EDM', 'PIL')
request_Species <- c('PARMAJ', 'FICHYP')
request_PopSpec <- paste(expand.grid(request_PopIDs, request_Species)[,1], expand.grid(request_PopIDs, request_Species)[,2], sep = '_')

request_data <- subset_datarqst(PopID = request_PopIDs, Species = request_Species,
                                test = TRUE, save = FALSE)


test_that("All 4 tables are retained...", {

  expect_true(exists("Brood_data", where = request_data))
  expect_true(exists("Capture_data", where = request_data))
  expect_true(exists("Individual_data", where = request_data))
  expect_true(exists("Location_data", where = request_data))

})


test_that("All tables contain correct columns...", {
  expect_identical(colnames(request_data$Brood_data), colnames(pipeline_output$Brood_data))
  expect_identical(colnames(request_data$Capture_data), colnames(pipeline_output$Capture_data))
  expect_identical(colnames(request_data$Individual_data), colnames(pipeline_output$Individual_data))
  expect_identical(colnames(request_data$Location_data), colnames(pipeline_output$Location_data))
})


test_that("Subset output is identical when 'PopID' and 'Species' or 'filter' is used...", {

  request_data_filter <- subset_datarqst(PopSpec = request_PopSpec, test = TRUE, save = FALSE)

  expect_identical(request_data$Brood_data, request_data_filter$Brood_data)
  expect_identical(request_data$Capture_data, request_data_filter$Capture_data)
  expect_identical(request_data$Individual_data, request_data_filter$Individual_data)
  expect_identical(request_data$Location_data, request_data_filter$Location_data)

})


test_that("Conflicted species individuals are only included when prompted...", {

  request_data_C <- subset_datarqst(PopID = request_PopIDs, Species = request_Species,
                                  include_conflicting = TRUE, test = TRUE, save = FALSE)

  expect_length(subset(request_data$Individual_data, IndvID == 'V56449')$IndvID, 0)
  expect_length(subset(request_data$Capture_data, IndvID == 'V56449')$IndvID, 0)

  expect_gt(length(subset(request_data_C$Individual_data, IndvID == 'V56449')$IndvID), 0)
  expect_gt(length(subset(request_data_C$Capture_data, IndvID == 'V56449')$IndvID), 0)

})

test_that("We get correct errors when wrong Species, Pop_ID or PopSpec given...", {

  message1 <- tryCatch(subset_datarqst(Species = "WRONG", test = TRUE),
                      error = function(e) e$message )

  expect_true(stringr::str_detect(message1, pattern = "^\n Species code is incorrect:"))

  message2 <- tryCatch(subset_datarqst(PopID = "WRONG", test = TRUE),
                      error = function(e) e$message )

  expect_true(stringr::str_detect(message2, pattern = "^\n PopID is incorrect:"))

  message3 <- tryCatch(subset_datarqst(PopSpec = "AMM_WRONG", test = TRUE),
                       error = function(e) e$message )

  expect_true(stringr::str_detect(message3, pattern = "^\n Species code is incorrect:"))

  message4 <- tryCatch(subset_datarqst(PopSpec = "WRONG_AMM", test = TRUE),
                       error = function(e) e$message )

  expect_true(stringr::str_detect(message4, pattern = "^\n PopID is incorrect:"))

})



