pipeline_output <- run_pipelines(path = data_path, PopID = c("CHO", "SSQ", "HOC", "HAR", "PIL"))

request_PopIDs <- c('CHO', 'HOC', 'HAR', 'PIL')
request_Species <- c('PARMAJ', 'FICHYP')
request_PopSpec <- paste(expand.grid(request_PopIDs, request_Species)[,1], expand.grid(request_PopIDs, request_Species)[,2], sep = '_')

request_data <- subset_datarqst(file = pipeline_output, PopID = request_PopIDs, Species = request_Species,
                                save = FALSE)


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

  request_data_filter <- subset_datarqst(file = pipeline_output, PopSpec = request_PopSpec, save = FALSE)

  expect_identical(request_data$Brood_data, request_data_filter$Brood_data)
  expect_identical(request_data$Capture_data, request_data_filter$Capture_data)
  expect_identical(request_data$Individual_data, request_data_filter$Individual_data)
  expect_identical(request_data$Location_data, request_data_filter$Location_data)

})


test_that("Conflicted species individuals are only included when prompted...", {

  request_data_C <- subset_datarqst(file = pipeline_output, PopID = request_PopIDs, Species = request_Species,
                                  include_conflicting = TRUE, save = FALSE)

  expect_length(subset(request_data$Individual_data, IndvID == 'V56449')$IndvID, 0)
  expect_length(subset(request_data$Capture_data, IndvID == 'V56449')$IndvID, 0)

  expect_gt(length(subset(request_data_C$Individual_data, IndvID == 'V56449')$IndvID), 0)
  expect_gt(length(subset(request_data_C$Capture_data, IndvID == 'V56449')$IndvID), 0)

})

test_that("Conflicted species individuals return all capture records (even non-focal species)...", {

  request_data_C <- subset_datarqst(file = pipeline_output, PopID = "HAR", Species = "PARMAJ",
                                    include_conflicting = TRUE, save = FALSE)

  focal_indv <- request_data_C$Capture_data %>%
    filter(IndvID == "X-396693")

  expect_equal(nrow(focal_indv), 3)
  expect_equal(unname(focal_indv$Species), c("FICHYP", "FICHYP", "PARMAJ"))

  request_data_C2 <- subset_datarqst(file = pipeline_output, PopSpec = "HAR_PARMAJ",
                                    include_conflicting = TRUE, save = FALSE)

  focal_indv2 <- request_data_C2$Capture_data %>%
    filter(IndvID == "X-396693")

  expect_equal(nrow(focal_indv2), 3)
  expect_equal(unname(focal_indv2$Species), c("FICHYP", "FICHYP", "PARMAJ"))

})

test_that("We get correct errors when wrong Species, Pop_ID or PopSpec given...", {

  message1 <- tryCatch(subset_datarqst(file = pipeline_output, Species = "WRONG", save = FALSE),
                      error = function(e) e$message )

  expect_true(stringr::str_detect(message1, pattern = "^\n Species code is incorrect:"))

  message2 <- tryCatch(subset_datarqst(file = pipeline_output, PopID = "WRONG", save = FALSE),
                      error = function(e) e$message )

  expect_true(stringr::str_detect(message2, pattern = "^\n PopID is incorrect:"))

  message3 <- tryCatch(subset_datarqst(file = pipeline_output, PopSpec = "AMM_WRONG", save = FALSE),
                       error = function(e) e$message )

  expect_true(stringr::str_detect(message3, pattern = "^\n Species code is incorrect:"))

  message4 <- tryCatch(subset_datarqst(file = pipeline_output, PopSpec = "WRONG_AMM", save = FALSE),
                       error = function(e) e$message )

  expect_true(stringr::str_detect(message4, pattern = "^\n PopID is incorrect:"))

})



