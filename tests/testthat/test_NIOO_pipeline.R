# context("Run data quality check on Choupal pipeline output")
#
# test_that("CHO pipeline works...", {
#
#   #Save in global environment for use later
#   suppressWarnings(pipeline_output <<- run_pipelines(path = path,
#                                                      PopID = c("HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES"),
#                                                      output_type = "R"))
#
# })
#
# test_that("CHO outputs all files...", {
#
#   expect_true(exists("Brood_data", where = .GlobalEnv$pipeline_output$CHO))
#   expect_true(exists("Capture_data", where = .GlobalEnv$pipeline_output$CHO))
#   expect_true(exists("Individual_data", where = .GlobalEnv$pipeline_output$CHO))
#   expect_true(exists("Location_data", where = .GlobalEnv$pipeline_output$CHO))
#
# })
#
# test_that("CHO individual data has no errors...", {
#
#   check <- check_format_individual(.GlobalEnv$pipeline_output$CHO$Individual_data)
#
#   expect_false(check$check_list$Error)
#
#   if(check$check_list$Error == TRUE){
#
#     purrr::pwalk(.l = check$error_output, .f = print)
#
#   }
#
# })
#
# test_that("CHO brood data has no errors...", {
#
#   #Check that the format of the data is correct
#   check <- check_format_individual(.GlobalEnv$pipeline_output$CHO$Brood_data)
#
#   expect_false(check$check_list$Error)
#
#   if(check$check_list$Error == TRUE){
#
#     purrr::pwalk(.l = check$error_output, .f = print)
#
#   }
#
#   #Check that clutch size < brood size
#   check <- compare_clutch_brood(.GlobalEnv$pipeline_output$CHO$Brood_data)
#
#   expect_false(check$check_list$Error)
#
#   if(check$check_list$Error == TRUE){
#
#     purrr::pwalk(.l = check$error_output, .f = print)
#
#   }
#
#   #Check that brood size < fledglings
#   check <- compare_brood_fledglings(.GlobalEnv$pipeline_output$CHO$Brood_data)
#
#   expect_false(check$check_list$Error)
#
#   if(check$check_list$Error == TRUE){
#
#     purrr::pwalk(.l = check$error_output, .f = print)
#
#   }
#
#   #Check that laying date < hatch date
#   check <- compare_laying_hatching(.GlobalEnv$pipeline_output$CHO$Brood_data)
#
#   expect_false(check$check_list$Error)
#
#   if(check$check_list$Error == TRUE){
#
#     purrr::pwalk(.l = check$error_output, .f = print)
#
#   }
#
#   #Check that hatch date < fledge date
#   check <- compare_hatching_fledging(.GlobalEnv$pipeline_output$CHO$Brood_data)
#
#   expect_false(check$check_list$Error)
#
#   if(check$check_list$Error == TRUE){
#
#     purrr::pwalk(.l = check$error_output, .f = print)
#
#   }
#
# })
#
# test_that("Check for impossible values in CHO brood data...", {
#
#   #Check that the format of the data is correct
#   brood_data <- .GlobalEnv$pipeline_output$CHO$Brood_data %>%
#     split(f = as.factor(.$Species))
#
#   purrr::pwalk(.l = list(brood_data),
#                .f = ~{
#
#                  check <- check_values_brood(Brood_data = ..1, species = unique(..1$Species))
#
#                  expect_false(check$check_list$Error)
#
#                  if(check$check_list$Error == TRUE){
#
#                    purrr::pwalk(.l = check$error_output, .f = print)
#
#                  }
#
#                })
#
# })
#
# ## THIS NEEDS TO BE FIXED AS THE check_values_capture() function has missing info.
# # test_that("Check for impossible values in CHO capture data...", {
# #
# #   #Check that the format of the data is correct
# #   capture_data <- utils::read.csv("Capture_data_CHO.csv", stringsAsFactors = FALSE) %>%
# #     split(f = as.factor(.$Species))
# #
# #   purrr::pwalk(.l = list(capture_data),
# #                .f = ~{
# #
# #                  check <- check_values_capture(Capture_data = ..1, species = unique(..1$Species))
# #
# #                  expect_false(check$check_list$Error)
# #
# #                })
# #
# # })
#
# test_that("CHO capture data has no errors...", {
#
#   check <- check_format_individual(.GlobalEnv$pipeline_output$CHO$Capture_data)
#
#   expect_false(check$check_list$Error)
#
#   if(check$check_list$Error == TRUE){
#
#     purrr::pwalk(.l = check$error_output, .f = print)
#
#   }
#
# })
#
# test_that("CHO location data has no errors...", {
#
#   check <- check_format_individual(.GlobalEnv$pipeline_output$CHO$Location_data)
#
#   expect_false(check$check_list$Error)
#
#   if(check$check_list$Error == TRUE){
#
#     purrr::pwalk(.l = check$error_output, .f = print)
#
#   }
#
# })
