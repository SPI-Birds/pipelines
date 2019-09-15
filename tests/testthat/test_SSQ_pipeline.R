context("Run data quality check on Santo Stefano Quisqina pipeline output")

test_that("SSQ outputs all files...", {

  expect_true(exists("Brood_data", where = pipeline_output$SSQ))
  expect_true(exists("Capture_data", where = pipeline_output$SSQ))
  expect_true(exists("Individual_data", where = pipeline_output$SSQ))
  expect_true(exists("Location_data", where = pipeline_output$SSQ))

})

test_that("SSQ individual data has no errors...", {

  check <- check_format_individual(pipeline_output$SSQ$Individual_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    sapply(check$error_output, print)

  }

})

test_that("SSQ brood data has no errors...", {

  #Check that the format of the data is correct
  check <- check_format_individual(pipeline_output$SSQ$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    sapply(check$error_output, print)

  }

  #Check that clutch size < brood size
  check <- compare_clutch_brood(pipeline_output$SSQ$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    sapply(check$error_output, print)

  }

  #Check that brood size < fledglings
  check <- compare_brood_fledglings(pipeline_output$SSQ$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    sapply(check$error_output, print)

  }

  #Check that laying date < hatch date
  check <- compare_laying_hatching(pipeline_output$SSQ$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    sapply(check$error_output, print)

  }

  #Check that hatch date < fledge date
  check <- compare_hatching_fledging(pipeline_output$SSQ$Brood_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    sapply(check$error_output, print)

  }

})

test_that("Check for impossible values in SSQ brood data...", {

  #Check that the format of the data is correct
  brood_data <- pipeline_output$SSQ$Brood_data %>%
    split(f = as.factor(.$Species))

  purrr::pwalk(.l = list(brood_data),
               .f = ~{

                 check <- check_values_brood(Brood_data = ..1, species = unique(..1$Species))

                 expect_false(check$check_list$Error)

                 if(check$check_list$Error == TRUE){

                   sapply(check$error_output, print)

                 }

               })

})

## THIS NEEDS TO BE FIXED AS THE check_values_capture() function has missing info.
# test_that("Check for impossible values in SSQ capture data...", {
#
#   #Check that the format of the data is correct
#   capture_data <- utils::read.csv("Capture_data_SSQ.csv", stringsAsFactors = FALSE) %>%
#     split(f = as.factor(.$Species))
#
#   purrr::pwalk(.l = list(capture_data),
#                .f = ~{
#
#                  check <- check_values_capture(Capture_data = ..1, species = unique(..1$Species))
#
#                  expect_false(check$check_list$Error)
#
#                })
#
# })

test_that("SSQ capture data has no errors...", {

  check <- check_format_individual(pipeline_output$SSQ$Capture_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    sapply(check$error_output, print)

  }

})

test_that("SSQ location data has no errors...", {

  check <- check_format_individual(pipeline_output$SSQ$Location_data)

  expect_false(check$check_list$Error)

  if(check$check_list$Error == TRUE){

    sapply(check$error_output, print)

  }

})
