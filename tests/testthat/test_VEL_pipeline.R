context("Run data quality check on Velky Kosir pipeline output")

test_that("Format of output data is as expected...", {

  all_csvs <- list.files(path = ".", pattern = "VEL.csv")

  skip_if(length(all_csvs) == 0)


})
