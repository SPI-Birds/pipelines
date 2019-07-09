context("Run data quality check on University of Antwerp pipeline output")

test_that("Format of output data is as expected...", {

  all_csvs <- list.files(path = ".", pattern = "UAN.csv")

  skip_if(length(all_csvs) == 0)


})
