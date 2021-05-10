context("Test archiving function...")

test_that("We get correct errors...", {

  #Error when OwnerID doesn't exist
  expect_error(archive(data_folder = here::here("./inst/extdata/test_archiving"), OwnerID = "NO", new_data_path = ""))
  #Error when no new files are given
  expect_error(archive(data_folder = here::here("./inst/extdata/test_archiving"), OwnerID = "POP", new_data_path = NULL))
  #Error when update type is not major or minor
  expect_error(archive(data_folder = here::here("./inst/extdata/test_archiving"), OwnerID = "POP", new_data_path = "", update_type = ""))

})

test_that("Initial archiving works...", {

  testthat::skip(message = "Skipped in devtools::test(). Run manually to test archiving")

  date_folder    <- format(Sys.Date(), format = "%Y_%m_%d")
  main_folder    <- here::here("./inst/extdata/test_archiving")

  archive(data_folder = main_folder, OwnerID = "NEW", initial = TRUE, new_data_date = "2020-01-01")

  #Check that an archive folder was created
  expect_true("archive" %in% list.dirs(paste0(main_folder, "/NEW_testpop"), full.names = FALSE))

  #Check that a folder with new date was created inside archive
  expect_true(date_folder %in% list.dirs(paste0(main_folder, "/NEW_testpop/archive"), full.names = FALSE))

  #Check that data has been moved correctly
  #It should be the same in both the data and original
  expect_equal(read.csv(paste0(main_folder, "/NEW_testpop/NEW_PrimaryData_XXX.csv")) %>% dplyr::pull(Col2), c(1, 2, 3))
  expect_equal(read.csv(paste0(main_folder, "/NEW_testpop/archive/", date_folder, "/NEW_PrimaryData_XXX.csv")) %>% dplyr::pull(Col2), c(1, 2, 3))

  #Check that metadata is correct in both original and archive folder
  expect_equal(read.delim(paste0(main_folder, "/NEW_testpop/NEW_ArchiveMetaData.txt"), sep = "", header = FALSE),
               structure(list(V1 = c("Name:", "Owner:", "Version:", "LastUpdate:"),
                              V2 = c(NA, "NEW", paste0("2020.00"), "2020-01-01")),
                         class = "data.frame", row.names = c(NA, -4L)))

  expect_equal(read.delim(paste0(main_folder, "/NEW_testpop/archive/2020_01_01/NEW_ArchiveMetaData.txt"), sep = "", header = FALSE),
               structure(list(V1 = c("Name:", "Owner:", "Version:", "LastUpdate:"),
                              V2 = c(NA, "NEW", paste0("2020.00"), "2020-01-01")),
                         class = "data.frame", row.names = c(NA, -4L)))

  #Switch back to original state
  workd <- getwd()

  #Delete new archiving created
  system(paste0("rm -r ", workd, "/inst/extdata/test_archiving/NEW_testpop/archive"))
  system(paste0("rm ", workd, "/inst/extdata/test_archiving/NEW_testpop/NEW_ArchiveMetaData.txt"))

})

test_that("Test archiving works when an archiving folder already exists...", {

  testthat::skip(message = "Skipped in devtools::test(). Run manually to test archiving")

  date_folder    <- format(Sys.Date(), format = "%Y_%m_%d")
  main_folder    <- here::here("./inst/extdata/test_archiving")

  archive(data_folder = main_folder, OwnerID = "POP", new_data_path = here::here("./inst/extdata/POP_PrimaryData_XXX.csv"))

  #Check that a new folder with current date was created inside archive
  #and that the original folder still exists
  expect_true(all(c(date_folder, "2019_01_01") %in% list.dirs(paste0(main_folder, "/POP_testpop/archive"), full.names = FALSE)))

  #Check that data has been moved correctly
  expect_equal(read.csv(paste0(main_folder, "/POP_testpop/POP_PrimaryData_XXX.csv")) %>% pull(Col2), c(3, 4, 5))
  expect_equal(read.csv(paste0(main_folder, "/POP_testpop/archive/", "2019_01_01", "/POP_PrimaryData_XXX.csv")) %>% pull(Col2), c(1, 2, 3))
  expect_equal(read.csv(paste0(main_folder, "/POP_testpop/archive/", date_folder, "/POP_PrimaryData_XXX.csv")) %>% pull(Col2), c(3, 4, 5))

  #Check that meta-data is correct in all folder
  #Original archive data should stay the same
  expect_equal(read.delim(paste0(main_folder, "/POP_testpop/archive/", "2019_01_01", "/POP_ArchiveMetaData.txt"), sep = "", header = FALSE),
               structure(list(V1 = c("Name:", "Owner:", "Version:", "LastUpdate:"),
                              V2 = c(NA, "POP", "2019.00", "2019-01-01")),
                         class = "data.frame", row.names = c(NA, -4L)))

  #Main folder should contain new metadata
  expect_equal(read.delim(paste0(main_folder, "/POP_testpop/POP_ArchiveMetaData.txt"), sep = "", header = FALSE),
               structure(list(V1 = c("Name:", "Owner:", "Version:", "LastUpdate:"),
                              V2 = c(NA, "POP", paste0(lubridate::year(Sys.Date()), ".00"), as.character(Sys.Date()))),
                         class = "data.frame", row.names = c(NA, -4L)))

  #New archive folder should be the same as main folder
  expect_equal(read.delim(paste0(main_folder, "/POP_testpop/POP_ArchiveMetaData.txt"), sep = "", header = FALSE),
               read.delim(paste0(main_folder, "/POP_testpop/archive/", date_folder, "/POP_ArchiveMetaData.txt"), sep = "", header = FALSE))

  #Switch back to original state
  workd <- getwd()

  #Move new data back outside of the folder
  system(paste0("mv ", workd, "/inst/extdata/test_archiving/POP_testpop/POP_PrimaryData_XXX.csv ", workd, "/"))

  #Delete new meta-data
  system(paste0("rm ", workd, "/inst/extdata/test_archiving/POP_testpop/POP_ArchiveMetaData.txt"))

  #Move current primary and meta data back to original location
  system(paste0("mv ", workd, "/inst/extdata/test_archiving/POP_testpop/archive/", format(Sys.Date(), "%Y_%m_%d"), "/POP_PrimaryData_XXX.csv ", workd, "/inst/extdata/test_archiving/POP_testpop/"))
  system(paste0("mv ", workd, "/inst/extdata/test_archiving/POP_testpop/archive/", format(Sys.Date(), "%Y_%m_%d"), "/POP_ArchiveMetaData.txt ", workd, "/inst/extdata/test_archiving/POP_testpop/"))

  #Delete archiving folder
  system(paste0("rm -r ", workd, "/inst/extdata/test_archiving/POP_testpop/archive"))

})
