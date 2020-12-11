context("Test archiving function...")

test_that("We get correct errors...", {

  #Error when PopID doesn't exist
  expect_error(archive(data_folder = here::here("./inst/extdata/test_archiving"), PopID = "NO"))
  #Error when no new files are given
  expect_error(archive(data_folder = here::here("./inst/extdata/test_archiving"), PopID = "POP", new_data_path = NULL))
  #Error when update type is not major or minor
  expect_error(archive(data_folder = here::here("./inst/extdata/test_archiving"), PopID = "POP", new_data_path = "", update_type = ""))

})

test_that("Test archiving works when no archiving folder exists...", {

  date_folder    <- format(Sys.Date(), format = "%Y_%m_%d")
  main_folder    <- here::here("./inst/extdata/test_archiving")

  archive(data_folder = main_folder, PopID = "POP", new_data_path = here::here("./inst/extdata/POP_PrimaryData_XXX.csv"))

  #Check that an archive folder was created
  expect_true("archive" %in% list.dirs(paste0(main_folder, "/POP_testpop"), full.names = FALSE))

  #Check that a folder with current date was created inside archive
  expect_true(date_folder %in% list.dirs(paste0(main_folder, "/POP_testpop/archive"), full.names = FALSE))

  #Check that data has been moved correctly
  expect_equal(read.csv(paste0(main_folder, "/POP_testpop/POP_PrimaryData_XXX.csv")) %>% pull(Col2), c(3, 4, 5))
  expect_equal(read.csv(paste0(main_folder, "/POP_testpop/archive/", date_folder, "/POP_PrimaryData_XXX.csv")) %>% pull(Col2), c(1, 2, 3))

  #Check that meta-data is correct
  expect_equal(read.delim(paste0(main_folder, "/POP_testpop/archive/", date_folder, "/POP_ArchiveMetaData.txt"), sep = "", header = FALSE),
               structure(list(V1 = c("Name:", "PopID:", "Version:", "LastUpdate:"),
                              V2 = c("Population", "POP", "2019.00", "2019-01-01")),
                         class = "data.frame", row.names = c(NA, -4L)))

  expect_equal(read.delim(paste0(main_folder, "/POP_testpop/POP_MetaData.txt"), sep = "", header = FALSE),
               structure(list(V1 = c("Name:", "PopID:", "Version:", "LastUpdate:"),
                              V2 = c("Population", "POP", paste0(lubridate::year(Sys.Date()), ".00"), as.character(Sys.Date()))),
                         class = "data.frame", row.names = c(NA, -4L)))

})
