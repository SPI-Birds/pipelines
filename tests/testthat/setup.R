data_path <- Sys.getenv("SPI_BIRDS_DATA_DIR", unset = "")
if (!nzchar(data_path)) {
  data_path <- choose_directory()
}

withr::defer(teardown_env())
