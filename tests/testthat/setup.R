if(Sys.getenv("test") == TRUE) {

  data_path <- choose_directory()

  withr::defer(teardown_env())
  Sys.unsetenv("test")

}
