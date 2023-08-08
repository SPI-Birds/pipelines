test_pipeline <- function(...){

  Sys.setenv(test = TRUE)

  devtools::test(...)

}
