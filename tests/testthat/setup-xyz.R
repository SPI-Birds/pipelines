# #Check for exisiting data file in extdata
# #Only save this here for efficiency when testing quality check
# #Do not save to github repo as it contains primary data
if(file.exists("../../inst/extdata/test_data.RDS")){

  pipeline_output <- readRDS(system.file("extdata", "test_data.RDS", package = "SPIbirds", mustWork = TRUE))

} else {

  #Run pipelines for all populations
  message("Choose the location of the raw data to run tests...")
  pipeline_output <- run_pipelines(path = choose.dir(),
                                   PopID = c("SSQ", "BAN", "VEL", "CHO", "COR", "ROU"),
                                   output_type = "R")

}
