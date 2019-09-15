#Check for exisiting data file in extdata
#Only save this here for efficiency when testing quality check
#Do not save to github repo as it contains primary data
if(file.exists("../../inst/extdata/test_data.RDS")){

  pipeline_output <- readRDS(system.file("extdata", "test_data.RDS", package = "HNBStandFormat", mustWork = TRUE))

} else {

  #Run pipelines for all populations
  pipeline_output <- run_pipelines(path = "C:\\Users\\Liam\\Desktop\\Git_projects\\HNBStandFormat\\Raw_data",
                                   PopID = c("HAR", "CHO", "BAN", "SSQ", "VEL"),
                                   output_type = "R")

}
