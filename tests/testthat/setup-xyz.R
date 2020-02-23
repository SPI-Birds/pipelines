# #Check for exisiting data file in extdata
# #Only save this here for efficiency when testing quality check
# #Do not save to github repo as it contains primary data
if(file.exists("../../inst/extdata/test_data.RDS")){

  pipeline_output <- readRDS(system.file("extdata", "test_data.RDS", package = "pipelines", mustWork = TRUE))

} else {

  #Run pipelines for all populations
  message("Choose the location of the raw data to run tests...")
  pipeline_output <- run_pipelines(path = choose_directory(),
                                   PopID = c("SSQ", "BAN", "VEL", "CHO", "MUR", "PIR", "ROU", "MON", "MTV", "MIS", "HOC",
                                             "HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES", "KEV", "HAR", "PEE", "BOS",
                                             "WYT"),
                                   output_type = "R")

}


# Run quality check for dummy data and produce no report
dummy_check <- quality_check(R_data = dummy_data,
                             output = FALSE,
                             check_format = FALSE)
