# #Check for exisiting data file in extdata
# #Only save this here for efficiency when testing quality check
# #Do not save to github repo as it contains primary data
if(file.exists("../../inst/extdata/test_data.RDS")){

  pipeline_output <- readRDS(system.file("extdata", "test_data.RDS", package = "pipelines", mustWork = TRUE))

} else {

  #Run pipelines for all populations
  message("Choose the location of the raw data to run tests...")

  #Determine operating system
  OS <- tolower(sessionInfo()$running)

  # Run pipelines depending on operating system
  # (If running on a Mac, pipelines that use Access databases are not run)
  if(grepl(pattern = 'mac', x = OS)){
    pipeline_output <- run_pipelines(path = choose_directory(),
                                     PopID = c("SSQ", "BAN", "VEL", "CHO", "MUR", "PIR", "ROU", "MON", "MTV", "MIS", "HOC",
                                               "KEV", "HAR", "PEE", "BOS",
                                               "WYT", "PIP", "EDM"),
                                     output_type = "R")
  }else if(grepl(pattern = 'windows', x = OS)){

    pipeline_output <- run_pipelines(path = choose_directory(),
                                     PopID = c("SSQ", "BAN", "VEL", "CHO", "MUR", "PIR", "ROU", "MON", "MTV", "MIS", "HOC",
                                               "HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES", "KEV", "HAR", "PEE", "BOS",
                                               "WYT", "PIP", "EDM"),
                                     output_type = "R")
  }else{
    stop(paste0('Operating system ', OS, ' not supported'))
  }



}


# Run quality check for dummy data and produce no report
dummy_check <- quality_check(R_data = dummy_data,
                             output = FALSE,
                             check_format = FALSE)
