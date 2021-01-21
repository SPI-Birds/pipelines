# #Check for existing data file in extdata
# #Only save this here for efficiency when testing quality check
# #Do not save to github repo as it contains primary data
OS <- tolower(sessionInfo()$running)

#Run pipelines for all populations
message("Choose the location of the data to run tests...")

if (grepl(pattern = 'mac', x = OS)) {

  path <- tcltk::tk_choose.dir()

} else if (grepl(pattern = 'windows', x = OS)) {

  path <-  utils::choose.dir()

}

#Link to folder with completed pipeline data...
if ("test_data.RDS" %in% list.files(path)) {

  message("Loading completed pipeline data...")

  pipeline_output <- readRDS(paste0(path, "/test_data.RDS"))

#Or folder with raw data...
} else {

  message("Running all pipelines...")

  # Run pipelines depending on operating system
  # (If running on a Mac, pipelines that use Access databases are not run)
  if (grepl(pattern = 'mac', x = OS)) {
    pipeline_output <- run_pipelines(path = path,
                                     PopID = c("SSQ", "BAN", "VEL", "CHO", "MUR", "PIR", "ROU", "MON", "MTV", "MIS", "HOC",
                                               "KEV", "HAR", "PEE", "BOS",
                                               "WYT", "PIL", "EDM", "DIN", "KAT", "NAG", "OKE", "TEI"),
                                     output_type = "R")
  } else if (grepl(pattern = 'windows', x = OS)) {

    pipeline_output <- run_pipelines(path = path,
                                     PopID = c("SSQ", "BAN", "VEL", "CHO", "MUR", "PIR", "ROU", "MON", "MTV", "MIS", "HOC",
                                               "HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES", "KEV", "HAR", "PEE", "BOS",
                                               "WYT", "PIL", "EDM", "AMM", "DIN", "KAT", "NAG", "OKE", "TEI"),
                                     output_type = "R")
  } else {
    stop(paste0('Operating system ', OS, ' not supported'))
  }

}

# Create dummy
# message("Create dummy data approved list...")
approved_list <- create_approved_list(dummy = TRUE)

# Run quality check for dummy data and produce no report
message("Create dummy data quality check output...")
dummy_check <- quality_check(test = TRUE,
                             output = FALSE,
                             check_format = FALSE)
