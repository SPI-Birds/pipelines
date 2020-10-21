# #Check for existing data file in extdata
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
    pipeline_output <- run_pipelines(path = choose_directory(), save = FALSE,
                                     PopID = c("SSQ", "BAN", "VEL", "CHO", "MUR", "PIR", "ROU", "MON", "MTV", "MIS", "HOC",
                                               "KEV", "HAR", "PEE", "BOS",
                                               "WYT", "PIL", "EDM"),
                                     output_type = "R")
  }else if(grepl(pattern = 'windows', x = OS)){

    pipeline_output <- run_pipelines(path = choose_directory(), save = FALSE,
                                     PopID = c("SSQ", "BAN", "VEL", "CHO", "MUR", "PIR", "ROU", "MON", "MTV", "MIS", "HOC",
                                               "HOG", "OOS", "VLI", "BUU", "LIE", "WAR", "WES", "KEV", "HAR", "PEE", "BOS",
                                               "WYT", "PIL", "EDM", "AMM"),
                                     output_type = "R")
  }else{
    stop(paste0('Operating system ', OS, ' not supported'))
  }



}

# Create dummy approved_list
dummy_approved_list <- list(Brood_approved_list = tibble::tibble(PopID = "AAA",
                                                                 BroodID = "AAA-2020-0",
                                                                 CheckID = "B4"),
                            Capture_approved_list = tibble::tibble(PopID = NA_character_,
                                                                   CaptureID = NA_character_,
                                                                   CheckID = NA_character_),
                            Individual_approved_list = tibble::tibble(PopID = NA_character_,
                                                                      IndvID = NA_character_,
                                                                      CheckID = NA_character_),
                            Location_approved_list = tibble::tibble(PopID = NA_character_,
                                                                    LocationID = NA_character_,
                                                                    CheckID = NA_character_)
                            )

# Back-up existing approved_list
backup_approved_list <- approved_list

# Create dummy
create_approved_list(dummy_approved_list, dummy = TRUE)

# Run quality check for dummy data and produce no report
dummy_check <- quality_check(R_data = dummy_data,
                             output = FALSE,
                             check_format = FALSE)
