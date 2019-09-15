#Run pipelines for all populations
pipeline_output <- run_pipelines(path = "C:\\Users\\Liam\\Desktop\\Git_projects\\HNBStandFormat\\Raw_data",
                                 PopID = c("HAR", "CHO", "BAN"),
                                 output_type = "R")
