data_path <- choose.dir()

# Run quality check for dummy data and produce no report
message("Create dummy data quality check output...")
dummy_check <- quality_check(test = TRUE,
                             output = FALSE,
                             check_format = FALSE)
