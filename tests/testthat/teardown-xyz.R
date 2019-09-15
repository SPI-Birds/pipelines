#Delete all .csv files in test environment
# purrr::pwalk(.l = list(list.files(pattern = ".csv")),
#              .f = ~{
#
#                file.remove(eval(..1))
#
#              })

#Remove path object and pipeline output object from the Global Env
rm(path)
rm(pipeline_output, envir = .GlobalEnv)
