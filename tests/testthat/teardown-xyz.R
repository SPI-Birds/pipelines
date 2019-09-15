#Delete all .csv files in test environment
# purrr::pwalk(.l = list(list.files(pattern = ".csv")),
#              .f = ~{
#
#                file.remove(eval(..1))
#
#              })
