#Delete all .csv files in test environment
# purrr::pwalk(.l = list(list.files(pattern = ".csv")),
#              .f = ~{
#
#                file.remove(eval(..1))
#
#              })

# Restore approved list
create_approved_list(backup_approved_list, dummy = TRUE)
