# Restore approved list
create_approved_list(backup_approved_list)

#Move archiving test data back to its original location for future tests
oldwd <- getwd()
setwd(here::here("./inst/extdata"))

#Move new data back outside of the folder
system("mv ./test_archiving/POP_testpop/POP_PrimaryData_XXX.csv ./")

#Delete new meta-data
system("rm ./test_archiving/POP_testpop/POP_MetaData.txt")

#Move current primary and meta data back to original location
system(paste0("mv ./test_archiving/POP_testpop/archive/", format(Sys.Date(), "%d_%m_%Y"), "/POP_PrimaryData_XXX.csv ./test_archiving/POP_testpop/"))
system(paste0("mv ./test_archiving/POP_testpop/archive/", format(Sys.Date(), "%d_%m_%Y"), "/POP_MetaData.txt ./test_archiving/POP_testpop/"))

#Delete archiving folder
system("rm -r ./test_archiving/POP_testpop/archive")
