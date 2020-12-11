# Restore approved list
create_approved_list()

browser()

#Delete output of archiving tests
oldwd <- getwd()
setwd(here::here("./inst/extdata"))

#Delete new archiving created
system("rm -r ./test_archiving/NEW_testpop/archive")
system("rm ./test_archiving/NEW_testpop/NEW_ArchiveMetaData.txt")

#Move new data back outside of the folder
system("mv ./test_archiving/POP_testpop/POP_PrimaryData_XXX.csv ./")

#Delete new meta-data
system("rm ./test_archiving/POP_testpop/POP_ArchiveMetaData.txt")

#Move current primary and meta data back to original location
system(paste0("cp ./test_archiving/POP_testpop/archive/", "2019_01_01", "/POP_PrimaryData_XXX.csv ./test_archiving/POP_testpop/"))
system(paste0("cp ./test_archiving/POP_testpop/archive/", "2019_01_01", "/POP_ArchiveMetaData.txt ./test_archiving/POP_testpop/"))

#Delete new archiving folder
system(paste0("rm -r ./test_archiving/POP_testpop/archive/", format(Sys.Date(), format = "%Y_%m_%d")))
