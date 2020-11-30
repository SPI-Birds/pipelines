#' Construct standard summary for data from Peerdsbos West, Belgium.
#' Actively started 27/11/2020

# library(pipelines)
# library(readxl)
#
# db_dir <- choose_directory() ## temp copy in C:\Users\ZuzanaZ\Dropbox\POSTDOC\POSTDOC_NIOO_NETHERLANDS_2020\SPI-birds_project\data_pipeline_temp_copy\PEW_PeerdsbosWest_Belgium
# db <- paste0(db_dir, "/PEW_PrimaryData.xlsx")
#
# message("Importing primary data...")
#
# pew <- readxl::read_excel(db,
#                           col_types = c("text", "numeric", "text",
#                                         "text", "text", "numeric", "date",
#                                         "text", "text", "text", "text", "text",
#                                         "text", "text", "text", "text", "text",
#                                         "text", "text", "text", "text", "text",
#                                         "text", "numeric", "date", "date",
#                                         "text", "date", "numeric", "numeric",
#                                         "text", "numeric", "numeric", "numeric",
#                                         "numeric", "numeric", "numeric",
#                                         "numeric", "numeric", "numeric")) %>%
#   janitor::clean_names(case = "upper_camel") %>%
#   janitor::remove_empty(which = "rows") %>%
#   janitor::remove_empty(which = "cols") %>%
#   #### Remove columns which we do not store in the standardized format
#   dplyr::select(-FeatherCollection ,
#                -BreathRateTime50Breaths,
#                -FeathersPartner,
#                -BreathRatePartnerTime50Breaths,
#                -BloodSample,
#                -TimeBloodSample,
#                -BloodSampleDuration,
#                -NewRing,
#                -VisitRateVisitsH,
#                -VrPartner,
#                -VisitsAlternated,
#                -VisitsAlternatedPartner,
#                -VisitsSync10,
#                -MateStrategy) %>%
#   #### Rename variables to standardized format
#   dplyr::rename(Sex = Seks,
#                 ObserverID = Measurer)
#
#
#
#
#
#
# head(as.data.frame(pew))
# sort(apply(pew, 2, function(x) sum(is.na(x))), decreasing = TRUE)
#
#
#
#
#
#
# #### Questions/Doubts
# # Name of this population is PEW or PEE?
# # What species?????
# # What is "Neophobia Transponder"?
# # Code "Experiment"?
# # Where does the chick-rearing data go? To Individual data?




















