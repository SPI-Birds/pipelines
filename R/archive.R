#' Archive SPI-Birds raw data
#'
#' @param data_folder Path. Folder containing all current SPI-Birds data (N drive)
#' @param OwnerID Character string. 3-4 letter code for data owner.
#' Should be the same as the folder name where data are saved.
#' @param new_data_path File path. Updated data to use as current data in the system.
#' Where a population uses multiple files provide a list of file paths.
#' @param update_type Character string. Have we received a new year of data ("major")
#' or data with bug/error fixes ("minor")?
#' @param new_data_date. Date (YYYY-MM-DD). The date at which data was last updated. If NULL,
#' will use todays date.
#' @param initial Logical. Are we creating the initial archiving information? If TRUE, will create
#' a MetaData.txt file and copy all information into an archive folder.
#'
#' @return Returns nothing.
#' @export
#'
#' @examples

archive <- function(data_folder = choose_directory(), update_type = "major", OwnerID, new_data_path,
                    new_data_date = NULL, initial = FALSE) {

  #Run all checks that are required for both initial and updating archiving process
  #Force the directory find
  force(data_folder)

  #Check that OwnerID has been provided correctly...
  if (missing(OwnerID)) {
    stop("Provide the OwnerID that you wish to archive")
  }

  #Define the date for archiving purposes
  #Use current date if no other date is provided
  if (is.null(new_data_date)) {

    new_data_date <- Sys.Date()

  } else {

    new_data_date <- as.Date(new_data_date)

  }

  #Extract existing file information.
  #Find the subfolder corresponding to the population...
  all_subfolder <- list.dirs(data_folder)
  pop_subfolder <- all_subfolder[stringr::str_detect(all_subfolder, pattern = paste0(OwnerID, "_"))]

  #Check that OwnerID matches one of the subfolders in the data
  if (length(pop_subfolder) == 0) {

    stop("No subfolder exists for this owner. Do you have the correct OwnerID?")

  }

  #If we're just doing initial archiving we can go straight into bash script...
  if (initial) {

    #Create the metadata file
    #Move working directory to population subfolder so that we can carry out bash script
    resetwd <- getwd()
    setwd(pop_subfolder)

    #Create first meta-data file
    new_metadata <- paste0("Name: ", pop_codes$OwnerName[pop_codes$Owner == OwnerID][1],
                           "\nOwner: ", OwnerID,
                           "\nVersion: ", paste0(lubridate::year(new_data_date), ".00"),
                           "\nLastUpdate: ", new_data_date, "\n")
    write(new_metadata, file = paste0(OwnerID, "_ArchiveMetaData.txt"))

    #Create archive folder...
    system("mkdir archive")

    #Create a new folder named after the archiving date
    date_folder_name <- format(new_data_date, format = "%Y_%m_%d")
    system(paste0("mkdir ./archive/", date_folder_name))

    #Copy all primary data and metadata.txt to the new folder
    #It is now stored in both places
    current_data_path <- list.files(pop_subfolder, pattern = "_PrimaryData|MetaData.txt", full.names = TRUE)
    purrr::walk(.x = current_data_path,
                .f = ~{

                  system(paste0("cp ", ..1, " ./archive/", date_folder_name))

                })

    #Set working directory back to the project folder
    setwd(resetwd)

    return(invisible())

  } else {

    #Force the location of new files
    force(new_data_path)

    #Check that data files have been specified
    if (missing(new_data_path) | !is.vector(new_data_path)) {
      stop("Provide a vector of new data files.")
    }

    #Check if the update type has been specified
    if (!update_type %in% c("minor", "major")) {
      stop("Update type should be either 'major' or 'minor'")
    }

    #Find all file paths inside the folder that are considered primary data or metadata.txt
    current_data_path <- list.files(pop_subfolder, pattern = "_PrimaryData|MetaData.txt", full.names = TRUE)

    #Separate filename from paths
    #Need this to check that names match (regardless of the file location)
    current_data_name <- purrr::map_chr(.x = current_data_path,
                                        .f = ~{

                                          split_data <- stringr::str_split(..1, "\\\\|/")[[1]]
                                          filename   <- split_data[length(split_data)]

                                          return(filename)

                                        })

    #Extract meta-data info. We need this for creating new meta-data
    current_metadata <- utils::read.delim(current_data_path[1], sep = "", header = FALSE)
    current_name     <- current_metadata[1, 2]
    current_version  <- as.numeric(current_metadata[3, 2])

    #Separate filename of new data from file path
    #As above, we need this to check that names match (regardless of the file location)
    new_data_name <- purrr::map_chr(.x = new_data_path,
                                    .f = ~{

                                      split_data <- stringr::str_split(..1, "\\\\|/")[[1]]
                                      filename   <- split_data[length(split_data)]

                                      return(filename)

                                    })

    #Check 1: Check new data fits our naming convention (i.e. OwnerID_PrimaryData)
    file_correct_format <- purrr::map_lgl(.x = new_data_name,
                                          .f = function(file, expected_string){

                                            return(stringr::str_detect(file, paste0("^", expected_string)))

                                          }, expected_string = paste(OwnerID, "PrimaryData", sep = "_"))

    if (!all(file_correct_format)) {

      stop(paste0("Files should be in the format ", OwnerID, "_PrimaryData"))

    }

    #Check 2: Check that the file names are the same between the new and current data
    #If they are different, prompt the user to decide if they want to continue
    #(this may be needed if some data is removed or changed, but it will require changes to the pipelines)
    if (!all(new_data_name %in% current_data_name)) {

      if (!interactive()) {

        stop("File names of new data and current data do not match!")

      } else {

        input <- 3

        message("File names of new data and current data do not match!")

        while (input == 3) {

          input <- utils::menu(title = "\n Do you want to continue with the archiving? This may break the pipelines...",
                               choices = c("Abort", "Continue", "View filenames"))

          if (input == 3) {

            print(data.frame(new_files = new_data_name,
                             old_files = current_data_name))

          }

          if (input == 1) {

            stop("File names of new data and current data do not match!")

          }

        }

      }

    }

    #Move working directory to population subfolder so that we can carry out bash script
    resetwd <- getwd()
    setwd(pop_subfolder)

    #Delete existing data and meta-data. This data is already in an archive folder
    purrr::walk(.x = current_data_path,
                .f = ~{

                  system(paste0("rm ", ..1))

                })

    #Move all new data into the main folder
    purrr::walk(.x = new_data_path,
                .f = ~{

                  system(paste0("mv ", ..1, " ."))

                })

    #Create a new meta-data file
    #If it's a major update, the version number is YYYY.00 FIXME: Do we want this, or do we prefer regular version numbers e.g. 1.0, 1.1 etc.
    #If it's a minor update, the version number is current version + .01
    if (update_type == "major") {

      new_version <- paste0(lubridate::year(new_data_date), ".00")

    } else {

      new_version <- as.character(current_version + 0.01)

    }

    new_metadata <- paste0("Name: ", current_name,
                           "\nOwnerID: ", OwnerID,
                           "\nVersion: ", new_version,
                           "\nLastUpdate: ", new_data_date, "\n")
    write(new_metadata, file = paste0(OwnerID, "_ArchiveMetaData.txt"))

    #Create new archiving folder
    date_folder_name <- format(new_data_date, format = "%Y_%m_%d")
    system(paste0("mkdir ./archive/", date_folder_name))

    #Find all file paths inside the folder that are considered primary data or metadata.txt
    current_data_path <- list.files(pop_subfolder, pattern = "_PrimaryData|MetaData.txt", full.names = TRUE)

    #Copy all current data and metadata into the new archive folder
    purrr::walk(.x = current_data_path,
                .f = ~{

                  system(paste0("cp ", ..1, " ./archive/", date_folder_name))

                })

    #We now have new data stored in both the main folder and the archiving folder

    #Set working directory back to the project folder
    setwd(resetwd)

    return(invisible())

  }

}
