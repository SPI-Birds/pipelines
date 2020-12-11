#' Archive SPI-Birds raw data
#'
#' @param data_folder Path. Folder containing all current SPI-Birds data (N drive)
#' @param PopID Character string. 3-4 letter code for population.
#' Should be the same as the folder name where data are saved.
#' @param new_data_path File path. Updated data to use as current data in the system.
#' Where a population uses multiple files provide a list of file paths.
#' @param update_type Character string. Have we received a new year of data ("major")
#' or data with bug/error fixes ("minor")?
#'
#' @return Returns nothing.
#' @export
#'
#' @examples
archive <- function(data_folder = choose_directory(), update_type = "major", PopID, new_data_path) {

  #Force data folder and new data paths in that order
  #This is useful if both arguments use file explorer
  force(data_folder)
  force(new_data_path)

  #Check that PopID and new files have been provided correctly...
  if (missing(PopID)) {
    stop("Provide the PopID that you wish to archive")
  }

  if (missing(new_data_path) | !is.vector(new_data_path)) {
    stop("Provide a vector of new data files.")
  }

  if (!update_type %in% c("minor", "major")) {
    stop("Update type should be either 'major' or 'minor'")
  }

  #Extract existing file information.
  #Find the subfolder corresponding to the population...
  all_subfolder <- list.dirs(data_folder)
  pop_subfolder <- all_subfolder[stringr::str_detect(all_subfolder, pattern = paste0(PopID, "_"))]

  #Check that PopID matches one of the subfolders in the data
  if (length(pop_subfolder) == 0) {

    stop("No subfolder exists for this population. Do you have the correct PopID?")

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

  #Extract meta-data info. We need this for determining new meta-data
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

  #Check 1: Check new data fits our naming convention (i.e. PopID_PrimaryData)
  file_correct_format <- purrr::map_lgl(.x = new_data_name,
                               .f = function(file, expected_string){

                                 return(stringr::str_detect(file, paste0("^", expected_string)))

                               }, expected_string = paste(PopID, "PrimaryData", sep = "_"))

  if (!all(file_correct_format)) {

    stop(paste0("Files should be in the format ", PopID, "_PrimaryData_XXX"))

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

  #Create an archive folder if it doesn't exist...
  if (!any(c("./archive", ".\\archive") %in% list.dirs())) {

    system("mkdir archive")

  }

  #Determine the name for the new archiving subfolder
  date_folder_name <- format(Sys.Date(), format = "%Y_%m_%d")

  #Create a new folder with this name inside the archive folder
  system(paste0("mkdir ./archive/", date_folder_name))

  #Move all current data and metadata into the new archive folder
  purrr::walk(.x = current_data_path,
              .f = ~{

                  system(paste0("mv ", ..1, " ./archive/", date_folder_name))

              })

  #Move all the new data into the main folder
  purrr::walk(.x = new_data_path,
              .f = ~{

                system(paste0("mv ", ..1, " ."))

              })

  #Update the meta-data
  #If it's a major update, the version number is YYYY.00 FIXME: Do we want this, or do we prefer regular version numbers e.g. 1.0, 1.1 etc.
  #If it's a minor update, the version number is current version + .01
  if (update_type == "major") {

    new_version <- paste0(lubridate::year(Sys.Date()), ".00")

  } else {

    new_version <- as.character(current_version + 0.01)

  }

  new_metadata <- paste0("Name: ", current_name, "\nPopID: ", PopID, "\nVersion: ", new_version, "\nLastUpdate: ", Sys.Date(), "\n")
  write(new_metadata, file = paste0(PopID, "_ArchiveMetaData.txt"))

  #Set working directory back to the project folder
  setwd(resetwd)

}
