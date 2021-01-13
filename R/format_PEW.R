#' Construct standard format data from Peerdsbos West, Belgium (PEW).

format_PEW <- function(db = choose_directory(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       output_type = "R"){

  # Force user to select directory
  force(db)

  # Determine species codes for filtering
  if(is.null(species)){

    species <- species_codes$Species

  }

  start_time <- Sys.time()




  # BROOD DATA

  message("Compiling brood information...")
  Brood_data <- create_brood_PEW()


  # CAPTURE DATA

  message("Compiling capture information...")
  Capture_data <- create_capture_PEW()


  # INDIVIDUAL DATA

  message("Compiling individual information...")
  Individual_data <- create_individual_PEW()


  # LOCATION DATA

  message("Compiling location information...")
  Location_data <- create_location_PEW()

### OTHER ARRANGEMENTS ....
  time <- difftime(Sys.time(), start_time, units = "sec")

message(paste0("All tables generated in ", round(time, 2), " seconds"))

# EXPORT DATA

if(output_type == "csv"){

  message("Saving .csv files...")

  utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_PEW.csv"), row.names = F)

  utils::write.csv(x = Capture_data, file = paste0(path, "\\Capture_data_PEW.csv"), row.names = F)

  utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_PEW.csv"), row.names = F)

  utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_PEW.csv"), row.names = F)

  invisible(NULL)

}

if(output_type == "R"){

  message("Returning R objects...")

  return(list(Brood_data = Brood_data,
              Capture_data = Capture_data,
              Individual_data = Individual_data,
              Location_data = Location_data))

}


}


create_brood_PEW <- function(data){}

create_capture_PEW <- function(data){}

create_individual_PEW <- function(data){}

create_location_PEW <- function(data){}



