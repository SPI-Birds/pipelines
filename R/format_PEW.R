#' Construct standard summary for data from Peerdsbos West, Belgium.
#' Actively started 27/11/2020

# format_PEW <- function(db = choose_directory(),
#                        species = NULL,
#                        pop = NULL,
#                        path = ".",
#                        output_type = "R"){
#
#   # Force user to select directory
#   force(db)
#
#   # Determine species codes for filtering
#   if(is.null(species)){
#
#     species <- species_codes$Species
#
#   }
#
#   start_time <- Sys.time()
#
#
#
#
#   # BROOD DATA
#
#   message("Compiling brood information...")
#   Brood_data <- create_brood_PEW()
#
#
#   # CAPTURE DATA
#
#   message("Compiling capture information...")
#   Capture_data <- create_capture_PEW()
#
#
#   # INDIVIDUAL DATA
#
#   message("Compiling individual information...")
#   Individual_data <- create_individual_PEW()
#
#
#   # LOCATION DATA
#
#   message("Compiling location information...")
#   Location_data <- create_location_PEW()
#
# ### OTHER ARRANGEMENTS ....
#
#
# }
#
#
# create_brood_PEW <- function(data){}
#
# create_capture_PEW <- function(data){}
#
# create_individual_PEW <- function(data){}
#
# create_location_PEW <- function(data){}



