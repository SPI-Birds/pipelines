#' Determine major species studied for each population.
#'
#' For each of the main populations, return the names
#' of all species where >100 broods have been recorded
#' over the study period.
#' @param db Location of database file.
#'
#' @return A data frame with all major species for each population
#' @export

extract_species <- function(db = NULL){

  #Assign database location if none given.
  if(is.null(db)){

    print("Please choose a database file...")

    db <- file.choose()

  }

  connection <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db, ";Uid=Admin;Pwd=;"))

  species_qry <- DBI::dbGetQuery(connection,
  "SELECT dbo_tl_AreaGroup.Name AS Pop_name, dbo_tl_Species.Name AS Species_name
FROM (((dbo_tl_AreaGroup LEFT JOIN dbo_tx_Area_AreaGroup ON dbo_tl_AreaGroup.ID = dbo_tx_Area_AreaGroup.AreaGroup) LEFT JOIN dbo_tbl_Location ON dbo_tx_Area_AreaGroup.Area = dbo_tbl_Location.AreaID) LEFT JOIN dbo_tbl_Brood ON dbo_tbl_Location.ID = dbo_tbl_Brood.BroodLocationID) LEFT JOIN dbo_tl_Species ON dbo_tbl_Brood.BroodSpecies = dbo_tl_Species.ID
GROUP BY dbo_tl_AreaGroup.Name, dbo_tbl_Brood.BroodSpecies, dbo_tl_Species.Name
HAVING (((Count(dbo_tbl_Brood.ID))>100));") %>%
    dplyr::filter(Pop_name %in% c("Hoge Veluwe", "Liesbosch Breda", "Vlieland", "Warnsborn",
                                  "Westerheide/NUON/Boslust", "Oosterhout", "Buunderkamp"))

  return(species_qry)

}
