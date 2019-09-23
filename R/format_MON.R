#'Construct standard format for data from Montpellier, France.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Montpellier, France, administered by CNRS Montpellier (Anne Charmantier and colleagues).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{IndvID:} In Capture_data, any individuals given unidentified numbers (i.e. containing 'no-ident') are treated as
#'NA. These records are still kept as they contain information about broods (e.g. AvgChickMass) but are not integrated into
#'the individual data table.
#'
#'\strong{Sex:} We ignore uncertainty in sex (e.g. category 4 = Male probable but not certain)
#'
#'\strong{LocationID:} For birds caught in boxes. Location is Plot_NextboxNumber.
#'
#'\strong{BroodID:} Broods are BreedingSeason_LocationID_day_month = BreedingSeason_Plot_NestboxNumber_day_month.
#'
#'\strong{ClutchTypeObserved:} No clutch type recorded, only calculated clutch type is given.
#'
#'\strong{Tarsus:} Left and right tarsus are measured. Right generally has more data, so we use this as our
#'measure of tarsus length. Currently, we assume everything is in Svensson's alternative, but there is supposedly
#'some change from before 1989. Need to ask Anne about this.
#'
#'\strong{Age:} We translate observed age codes into EURING codes as follows:
#'\itemize{
#'
#'\item P0 (Poussin bagué au nichoir ou en cavité naturelle/chick banded in box or natural cavity):
#'We give EURING code 1: Nestling or chick unable to fly freely.
#'
#'\item J0
#'(Juvénile (oiseau de l’année) capturé entre le moment où il s’est envolé du nichoir et le 31 décembre)
#'(Juvenile (of this year) captured between fledging and Dec 31st of that year)
#'We give EURING code 3: First-year: full-grown bird hatched in the breeding season of this calendar year.
#'
#'\item J1
#'(Juvénile (oiseau de l’année dernière) capturé après le 1er janvier et avant la mue post reproduction (juillet-août))
#'(Juvenile (of last year) capture between Jan 1st and July-Aug)
#'We give EURING code 5: 2nd year: a bird hatched last calendar year and now in its second calendar year.
#'
#'\item A0
#'(Adulte capturé entre la mue post reproduction (environ juillet-août) et le 31 dec)
#'(Adult captured between July-Aug and Dec 31st)
#'We give EURING code 4: Afer first-year: full-grown bird hatched before this calendar year; year of hatching otherwise unknown.
#'
#'\item A1
#'(Adulte capturé après le 1er janvier et avant la mue post reproduction (environ juillet-août).)
#'(Adult captured between Jan 1st and July-Aug)
#'We give EURING code 6: Afer 2nd year: full-grown bird hatched before last calendar year; year of hatching otherwise unknown.
#'This one is at least 2nd year because if it already had adult feathers before reproduction
#'it can't have been born last year.
#'}
#'
#'@inheritParams pipeline_params
#'
#'@return Generates either 4 .csv files or 4 data frames in the standard format.
#'@export

format_MON <- function(db = utils::choose.dir(),
                       species = NULL,
                       pop = NULL,
                       path = ".",
                       debug = FALSE,
                       output_type = "csv"){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }


  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_MON(db = db, species_filter = species)

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_MON(db = db, species_filter = species)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_MON(Capture_data)

  # WRANGLE DATA FOR EXPORT

  #Remove chick rings from brood data
  #Add NAs for Avg measurements until we add them later
  Brood_data <- dplyr::select(Brood_data, -pulbag1:-pulbag14) %>%
    dplyr::mutate(AvgEggMass = NA_real_, NumberEggs = NA_integer_,
                  AvgChickMass = NA_real_, NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_, NumberChicksTarsus = NA_integer_)

  # GENERATE DEBUG REPORT

  if(debug){

    message("Generating debug report...")

    generate_debug_report(path = path, Pop = "MON", Brood_data = Brood_data,
                          Capture_data = Capture_data,
                          Indv_data = Individual_data)

  }

  # EXPORT DATA

  time <- difftime(Sys.time(), start_time, units = "sec")

  message(paste0("All tables generated in ", round(time, 2), " seconds"))

  if(output_type == "csv"){

    message("Saving .csv files...")

    utils::write.csv(x = Brood_data, file = paste0(path, "\\Brood_data_MON.csv"), row.names = F)

    utils::write.csv(x = Individual_data, file = paste0(path, "\\Individual_data_MON.csv"), row.names = F)

    utils::write.csv(x = Capture_data %>% select(-Sex, -BroodID), file = paste0(path, "\\Capture_data_MON.csv"), row.names = F)

    #utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MON.csv"), row.names = F)

    invisible(NULL)

  }

  if(output_type == "R"){

    message("Returning R objects...")

    return(list(Brood_data = Brood_data,
                Capture_data = Capture_data,
                Individual_data = Individual_data,
                Location_data = data.frame(LocationID = NA, Habitat = NA)))

  }

}

#' Create capture data table for Montpellier
#'
#' @param db Location of primary data from Montpellier.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame with capture data

create_capture_MON <- function(db, species_filter){

  Full_capture_data <- readxl::read_excel(paste0(db, "//", "SIE MORPH 1976-2018.xlsx"),
                                     col_types = c("text")) %>%
    #There is a potential issue in excel that numbers are stored as text in the excel sheets.
    #These can easily be coerced back to numerics, but this throws many warnings,
    #which will masks any real problematic coercion issues (e.g. NA introduced by coercion)
    #Therefore, we read everything as text and coerce individually
    dplyr::mutate_at(.vars = vars(3, 7, 16, 37), as.integer) %>%
    dplyr::mutate_at(.vars = vars(5, 6, 14, 18:24, 26, 27, 35), as.numeric) %>%
    dplyr::mutate(Species = dplyr::case_when(.$espece == "ble" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                             .$espece == "noi" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)],
                                             .$espece == "cha" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                             .$espece == "eto" ~ "STARLING",
                                             .$espece == "grp" ~ "UN-IDENTIFIED CREEPER",
                                             .$espece == "grpj" ~ "SHORT-TOED TREECREEPER",
                                             .$espece == "grpd" ~ "EURASIAN TREECREEPER",
                                             .$espece == "hup" ~ "CRESTED TIT",
                                             .$espece == "sit" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14790)],
                                             .$espece == "moi" ~ "UN-IDENTIFIED SPARROW",
                                             .$espece == "moid" ~ "HOUSE SPARROW",
                                             .$espece == "moif" ~ Species_codes$Code[which(Species_codes$SpeciesID == 15980)],
                                             .$espece == "non" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14400)])) %>%
    #Filter by species
    dplyr::filter(Species %in% species_filter) %>%
    dplyr::mutate(CaptureDate = janitor::excel_numeric_to_date(as.numeric(date_mesure)),
                  BreedingSeason = an, CaptureTime = heure,
                  IndvID = bague, WingLength = aile,
                  BeakLength = becna,
                  Mass = poids, ObserverID = obs,
                  Destination_Aviary = dest, BirthPlot = orig,
                  LocationID = paste(lieu, nic, sep = "_"),
                  ActionTaken = dplyr::case_when(.$action == "bcj" ~ "Ringed",
                                                 .$action == "mor" ~ "Dead",
                                                 .$action == "nb" ~ "Not_Ringed",
                                                 .$action == "ct" ~ "Caught???"),
                  ObservedSex = dplyr::case_when(.$sex %in% c(1, 4) ~ "M",
                                                 .$sex %in% c(2, 5) ~ "F"),
                  GeneticSex = dplyr::case_when(.$sex_g == 1 ~ "M",
                                                .$sex_g == 2 ~ "F"),
                  CaptureMethod = dplyr::case_when(.$fil == "0" ~ "Nest box",
                                                   .$fil == "1" ~ "Mist net",
                                                   .$fil == "2" ~ "Trap cage and caller",
                                                   .$fil == "3" ~ "Winter roost"),
                  Age_observed = dplyr::case_when(.$age == "P0" ~ 1,
                                                 .$age == "J0" ~ 3,
                                                 .$age == "J1" ~ 5,
                                                 .$age == "A0" ~ 4,
                                                 .$age == "A1" ~ 6),
                  ExperimentID = dplyr::case_when(.$exp_ad == "FMR" ~ "Metabolic measurement",
                                                  .$exp_ad == "VACCIN" ~ "Vaccinated",
                                                  .$exp_ad == "OF" ~ "Openfield??",
                                                  .$exp_ad == "TRANSFERT" ~ "Translocation",
                                                  .$exp_ad == "COGNITION" ~ "Cognition test??"),
                  Status = dplyr::case_when(.$etat_sante %in% c("B", "BMAN") ~ "Injured",
                                            .$etat_sante %in% c("M", "MMAN") ~ "Dead",
                                            .$etat_sante == "D" ~ "Chick dead",
                                            .$etat_sante == "E" ~ "Healthy",
                                            .$etat_sante == "MAL" ~ "Sick"))


  #Format the Capture data to match the standard protocol
  #For now I'm just going to work with the data collected around Rouviere and Pirio (Corsica)
  #These are the areas I know there are long term populations
  #Will ask Anne about the other populations over Skype
  #Give them PopID COR (Corsica) and ROU (Rouviere)
  #Use the codes from within each population as Plot
  Full_capture_data <- Full_capture_data %>%
    dplyr::filter(lieu %in% c("cap", "mes", "pir", "tua", "rou")) %>%
    #Only include capture pop and plot for now, until we work out how to code translocations
    dplyr::mutate(CapturePopID = dplyr::case_when(.$lieu %in% c("cap", "mes", "pir", "tua") ~ "COR",
                                           .$lieu == "rou" ~ "ROU"),
                  CapturePlot = toupper(lieu), ReleasePopID = NA_character_, ReleasePlot = NA_character_,
                  Tarsus = as.numeric(tarsed), OriginalTarsusMethod = dplyr::case_when(!is.na(.$tarsed) ~ "Alternative")) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID,
                  CapturePopID, CapturePlot, ReleasePopID, ReleasePlot, Mass, Tarsus,
                  OriginalTarsusMethod, WingLength, Age_observed, ObservedSex, GeneticSex)



  #Do the same for the chick capture data
  #As above, we read all in as text and then coerce afterwards
  Chick_capture_data <- readxl::read_excel(paste0(db, "//SIE POUS 1976-2018.xlsx"),
                                           col_types = "text") %>%
    dplyr::mutate_at(.vars = vars(2, 15), as.integer) %>%
    dplyr::mutate_at(.vars = vars(16, 18:20), as.numeric) %>%
    dplyr::mutate(Species = dplyr::case_when(.$espece == "ble" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                             .$espece == "noi" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)],
                                             .$espece == "cha" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                             .$espece == "eto" ~ "STARLING",
                                             .$espece == "grp" ~ "UN-IDENTIFIED CREEPER",
                                             .$espece == "grpj" ~ "SHORT-TOED TREECREEPER",
                                             .$espece == "grpd" ~ "EURASIAN TREECREEPER",
                                             .$espece == "hup" ~ "CRESTED TIT",
                                             .$espece == "sit" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14790)],
                                             .$espece == "moi" ~ "UN-IDENTIFIED SPARROW",
                                             .$espece == "moid" ~ "HOUSE SPARROW",
                                             .$espece == "moif" ~ Species_codes$Code[which(Species_codes$SpeciesID == 15980)],
                                             .$espece == "non" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14400)])) %>%
    #Filter by species
    #Also remove only the pops we know
    dplyr::filter(Species %in% species_filter & lieu %in% c("cap", "mes", "pir", "tua", "rou")) %>%
    dplyr::mutate(CaptureDate = janitor::excel_numeric_to_date(as.numeric(date_mesure)),
                  BreedingSeason = as.integer(an), CaptureTime = heure,
                  IndvID = purrr::pmap_chr(.l = list(bague),
                                           .f = ~{

                                             if(grepl(pattern = "no-ident", ..1)){

                                               return(NA_character_)

                                             } else {

                                               return(..1)

                                             }

                                           }),
                  Mass = as.numeric(poids), ObserverID = obs,
                  ChickAge = as.integer(age_plume),
                  ActionTaken = dplyr::case_when(.$action == "bcj" ~ "Ringed",
                                                 .$action == "nb" ~ "Not_Ringed",
                                                 .$action == "ct" ~ "Caught???"),
                  ObservedSex = dplyr::case_when(.$sex %in% c(1, 4) ~ "M",
                                                 .$sex %in% c(2, 5) ~ "F"),
                  GeneticSex = dplyr::case_when(.$sex_g == 1 ~ "M",
                                                .$sex_g == 2 ~ "F"),
                  Age_observed = 1,
                  ExperimentID = dplyr::case_when(.$exp_p == "alimente" ~ "Supplemental feeding",
                                                  .$exp_p == "poussintron" ~ "Behavioural test",
                                                  .$exp_p == "poussintronfc" ~ "Behavioural test and heart rate",
                                                  .$exp_p == "transf" ~ "Translocation",
                                                  .$exp_p == "fc" ~ "Heart rate monitored",
                                                  .$exp_p == "biopsie" ~ "Underwent biopsy",
                                                  .$exp_p == "fmr" ~ "Doubly labled water",
                                                  .$exp_p == "voliere" ~ "Bred in aviary",
                                                  .$exp_p == "gavage" ~ "Chick force-feeding supplement",
                                                  .$exp_p == "inuline" ~ "Insulin injection"),
                  Status = dplyr::case_when(.$etat_sante == "M" ~ "Dead",
                                            .$etat_sante == "D" ~ "Chick dead",
                                            .$etat_sante == "E" ~ "Healthy"),
                  #Make LocationID Plot_BoxNumber
                  LocationID = paste(lieu, nic, sep = "_"),
                  #Make CapturePopID and plot. Leave Release NA for now until we work out how to code these
                  CapturePopID = dplyr::case_when(.$lieu %in% c("cap", "mes", "pir", "tua") ~ "COR",
                                                  .$lieu == "rou" ~ "ROU"),
                  CapturePlot = lieu,
                  ReleasePopID = NA_character_, ReleasePlot = NA_character_,
                  Tarsus = as.numeric(tarsed), OriginalTarsusMethod = dplyr::case_when(!is.na(.$tarsed) ~ "Alternative"),
                  WingLength = NA_real_) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, ObserverID, LocationID,
                  CapturePopID, CapturePlot, ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, ChickAge, ObservedSex, GeneticSex)

  Capture_data <- dplyr::bind_rows(Full_capture_data, Chick_capture_data)

  return(Capture_data)

}

#' Create brood data table for Montpellier
#'
#' @param db Location of primary data from Montpellier.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame with Brood data

create_brood_MON <- function(db, species_filter){

  Brood_data <- readxl::read_excel(paste0(db, "//SIE DEMO 1976-2018.xlsx"),
                                   col_types = "text") %>%
    dplyr::mutate(Species = dplyr::case_when(.$espece == "ble" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                             .$espece == "noi" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)],
                                             .$espece == "cha" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                             .$espece == "eto" ~ "STARLING",
                                             .$espece == "grp" ~ "UN-IDENTIFIED CREEPER",
                                             .$espece == "grpj" ~ "SHORT-TOED TREECREEPER",
                                             .$espece == "grpd" ~ "EURASIAN TREECREEPER",
                                             .$espece == "hup" ~ "CRESTED TIT",
                                             .$espece == "sit" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14790)],
                                             .$espece == "moi" ~ "UN-IDENTIFIED SPARROW",
                                             .$espece == "moid" ~ "HOUSE SPARROW",
                                             .$espece == "moif" ~ Species_codes$Code[which(Species_codes$SpeciesID == 15980)],
                                             .$espece == "non" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14400)]),
                  Plot = lieu, BoxNumber = nic,
                  LocationID = paste(Plot, BoxNumber, sep = "_"),
                  BreedingSeason = as.integer(an),
                  LayingDate = janitor::excel_numeric_to_date(as.numeric(date_ponte)),
                  BroodID = paste(BreedingSeason, LocationID,
                                  stringr::str_pad(lubridate::day(LayingDate), width = 2, pad = "0"),
                                  stringr::str_pad(lubridate::month(LayingDate), width = 2, pad = "0"), sep = "_"),
                  ClutchSize = as.integer(grpo),
                  HatchDate = janitor::excel_numeric_to_date(as.numeric(date_eclo)),
                  BroodSize = as.integer(pulecl),
                  NumberFledged = as.integer(pulenv),
                  CauseFailure = mort, MaleID = mbag,
                  FemaleID = fbag, ClutchType_observed = NA_character_,
                  LayingDateError = NA_integer_, ClutchSizeError = NA_integer_,
                  HatchDateError = NA_integer_, BroodSizeError = NA_integer_,
                  FledgeDate = as.Date(NA), FledgeDateError = NA_integer_,
                  NumberFledgedError = NA_integer_, ExperimentID = NA_character_) %>%
    dplyr::filter(Species %in% species_filter & Plot %in% c("cap", "mes", "pir", "tua", "rou")) %>%
    #Only include capture pop and plot for now, until we work out how to code translocations
    dplyr::mutate(PopID = dplyr::case_when(.$lieu %in% c("cap", "mes", "pir", "tua") ~ "COR",
                                           .$lieu == "rou" ~ "ROU")) %>%
    dplyr::arrange(BreedingSeason, Species, FemaleID, LayingDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    #Keep all chick codes because we will use these for individual data table and remove later
    #Keep box number to link to Capture data
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot,
                  LocationID, FemaleID, MaleID, ClutchType_observed,
                  ClutchType_calculated, LayingDate, LayingDateError,
                  ClutchSize, ClutchSizeError, HatchDate, HatchDateError,
                  BroodSize, BroodSizeError, FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError, ExperimentID,
                  pulbag1:pulbag14, BoxNumber)

  return(Brood_data)

}

#' Create individual data table for Montpellier
#'
#' @param capture_data Capture data generated by \code{\link{create_capture_MON}}.
#'
#' @return A data frame with Individual data

create_individual_MON <- function(capture_data){

  Individual_data <- capture_data %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = purrr::pmap_chr(.l = list(unique(Species)),
                                            .f = ~{

                                              if(length(na.omit(..1)) == 1){

                                                return(..1)

                                              } else {

                                                return("CONFLICTED")

                                              }

                                            }),
                  PopID = first(CapturePopID),
                  BroodIDLaid = NA_character_, BroodIDFledged = NA_character_,
                  RingSeason = first(BreedingSeason),
                  RingAge = purrr::pmap_chr(.l = list(first(Age_observed)),
                                            .f = ~{

                                              if(is.na(..1)){

                                                return(NA_character_)

                                              } else if(..1 <= 3){

                                                return("chick")

                                              } else if(..1 > 3){

                                                return("adult")

                                              }

                                            }),
                  Sex = purrr::pmap_chr(.l = list(list(unique(ObservedSex)), list(unique(GeneticSex))),
                                        .f = ~{

                                          #Firstly, check genetic sex results
                                          #Are there any non-NA cases?
                                          if(length(na.omit(..2)) > 0){

                                            #If there's just one sex record return it
                                            if(length(na.omit(..2)) == 1){

                                              return(na.omit(..2))

                                            #Otherwise, check if we can use observed sex instead
                                            } else {

                                              #If there is one record of observed sex use this
                                              if(length(na.omit(..1)) == 1){

                                                return(na.omit(..1))

                                              #Otherwise, we need to say the sex is conflicted
                                              } else {

                                                return("C")

                                              }

                                            }

                                          #If there is no genetic sex
                                          } else {

                                            #Check if there are any non-NA observed records
                                            if(length(na.omit(..1)) > 0){

                                              #If there is just one, return that
                                              if(length(na.omit(..1)) == 1){

                                                return(na.omit(..1))

                                              } else {

                                                #Otherwise it is conflicted
                                                return("C")

                                              }

                                            } else {

                                              #If no sex data is available, return NA
                                              return(NA_character_)

                                            }

                                          }

                                        }))

  return(Individual_data)

}


