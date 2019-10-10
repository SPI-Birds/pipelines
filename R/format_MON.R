#'Construct standard format for data from Montpellier, France.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Montpellier, France, administered by CNRS Montpellier (Anne Charmantier and
#'colleagues).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong{IndvID:} In Capture_data, any individuals given unidentified numbers
#'(i.e. containing 'no-ident') are treated as NA. These records are still kept
#'as they contain information about broods (e.g. AvgChickMass) but are not
#'integrated into the individual data table.
#'
#'\strong{Sex:} We ignore uncertainty in sex (e.g. category 4 = Male probable
#'but not certain)
#'
#'\strong{LocationID:} For birds caught in boxes. Location is
#'Plot_NextboxNumber.
#'
#'\strong{BroodID:} Broods are BreedingSeason_LocationID_ClutchTypeObserved =
#'BreedingSeason_Plot_NestboxNumber_ClutchTypeObserved.
#'
#'\strong{ClutchTypeObserved:} No clutch type recorded, only calculated clutch
#'type is given.
#'
#'\strong{Tarsus:} Left and right tarsus are measured. Right generally has more
#'data, so we use this as our measure of tarsus length. Currently, we assume
#'everything is in Svensson's alternative, but there is supposedly some change
#'from before 1989. Need to ask Anne about this.
#'
#'\strong{Age:} We translate observed age codes into EURING codes as follows:
#'\itemize{
#'
#'\item P0 (Poussin bagué au nichoir ou en cavité naturelle/chick banded in box
#'or natural cavity): We give EURING code 1: Nestling or chick unable to fly
#'freely.
#'
#'\item J0 (Juvénile (oiseau de l’année) capturé entre le moment où il s’est
#'envolé du nichoir et le 31 décembre) (Juvenile (of this year) captured between
#'fledging and Dec 31st of that year) We give EURING code 3: First-year:
#'full-grown bird hatched in the breeding season of this calendar year.
#'
#'\item PN (where N > 0) Individuals caught as chick and caught again N seasons
#'later We give EURING code 3 + 2*N (i.e. of known age because it was caught
#'when it could be accurately aged)
#'
#'\item J1 (Juvénile (oiseau de l’année dernière) capturé après le 1er janvier
#'et avant la mue post reproduction (juillet-août)) (Juvenile (of last year)
#'capture between Jan 1st and July-Aug) We give EURING code 5: 2nd year: a bird
#'hatched last calendar year and now in its second calendar year.
#'
#'\item JN (where N > 1) Individuals caught as juvenile and caught again N
#'season later We give EURING code 3 + 2*N: Nestling or chick unable to fly
#'freely.
#'
#'\item AN (where N >= 0) An adult not captured as a chick or juvenile. We give
#'EURING code 4 + 2*N (i.e. of unknown age because it was not caught when it
#'could be accurately aged).
#'
#'\item IN (where N > 1): An adult not captured as a chick or juvenile. We give
#'EURING code 4 + 2*1-N (i.e. of unknown age because it was not caught when it
#'could be accurately aged). }
#'
#'\strong{Age_calculated:} We strictly say that we canot accurately know the age
#'of an individual unless they were caught in their first year. Therefore,
#'although birds classified as J1 can be aged very accurately, we still treat
#'these as having age uncertain because they weren't caught as chicks.
#'
#'\strong{BroodIDLaid/Fledged:} BroodIDLaid is the brood in which an individual
#'was listed in one of the chick columns. The BroodIDFledged is the same as BroodIDLaid
#'unless a destination brood is listed. In any cases where multiple broods are associated
#'with an individual (e.g. they are listed as a chick twice) we currently select the
#'first record and return a warning. We assume the first nest recorded is more likely to
#'be the 'true' nest until these are corrected.
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
                       output_type = "csv",
                       verbose = FALSE){

  #Force user to select directory
  force(db)

  #Determine species codes for filtering
  if(is.null(species)){

    species <- Species_codes$Code

  }

  if(is.null(pop)){

    pop <- c("MUR", "PIR", "ROU", "MON", "MTV", "MIS", "aviary")

  }


  #Record start time to estimate processing time.
  start_time <- Sys.time()

  # CAPTURE DATA

  message("Compiling capture data....")

  Capture_data <- create_capture_MON(db = db, species_filter = species, pop_filter = pop)

  # BROOD DATA

  message("Compiling brood data...")

  Brood_data <- create_brood_MON(db = db, species_filter = species, pop_filter = pop)

  # INDIVIDUAL DATA

  message("Compiling individual data...")

  Individual_data <- create_individual_MON(Capture_data, Brood_data, verbose)

  # LOCATION DATA

  message("Compiling location data...")

  Location_data <- create_location_MON(Capture_data)

  # WRANGLE DATA FOR EXPORT

  Capture_data <- Capture_data %>% dplyr::select(IndvID:ChickAge)

  #Remove chick rings from brood data
  #Add NAs for Avg measurements until we add them later
  Brood_data <- dplyr::select(Brood_data, -pulbag1:-pulbag14) %>%
    dplyr::mutate(AvgEggMass = NA_real_, NumberEggs = NA_integer_,
                  AvgChickMass = NA_real_, NumberChicksMass = NA_integer_,
                  AvgTarsus = NA_real_, NumberChicksTarsus = NA_integer_) %>%
    dplyr::select(BroodID:NumberFledgedError, AvgEggMass:NumberChicksTarsus, ExperimentID)

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

    utils::write.csv(x = Location_data, file = paste0(path, "\\Location_data_MON.csv"), row.names = F)

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

#' Create capture data table for Montpellier
#'
#' @param db Location of primary data from Montpellier.
#' @param species_filter Species of interest. The 6 letter codes of all the species of
#'  interest as listed in the
#'  \href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{standard
#'  protocol}.
#'
#' @return A data frame with capture data

create_capture_MON <- function(db, species_filter, pop_filter){

  Full_capture_data <- readxl::read_excel(paste0(db, "//MON_PrimaryData_MORPH.xlsx"),
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
                  CaptureTime = dplyr::na_if(paste(stringr::str_pad((24*as.numeric(heure)) %/% 1, width = 2, pad = "0"),
                                      stringr::str_pad(round(((24*as.numeric(heure)) %% 1) * 60), width = 2, pad = "0"),
                                      sep = ":"), "NA:NA"),
                  BreedingSeason = an, CaptureTime = heure,
                  IndvID = bague, WingLength = aile,
                  BeakLength = becna,
                  Mass = poids, ObserverID = obs,
                  Destination = dest,
                  LocationID = paste(lieu, nic, sep = "_"),
                  ActionTaken = dplyr::case_when(.$action == "bcj" ~ "Ringed",
                                                 .$action == "mor" ~ "Dead",
                                                 .$action == "nb" ~ "Not_Ringed",
                                                 .$action == "ct" ~ "Caught (already ringed)"),
                  ObservedSex = dplyr::case_when(.$sex %in% c(1, 4) ~ "M",
                                                 .$sex %in% c(2, 5) ~ "F"),
                  GeneticSex = dplyr::case_when(.$sex_g == 1 ~ "M",
                                                .$sex_g == 2 ~ "F"),
                  CaptureMethod = dplyr::case_when(.$fil == "0" ~ "Nest box",
                                                   .$fil == "1" ~ "Mist net",
                                                   .$fil == "2" ~ "Trap cage and caller",
                                                   .$fil == "3" ~ "Winter roost"),
                  Age_observed = purrr::pmap_int(.l = list(age),
                                                 .f = ~{

                                                   if(is.na(..1)){

                                                     return(NA_integer_)

                                                   } else {

                                                     split_age <- strsplit(..1, split = "")

                                                     letter <- split_age[[1]][1]
                                                     number <- as.integer(split_age[[1]][2])

                                                     if(letter == "P"){

                                                       if(number == 0L){

                                                         return(1L)

                                                       } else {

                                                         return(3L + 2L*number)

                                                       }

                                                     } else if(letter == "J"){

                                                       return(3L + 2L*number)

                                                     } else if(letter == "A"){

                                                       return(4L + 2L*number)

                                                     } else if(letter == "I"){

                                                       return(4L + 2L*(number - 1L))

                                                     }

                                                   }

                                                 }),
                  ExperimentDescription1 = dplyr::case_when(.$exp_ad == "FMR" ~ "Metabolic measurement",
                                                  .$exp_ad == "VACCIN" ~ "Vaccinated",
                                                  .$exp_ad == "OF" ~ "Openfield??",
                                                  .$exp_ad == "TRANSFERT" ~ "Translocation",
                                                  .$exp_ad == "COGNITION" ~ "Cognition test??"),
                  Status = dplyr::case_when(.$etat_sante %in% c("B", "BMAN") ~ "Injured",
                                            .$etat_sante %in% c("M", "MMAN") ~ "Dead",
                                            .$etat_sante == "D" ~ "Chick dead",
                                            .$etat_sante == "E" ~ "Healthy",
                                            .$etat_sante == "MAL" ~ "Sick"),
                  FoundDead = dplyr::case_when(.$etat_sante %in% c("M", "MMAN") ~ TRUE,
                                               .$action == "mor" ~ TRUE))


  #Format the Capture data to match the standard protocol
  #There are multiple populations within this dataset these are
  # Muro (Corsica), including a deciduous and evergreen sub-population
  # Pirio (Corsica), evergreen only
  # Rouviere (mainly deciduous)
  # Montpellier city (urban habitat)
  # Mont Ventoux (only laying date and clutch size recorded)
  # Remnant populations <- these are not one biological population but are grouped together

  Full_capture_data <- Full_capture_data %>%
    #If there is information in the 'Destination' column, add a different value for ReleasePopID/Plot
    dplyr::mutate(ReleasePlot = purrr::map2_chr(Destination, lieu, .f = ~{

      if(is.na(..1)){

        return(..2)

      } else {

        if(grepl(x = ..1, pattern = "voliére|volière")){

          return("aviary")

        } else {

          return(..1)

        }

      }

    })) %>%
    #Only include capture pop and plot for now, until we work out how to code translocations
    dplyr::mutate(CapturePopID = identify_PopID_MON(lieu),
                  ReleasePopID = identify_PopID_MON(ReleasePlot),
                  CapturePlot = lieu,
                  Tarsus = as.numeric(tarsed), OriginalTarsusMethod = dplyr::case_when(!is.na(.$tarsed) ~ "Alternative")) %>%
    dplyr::filter(CapturePopID %in% pop_filter) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, FoundDead, ObserverID, LocationID,
                  CapturePopID, CapturePlot, ReleasePopID, ReleasePlot, Mass, Tarsus,
                  OriginalTarsusMethod, WingLength, Age_observed, ObservedSex, GeneticSex, ExperimentDescription1, latitude, longitude, CaptureMethod)



  #Do the same for the chick capture data
  #As above, we read all in as text and then coerce afterwards
  Chick_capture_data <- readxl::read_excel(paste0(db, "//MON_PrimaryData_POUS.xlsx"),
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
    dplyr::filter(Species %in% species_filter) %>%
    dplyr::mutate(CaptureDate = janitor::excel_numeric_to_date(as.numeric(date_mesure)),
                  CaptureTime = dplyr::na_if(paste(stringr::str_pad((24*as.numeric(heure)) %/% 1, width = 2, pad = "0"),
                                                   stringr::str_pad(round(((24*as.numeric(heure)) %% 1) * 60), width = 2, pad = "0"),
                                                   sep = ":"), "NA:NA"),
                  BreedingSeason = an, CaptureTime = heure,
                  IndvID = purrr::pmap_chr(.l = list(bague),
                                           .f = ~{

                                             if(grepl(pattern = "no-ident", x = ..1)){

                                               return(NA_character_)

                                             } else {

                                               return(..1)

                                             }

                                           }),
                  Mass = poids, ObserverID = obs,
                  ChickAge = as.integer(age_plume),
                  ObservedSex = dplyr::case_when(.$sex %in% c(1, 4) ~ "M",
                                                 .$sex %in% c(2, 5) ~ "F"),
                  GeneticSex = dplyr::case_when(.$sex_g == 1 ~ "M",
                                                .$sex_g == 2 ~ "F"),
                  Age_observed = 1L,
                  ExperimentDescription1 = dplyr::case_when(.$expou == "1" ~ "Manipulation affect selective value (e.g. cross foster)",
                                                  .$expou == "2" ~ "No manipulation that affects selective value (e.g. weighing)"),
                  ExperimentDescription2 = dplyr::case_when(.$exp_p == "alimente" ~ "Supplemental feeding",
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
                  FoundDead = dplyr::case_when(.$etat_sante %in% c("M", "MMAN", "D") ~ TRUE),
                  LocationID = paste(lieu, nic, sep = "_"),
                  CapturePopID = identify_PopID_MON(lieu),
                  CapturePlot = lieu,
                  #ReleasePopID/Plot are NA for now until we work out how to include cross-foster
                  ReleasePopID = NA_character_, ReleasePlot = NA_character_,
                  Tarsus = as.numeric(tarsed),
                  OriginalTarsusMethod = dplyr::case_when(!is.na(.$tarsed) ~ "Alternative"),
                  WingLength = NA_real_,
                  OrigBoxNumber = purrr::map(orig, find_box), DestBoxNumber = purrr::map(dest, find_box),
                  BroodIDLaid = purrr::pmap_chr(.l = list(BreedingSeason, lieu, OrigBoxNumber),
                                                .f = ~{

                                                  if(length(..3) == 1 && is.na(..3)){

                                                    return(NA_character_)

                                                  } else {

                                                    if(length(..3) > 1){

                                                      return(paste(..1, ..3[1], ..3[2], sep = "_"))

                                                    } else {

                                                      return(paste(..1, ..2, ..3, sep = "_"))

                                                    }

                                                  }

                                                }),
                  BroodIDFledged = purrr::pmap_chr(.l = list(BreedingSeason, lieu, DestBoxNumber),
                                                   .f = ~{

                                                     if(length(..3) == 1 && is.na(..3)){

                                                       return(NA_character_)

                                                     } else {

                                                       if(length(..3) == 2){

                                                         return(paste(..1, ..3[1], ..3[2], sep = "_"))

                                                       } else {

                                                         return(paste(..1, ..2, ..3, sep = "_"))

                                                       }

                                                     }

                                                   })) %>%
    #If there is a destination box, give it a different ReleasePlot/PopID
    dplyr::mutate(ReleasePlot = purrr::map2_chr(.x = DestBoxNumber, .y = CapturePlot, ~{

      if(length(..1) == 2){

        return(..1[1])

      } else {

        return(..2)

      }

    }), ReleasePopID = identify_PopID_MON(ReleasePlot)) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, FoundDead, ObserverID, LocationID,
                  CapturePopID, CapturePlot, ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, ChickAge, ObservedSex, GeneticSex, ExperimentDescription1, ExperimentDescription2, BroodIDLaid, BroodIDFledged)

  Capture_data <- dplyr::bind_rows(Full_capture_data, Chick_capture_data) %>%
    calc_age(ID = IndvID, Age = Age_observed, Date = CaptureDate, Year = BreedingSeason) %>%
    dplyr::select(IndvID, Species, BreedingSeason, CaptureDate, CaptureTime, FoundDead, ObserverID, LocationID,
                  CapturePopID, CapturePlot, ReleasePopID, ReleasePlot, Mass, Tarsus, OriginalTarsusMethod,
                  WingLength, Age_observed, Age_calculated, ChickAge, ObservedSex, GeneticSex,
                  BroodIDLaid, BroodIDFledged, latitude, longitude, CaptureMethod, ExperimentDescription1, ExperimentDescription2)

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

create_brood_MON <- function(db, species_filter, pop_filter){

  Brood_data <- readxl::read_excel(paste0(db, "//MON_PrimaryData_DEMO.xlsx"),
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
                  LayDate = janitor::excel_numeric_to_date(as.numeric(date_ponte)),
                  BroodID = paste(BreedingSeason, Plot, BoxNumber, np,
                                  sep = "_"),
                  ClutchType_observed = dplyr::case_when(.$np == "1" ~ "first",
                                                         .$np == "2" ~ "second",
                                                         .$np == "3" ~ "replacement"),
                  ClutchSize = as.integer(grpo),
                  HatchDate = janitor::excel_numeric_to_date(as.numeric(date_eclo)),
                  BroodSize = as.integer(pulecl),
                  NumberFledged = as.integer(pulenv),
                  CauseFailure = mort, MaleID = mbag,
                  FemaleID = fbag,
                  ParasiteTreatment = dplyr::case_when(.$extnt == "1" ~ "Protected",
                                                       .$extnt == "2" ~ "Unprotected"),
                  Brood_ExperimentDescription1 = dplyr::case_when(.$expou == "1" ~ "No intervention that will affect breeding success",
                                                  .$expou == "2" ~ "Intervention that can influence number of eggs or quality of chicks"),
                  Brood_ExperimentDescription2 = experience,
                  Crossfostering_treatment = explique,
                  NumberParasites = proto,
                  FailureCause = dplyr::case_when(.$mort == "ABA" ~ "Abandoned",
                                                  .$mort == "MAN" ~ "Manipulated by researchers",
                                                  .$mort == "PRE" ~ "Predation",
                                                  .$mort == "CLI" ~ "Climatic event (e.g. storm)",
                                                  .$mort == "MAL" ~ "Sickness",
                                                  .$mort == "NCT" ~ "Not checked??"),
                  LayDateError = NA_integer_, ClutchSizeError = NA_integer_,
                  HatchDateError = NA_integer_, BroodSizeError = NA_integer_,
                  FledgeDate = as.Date(NA), FledgeDateError = NA_integer_,
                  NumberFledgedError = NA_integer_) %>%
    dplyr::filter(Species %in% species_filter) %>%
    #Only include capture pop and plot for now, until we work out how to code translocations
    dplyr::mutate(PopID = identify_PopID_MON(lieu)) %>%
    dplyr::arrange(BreedingSeason, Species, FemaleID, LayDate) %>%
    dplyr::mutate(ClutchType_calculated = calc_clutchtype(data = ., na.rm = FALSE)) %>%
    dplyr::mutate(ExperimentID = dplyr::case_when((!is.na(.$Crossfostering_treatment) | !is.na(.$Brood_ExperimentDescription2) | .$ParasiteTreatment == "Treated" | .$expou == "2") ~ "TRUE",
                                                  (is.na(.$Crossfostering_treatment) & is.na(.$Brood_ExperimentDescription2) & .$ParasiteTreatment != "Treated" & .$expou != "2") ~ "FALSE")) %>%
    dplyr::filter(PopID %in% pop_filter) %>%
    #Keep all chick codes because we will use these for individual data table and remove later
    #Keep box number to link to Capture data
    dplyr::select(BroodID, PopID, BreedingSeason, Species, Plot,
                  LocationID, FemaleID, MaleID, ClutchType_observed,
                  ClutchType_calculated, LayDate, LayDateError,
                  ClutchSize, ClutchSizeError, HatchDate, HatchDateError,
                  BroodSize, BroodSizeError, FledgeDate, FledgeDateError,
                  NumberFledged, NumberFledgedError, ExperimentID,
                  pulbag1:pulbag14, BoxNumber)

  return(Brood_data)

}

#' Create individual data table for Montpellier
#'
#' @param capture_data Capture data generated by \code{\link{create_capture_MON}}.
#' @param brood_data Capture data generated by \code{\link{create_brood_MON}}.
#'
#' @return A data frame with Individual data

create_individual_MON <- function(capture_data, brood_data, verbose){

  BroodAssignment <- brood_data %>%
    dplyr::select(BroodIDLaid = BroodID, pulbag1:pulbag14) %>%
    tidyr::pivot_longer(cols = pulbag1:pulbag14, names_to = "ChickNr", values_to = "IndvID") %>%
    dplyr::select(-ChickNr) %>%
    #Remove broods where there were no ringed chicks
    dplyr::filter(!is.na(IndvID))

  #Identify any cases where a individual was cross-fostered
  #It had info in the orig and/or dest column
  Cross_foster <- capture_data %>%
    dplyr::filter(!is.na(BroodIDLaid) | !is.na(BroodIDFledged))

  #Create progress bar to track cross-foster assignment
  pb <- dplyr::progress_estimated(n = nrow(Cross_foster))

  Cross_foster  <- Cross_foster %>%
    dplyr::select(IndvID, LocationID, CaptureDate, BroodIDLaid, BroodIDFledged) %>%
    #Run through each example and determine the destination
    #There are three possible situations
    #1. Individual caught in the genetic brood where the destionation brood is listed
    #2. Individual caught in the foster brood where the origin brood is listed
    #3. Individual with both origin and desination brood listed
    #Use apply because it doesn't coerce dates to numbers!!!!!
    apply(1, function(x, brood_data){

      pb$tick()$print()

      #If the destination brood has been given
      if(!is.na(x["BroodIDFledged"])){

        split_info <- unlist(stringr::str_split(x["BroodIDFledged"], pattern = "_"))
        CaptureDate <- as.Date(x["CaptureDate"])

        #If it has been transferred to an aviary, list this
        if(split_info[1] == "volièreMontpellier"){

          return(tibble(IndvID = x["IndvID"], BroodIDFledged = "aviary"))

        }

        #Find the nest that was active in the period of capture
        possible_nest <- brood_data %>%
          dplyr::filter(BreedingSeason == split_info[1],
                        Plot == split_info[2],
                        BoxNumber == split_info[3],
                        LayDate < CaptureDate)

        if(nrow(possible_nest) == 1){

          return(tibble::tibble(IndvID = x["IndvID"], BroodIDFledged = possible_nest$BroodID))

        } else if(nrow(possible_nest) > 1 & length(unique(possible_nest$BoxNumber)) == 1){

          possible_nest <- possible_nest %>%
            dplyr::slice(n())

        } else {

          if(verbose){

            print(x["IndvID"])
            print(CaptureDate)
            print(split_info)
            print(possible_nest, width = Inf)

            message("CROSS-FOSTERING DESTINATION RETURNS ERROR (MON DATA). This may be because more than one cross-fostering event
               is listed, or the destination nest doesn't exist. This record is skipped")

          }

          return(NULL)

        }

      } else if(!is.na(x["BroodIDLaid"])){

        split_info <- unlist(stringr::str_split(x["LocationID"], pattern = "_"))
        CaptureDate <- as.Date(x["CaptureDate"])

        #Find the nest that was active in the period of capture
        possible_nest <- brood_data %>%
          dplyr::filter(BreedingSeason == lubridate::year(CaptureDate),
                        Plot == split_info[1],
                        BoxNumber == split_info[2],
                        LayDate < CaptureDate)

        if(nrow(possible_nest) > 1){

          stop("MORE THAN ONE POSSIBLE NEST IDENTIFIED FOR CROSS-FOSTERING (MON DATA)")

        } else {

          return(tibble::tibble(IndvID = x["IndvID"], BroodIDFledged = possible_nest$BroodID))

        }

      }

    }, brood_data) %>%
   dplyr::bind_rows()

  Individual_data <- capture_data %>%
    dplyr::arrange(IndvID, CaptureDate) %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(Species = purrr::map_chr(.x = list(na.omit(unique(Species))),
                                            .f = ~{

                                              if(length(..1) == 1){

                                                return(..1)

                                              } else {

                                                return("CONFLICTED")

                                              }

                                            }),
                  PopID = first(CapturePopID),
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

  #Join in Brood data for all individuals
  Individual_data <- dplyr::left_join(Individual_data, BroodAssignment, by = "IndvID") %>%
    #Join in cross-fostering info
    dplyr::left_join(dplyr::select(Cross_foster, IndvID, BroodIDFledged), by = "IndvID") %>%
    #If no BroodIDFledged is listed, make it the same as BroodIDLaid
    dplyr::mutate(n = 1:n()) %>%
    dplyr::group_by(n) %>%
    dplyr::mutate(BroodIDFledged = ifelse(is.na(BroodIDFledged), BroodIDLaid, BroodIDFledged)) %>%
    dplyr::ungroup() %>%
    #Remove duplicates that will occur due to multiple values being in dest/orig
    dplyr::filter(!duplicated(.)) %>%
    dplyr::select(IndvID, Species, PopID, BroodIDLaid, BroodIDFledged, RingSeason, RingAge, Sex)

  #Identify those cases where an individual has multiple records in individual data
  duplicates <- Individual_data %>%
    dplyr::group_by(IndvID) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::filter(n > 1) %>%
    dplyr::pull(IndvID)

  purrr::pwalk(.l = list(duplicates),
               ~{

                 warning(glue::glue("Individual {duplicate} has more than one potential BroodID", duplicate = ..1))

               })



  #For any duplicate cases, we just take the first, which should be the most recent one
  Individual_data <- Individual_data %>%
    dplyr::group_by(IndvID) %>%
    dplyr::slice(1)

  return(Individual_data)

}

#' Create location data table for Montpellier
#'
#' @param capture_data Capture data generated by \code{\link{create_capture_MON}}.
#'
#' @return A data frame with Individual data

create_location_MON <- function(capture_data){

  #Take all unique locations where a capture of a chick or adult occurred
  #Currently give not LocationType, I have no idea how to determine this
  Location_data <- capture_data %>%
    dplyr::group_by(LocationID) %>%
    dplyr::summarise(NestboxID = unique(LocationID),
                     LocationType = NA_character_,
                     PopID = unique(CapturePopID),
                     Latitude = as.numeric(first(latitude)),
                     Longitude = as.numeric(first(longitude)),
                     StartSeason = min(BreedingSeason),
                     EndSeason = NA_integer_,
                     Habitat = NA_character_)

}

identify_PopID_MON <- function(variable){

  dplyr::case_when(variable %in% c("ava", "fel", "mur", "fil", "ari", "gra") ~ "MUR",
                   variable %in% c("pir", "tua") ~ "PIR",
                   variable == "rou" ~ "ROU",
                   variable %in% c("bot", "cef", "fac", "font",
                                 "gram", "mas", "mos", "val", "vol", "zoo") ~ "MON",
                   variable == "ven" ~ "MTV",
                   variable %in% c("hs", "aul", "mes", "aig", "bon", "cap", "crt", "gen", "mal",
                                 "mau", "mrt", "olm", "pac", "pie", "pog", "pon",
                                 "pre", "pue", "sfl", "stb", "tcb", "tcv", "vic") ~ "MIS",
                   grepl(x = variable, pattern = "voliére|volière|aviary") ~ "aviary")

}

find_box <- function(string, position = 1){

  if(is.na(string) | position == (nchar(string) + 1)){

    return(NA_character_)

  }

  split_string <- strsplit(string, "")[[1]]

  if(is.na(suppressWarnings(as.numeric(split_string[position])))){

    return(find_box(string = string, position = position + 1))

  } else {

    if(position == 1){

      return(paste(split_string[position:nchar(string)], collapse = ""))

    } else {

      return(c(paste(split_string[1:(position-1)], collapse = ""),
               paste(split_string[position:nchar(string)], collapse = "")))

    }

  }

}


