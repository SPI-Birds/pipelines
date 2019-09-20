#'Construct standard format for data from Montpellier, France.
#'
#'A pipeline to produce the standard format for the hole nesting bird population
#'in Montpellier, France, administered by CNRS Montpellier (Anne Charmantier and colleagues).
#'
#'This section provides details on data management choices that are unique to
#'this data. For a general description of the standard protocl please see
#'\href{https://github.com/LiamDBailey/SPIBirds_Newsletter/blob/master/SPI_Birds_Protocol_v1.0.0.pdf}{here}.
#'
#'\strong(Sex:) We ignore uncertainty in sex (e.g. category 4 = Male probable but not certain)
#'
#'\strong(LocationID:) For birds caught in boxes. Location is Plot_NextboxNumber.
#'
#'\strong(Tarsus:) Left and right tarsus are measured. Right generally has more data, so we use this as our
#'measure of tarsus length. Currently, we assume everything is in Svensson's alternative, but there is supposedly
#'some change from before 1989. Need to ask Anne about this.
#'
#'\strong(Age:) We translate observed age codes into EURING codes as follows:
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

}


#' Title
#'
#' @param db
#' @param species_filter
#'
#' @return
#' @export
#'
#' @examples
create_capture_MON <- function(db, species_filter){

  Full_capture_data <- readxl::read_excel(paste0(db, "//", "SIE MORPH 1976-2018.xlsx"),
                                     col_types = c("text")) %>%
    #There are two potential coercion issues in the data
    #1. Numbers are stored as text in the excel sheets. This isn't so problematic, because
    #they are easily coerced back to numerics, but it causes a lot of warnings which
    #prevents us from seeing any real coercion errors that cause NAs (e.g. number stored as 10,1).
    #We read them in as text and coerce them in R instead (where only NA coercion throws a warning).
    #This also allows us to distinguish between integer and numeric cols.
    #2. There is one typo in date (03/05/20111). We read dates in and text and then
    #coerce to date only the first 10 characters.
    #Firstly, we rename cols so it's easier for users to understand
    dplyr::rename(BreedingSeason = an, CaptureDate = date_mesure, CaptureTime = heure,
                  Species = espece, IndvID = bague, WingLength = aile,
                  TarsusRight = tarsed, TarsusLeft = tarseg, BeakLength = becna,
                  Mass = poids, ObserverID = obs,
                  Destination_Aviary = dest, BirthPlot = orig) %>%
    dplyr::mutate_at(.vars = vars(3, 7, 16, 37), as.integer) %>%
    dplyr::mutate_at(.vars = vars(5, 6, 14, 18:24, 26, 27, 35), as.numeric) %>%
    dplyr::mutate(CaptureDate = as.Date(substr(CaptureDate, start = 1, stop = 10), format = "%d/%m/%Y")) %>%
    #Add standard species codes
    dplyr::mutate(Species = dplyr::case_when(.$Species == "ble" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                             .$Species == "noi" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)],
                                             .$Species == "cha" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                             .$Species == "eto" ~ "STARLING",
                                             .$Species == "grp" ~ "UN-IDENTIFIED CREEPER",
                                             .$Species == "grpj" ~ "SHORT-TOED TREECREEPER",
                                             .$Species == "grpd" ~ "EURASIAN TREECREEPER",
                                             .$Species == "hup" ~ "CRESTED TIT",
                                             .$Species == "sit" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14790)],
                                             .$Species == "moi" ~ "UN-IDENTIFIED SPARROW",
                                             .$Species == "moid" ~ "HOUSE SPARROW",
                                             .$Species == "moif" ~ Species_codes$Code[which(Species_codes$SpeciesID == 15980)],
                                             .$Species == "non" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14400)])) %>%
    #Filter by species
    dplyr::filter(Species %in% species_filter) %>%
    dplyr::mutate(ActionTaken = dplyr::case_when(.$action == "bcj" ~ "Ringed",
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
                  AgeObserved = dplyr::case_when(.$age == "P0" ~ 1,
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
                  Tarsus = TarsusRight, OriginalTarsusMethod = dplyr::case_when(!is.na(.$TarsusRight) ~ "Alternative"))



  #Do the same for the chick capture data
  #As above, we read all in as text and then coerce afterwards
  Chick_capture_data <- readxl::read_excel(paste0(db, "//", "SIE POUS 1976-2018.xlsx"),
                                           col_types = c("text")) %>%
    dplyr::rename(BreedingSeason = an, CaptureDate = date_mesure, CaptureTime = heure,
                  Species = espece, IndvID = bague,
                  TarsusRight = tarsed, TarsusLeft = tarseg,
                  Mass = poids, ObserverID = obs,
                  ChickAge = age_plume) %>%
    dplyr::mutate_at(.vars = vars(2, 15), as.integer) %>%
    dplyr::mutate_at(.vars = vars(16, 18:20), as.numeric) %>%
    dplyr::mutate(CaptureDate = as.Date(substr(CaptureDate, start = 1, stop = 10), format = "%d/%m/%Y")) %>%
    #Add standard species codes
    dplyr::mutate(Species = dplyr::case_when(.$Species == "ble" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14620)],
                                             .$Species == "noi" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14610)],
                                             .$Species == "cha" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14640)],
                                             .$Species == "eto" ~ "STARLING",
                                             .$Species == "grp" ~ "UN-IDENTIFIED CREEPER",
                                             .$Species == "grpj" ~ "SHORT-TOED TREECREEPER",
                                             .$Species == "grpd" ~ "EURASIAN TREECREEPER",
                                             .$Species == "hup" ~ "CRESTED TIT",
                                             .$Species == "sit" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14790)],
                                             .$Species == "moi" ~ "UN-IDENTIFIED SPARROW",
                                             .$Species == "moid" ~ "HOUSE SPARROW",
                                             .$Species == "moif" ~ Species_codes$Code[which(Species_codes$SpeciesID == 15980)],
                                             .$Species == "non" ~ Species_codes$Code[which(Species_codes$SpeciesID == 14400)])) %>%
    #Filter by species
    #Also remove only the pops we know
    dplyr::filter(Species %in% species_filter & lieu %in% c("cap", "mes", "pir", "tua", "rou")) %>%
    dplyr::mutate(ActionTaken = dplyr::case_when(.$action == "bcj" ~ "Ringed",
                                                 .$action == "nb" ~ "Not_Ringed",
                                                 .$action == "ct" ~ "Caught???"),
                  ObservedSex = dplyr::case_when(.$sex %in% c(1, 4) ~ "M",
                                                 .$sex %in% c(2, 5) ~ "F"),
                  GeneticSex = dplyr::case_when(.$sex_g == 1 ~ "M",
                                                .$sex_g == 2 ~ "F"),
                  AgeObserved = 1,
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
                  CapturePlot = toupper(lieu),
                  ReleasePopID = NA_character_, ReleasePlot = NA_character_,
                  Tarsus = TarsusRight, OriginalTarsusMethod = dplyr::case_when(!is.na(.$TarsusRight) ~ "Alternative"),
                  WingLength = NA_real_)


}


