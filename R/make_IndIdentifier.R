#Use N to create new colnames (e.g. RING3, RING4)

#' Make a summary table containing all ring numbers of re-ringed individual
#'
#' Assigns new unique identifiers to individuals that appear with two or more
#' different ring numbers in a data set. The resulting table can then be used
#' to replace all alternative ring numbers of an individual with the new
#' identifier in all tables contained in the standard format.
#'
#' @param raw_data Data frame. Contains pairs of rings (one pair per
#' re-ringing event). Column 'RING' = first ring, column 'RING2' = second ring.
#'
#' @return A data frame with columns 'RingNr' (= original ring number) and
#' 'Identifier' (= new unique integer identifier for the individual).
#'
#' @export
#'
#' @examples
#' #Create fake dataset
#' dat <- data.frame(RING = c('A1', 'B2', 'C7', 'D7'), RING2 = c('B9', 'C7', 'E3', NA))
#' #Summarise re-ringing information by individual
#' make_IndIdentifier(raw_data = dat)

make_IndIdentifier = function(raw_data){

  #First, we need make sure any first ring ("RING") only appears once in the data.
  #(This involves removing pairings of re-curring rings with NA for RING2)
  duplicateRING <- c(raw_data[which(duplicated(raw_data$RING)),'RING'], raw_data[which(raw_data$RING %in% raw_data$RING2), 'RING'])

  raw_data <- raw_data %>%
    dplyr::filter(!(.data$RING %in% duplicateRING & is.na(.data$RING2)))

  #We will be left joining one set of data to the other
  #so we need to make sure the column names match
  check_data  <- raw_data %>%
    dplyr::rename("RING2" = .data$RING, "RING3" = .data$RING2)

  #Create an output_data object that will be updated
  output_data <- raw_data

  #As long as any new ring values (e.g. RING2) are also used as an old ring value (e.g. RING)
  #Then we need to keep going
  #We use ncol() so that the code is robust to any number of re-ringings
  N = 1
  while (any(check_data[, 2] %in% check_data[, 1])) {

    #Add a new column to the output
    #Instead of RING > RING2, we now have RING > RING2 > RING3
    output_data <- output_data %>%
      dplyr::left_join(check_data, by = "RING2") %>%
      #Here we are removing the now redundant rows (where RING2 is now RING3 somewhere else)
      #!!as.symbol is just a way of referring to a column in dplyr using some code that returns a string rather than just a column name
      #e.g. !!as.symbol(names(.)[ncol(.) - 1]) refers to the column with the second last name
      dplyr::filter(!(!is.na(!!as.symbol(names(.)[ncol(.) - 1])) & !!as.symbol(names(.)[ncol(.) - 1]) %in% !!as.symbol(names(.)[ncol(.)]) & is.na(!!as.symbol(names(.)[ncol(.)]))))

    #Update the check_data to only consider those where a new column was added (i.e. they have a RING3 value)
    check_data <- output_data %>%
      dplyr::filter(!is.na(!!as.symbol(names(.)[ncol(.)]))) %>%
      #Only use the last 2 columns
      #We're only interested in whether there are any remaining cases where a left join could be appropriate
      dplyr::select((ncol(.)-1):ncol(.))

    #Change the name of the columns to use in the next loop (i.e. next time we want to add RING4)
    colnames(check_data) <- paste0("RING", c(N, N + 1))

    #Update N so we can keep adding new columns
    N <- N + 1

  }

  #The while loop above will make it so that each individual has a single row with N columns
  #which is the number of rings they've been given over their life.
  #We can now convert this into long format where the IndvID is just the rownumber.
  final_rings <- output_data %>%
    #Create a unique IndvID from the row number that is independent of the ring
    tibble::rowid_to_column(var = "Identifier") %>%
    #Pivot so that each ring number has its own row
    tidyr::pivot_longer(cols = starts_with("RING"), values_to = "RingNr") %>%
    dplyr::filter(!is.na(.data$RingNr)) %>%
    dplyr::select(.data$RingNr, .data$Identifier)

  # Return final re-ringing table
  return(final_rings)
}
