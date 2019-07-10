#' Generate histograms of continuous data for debugging
#'
#' Generates histograms for any provided continuous variable. When editing
#' pipelines, these allow the user to check that outputs are as expected. This
#' functionality will eventually be integrated into the data quality check
#' pipeline.
#' @param table A table containing data of interest.
#' @param variable A character string of the variable name.
#'
#' @return A histogram generated with ggplot.
#' @export
#' @import ggplot2

plot_debug_hist <- function(table, variable){

  variable <- sym(variable)
  variable <- enquo(variable)
  var_name <- snakecase::to_any_case(dplyr::quo_name(variable), case = "sentence")

  #When variable is NA return NULL
  if(table %>% pull(!!variable) %>% {all(is.na(.))}){

    return(NULL)

  }

  #Make a number of bins the same as the number of unique values
  binwidth <- ifelse(grepl(pattern = "Tarsus|Mass|Length", x = quo_name(variable)), 0.5, 1)

  #Generate histogram of clutch size
  raw_dat <- table %>%
    filter(!is.na(!!variable) & !is.na(Species)) %>%
    {ggplot(.)+
        geom_histogram(aes(x = !!variable), binwidth = binwidth, fill = "grey", colour = "black")+
        labs(title = paste0("Histogram of ", tolower(var_name), " from \n", pop_names[which(pop_names$code == table$PopID[1]), ]$name),
             y = "Number of observations", x = var_name)+
        theme_classic()+
        theme(plot.title = element_text(hjust = 0.5, size = 20),
              axis.text = element_text(size = 12, colour = "black"),
              axis.title = element_text(size = 15, colour = "black"),
              plot.margin = margin(20, 20, 20, 20))}

  #Facet histograms if there are many species
  if(length(unique(table$Species)) > 1){

    raw_dat <- raw_dat +
      facet_wrap(facets = ~Species, scales = "free")

  }

  return(raw_dat)

}
