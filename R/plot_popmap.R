#' Create a map of all known populations
#'
#' Plot all known populations. Point colour represents whether data has been
#' provided.
#'
#' @param scale Scale of the saved jpeg image.
#' @param filename Name of saved file. Also allows for the filetype to be
#'   changed. By default, uses "Population_map.jpeg".
#'
#' @return A jpeg file in the working directory
#' @export
#'
#' @examples
#' #Create jpeg map
#' plot_popmap()
#' file.remove("Population_map.jpg")
plot_popmap <- function(scale = 2, filename = NULL){

  pop_locations <- utils::read.csv(system.file("extdata", "pop_locations.csv", package = "SPIbirds", mustWork = TRUE))

  world_map <- map_data("world")

  ggplot()+
    geom_polygon(data = GT_dist_gg, aes(x = long, y = lat, group = group), fill = "light grey") +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
    coord_cartesian(xlim = c(-20, 145), ylim = c(-10, 70)) +
    geom_point(data = pop_locations, aes(x = longitude, y = latitude, fill = data), shape = 21, size = 4) +
    #Add a second time to make sure that green points are always on top.
    #These are the ones we want people to see.
    geom_point(data = dplyr::filter(pop_locations, data == "Yes"), aes(x = longitude, y = latitude), fill = "green",
               shape = 21, size = 4) +
    scale_fill_manual(breaks = c("Yes", "No"),
                      values = c("#CCFFCC", "green"),
                      labels = c("Population meta-data provided", "Meta-data not yet provided")) +
    theme_classic() +
    guides(fill = guide_legend(override.aes = list(size = 5, stroke = 1))) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.275, 0.35),
          legend.background = element_rect(colour = "black", fill = "white", size = 1),
          legend.title = element_blank(),
          legend.text = element_text(size = 14))

  ggplot2::ggsave(filename = ifelse(is.null(filename), "Population_map.jpg", filename),
                  height = (3.58 * scale),
                  width = (5.18 * scale))

}

