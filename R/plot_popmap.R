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
#' @import maps
#'
#' @examples
#' #Create jpeg map
#' plot_popmap()
plot_popmap <- function(scale = 2, filename){

  world_map <- map_data("world")

  ggplot()+
    geom_polygon(data = GT_dist_gg, aes(x = long, y = lat, group = group), fill = "light grey") +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
    coord_fixed(xlim = c(-17, 145), ylim = c(-5, 69.75)) +
    geom_point(data = dplyr::filter(pop_locations, data == "No"), aes(x = longitude, y = latitude), fill = "#CCFFCC",
               shape = 21, size = 4)+
    geom_point(data = dplyr::filter(pop_locations, data == "Yes"), aes(x = longitude, y = latitude), fill = "green",
               shape = 21, size = 4)+
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")

  ggplot2::ggsave(filename = ifelse(is.null(filename), "Population_map.jpeg", filename),
                  height = (3.58 * scale),
                  width = (5.18 * scale))

}
