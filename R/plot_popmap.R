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
#' if (requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("maps", quietly = TRUE)) {
#'     plot_popmap()
#'     file.remove("Population_map.jpg")
#' }
plot_popmap <- function(scale = 2, filename = NULL){

  if (!requireNamespace(package = "ggplot2", quietly = TRUE) | !requireNamespace(package = "maps", quietly = TRUE)) {

    stop("Require ggplot2 and maps package for plotting.")

  }

  pop_locations <- utils::read.csv(system.file("extdata", "pop_locations.csv", package = "pipelines", mustWork = TRUE))

  world_map <- ggplot2::map_data("world")

  ggplot2::ggplot()+
    ggplot2::geom_polygon(data = GT_dist_gg, ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group), fill = "light grey") +
    ggplot2::geom_polygon(data = world_map,
                          ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group), color = "black", fill = NA) +
    ggplot2::coord_cartesian(xlim = c(-20, 145), ylim = c(10, 70)) +
    ggplot2::geom_point(data = pop_locations,
                        ggplot2::aes(x = .data$longitude, y = .data$latitude, fill = .data$data), shape = 21, size = 4) +
    #Add a second time to make sure that green points are always on top.
    #These are the ones we want people to see.
    ggplot2::geom_point(data = pop_locations %>%
                          dplyr::filter(.data$data == "Yes"),
                        ggplot2::aes(x = .data$longitude, y = .data$latitude), fill = "green",
               shape = 21, size = 4) +
    ggplot2::scale_fill_manual(breaks = c("Yes", "No"),
                      values = c("#CCFFCC", "green"),
                      labels = c("Population meta-data provided", "Meta-data not yet provided")) +
    ggplot2::theme_classic() +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 5, stroke = 1))) +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = c(0.265, 0.25),
          legend.background = ggplot2::element_rect(colour = "black", fill = "white", size = 1),
          legend.title = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 14))

  ggplot2::ggsave(filename = ifelse(is.null(filename), "Population_map.jpg", filename),
                  height = (3.58 * scale),
                  width = (8.18 * scale))

}

