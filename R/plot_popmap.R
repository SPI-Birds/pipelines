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
plot_popmap <- function(scale = 2, filename = NULL){

  pop_locations <- read.csv(system.file("extdata", "pop_locations.csv", package = "HNBStandFormat", mustWork = TRUE))

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

# plot_indv_popmap <- function(scale = 2, pop_name, filename){
#
#   world_map <- map_data("world")
#
#   ggplot()+
#     geom_polygon(data = GT_dist_gg, aes(x = long, y = lat, group = group), fill = "light grey") +
#     geom_polygon(data = world_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
#     coord_fixed(xlim = c(-17, 145), ylim = c(-5, 69.75)) +
#     geom_point(data = dplyr::filter(pop_locations, data == "No"), aes(x = longitude, y = latitude), fill = "#CCFFCC",
#                shape = 21, size = 2)+
#     geom_point(data = dplyr::filter(pop_locations, data == "Yes" & site_name != pop_name), aes(x = longitude, y = latitude), fill = "green",
#                shape = 21, size = 2)+
#     geom_point(data = dplyr::filter(pop_locations, site_name == pop_name), aes(x = longitude, y = latitude), fill = "green",
#                shape = 21, size = 4, stroke = 1.5)+
#     geom_curve(data = dplyr::filter(pop_locations, site_name == pop_name),
#                aes(x = longitude - 10, y = latitude + 10,
#                    xend = longitude - 1, yend = latitude),
#                arrow = arrow(length = unit(7, "pt")), size = 1)+
#     geom_label(data = dplyr::filter(pop_locations, site_name == pop_name),
#               aes(x = longitude - 10, y = latitude + 7, label = pop_name),
#               size = 7)+
#     theme_classic() +
#     theme(axis.line = element_blank(),
#           axis.text = element_blank(),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           legend.position = "none")
#
#   ggplot2::ggsave(filename = ifelse(is.null(filename), "Population_map.jpeg", filename),
#                   height = (3.58 * scale),
#                   width = (5.18 * scale))
#
# }
#
# plot_indv_mapWsp <- function(scale = 2, pop_name, filename, species){
#
#   #Single Pop
#   singlepop <- dplyr::filter(pop_locations, site_name == pop_name)
#
#   #Get all species associated with this population
#   list_imgs <- list.files("./inst/extdata", pattern = ".png") %>%
#     {`[`(., grepl(pattern = paste(species, collapse = "|"), x = .))}
#
#   pngs <- purrr::map(.x = list_imgs,
#                      .f = ~{
#
#                        grid::rasterGrob(png::readPNG(source = system.file("extdata", .x, package = "HNBStandFormat", mustWork = TRUE)),
#                                         width = unit(4, "cm"), height = unit(4, "cm"))
#
#                      })
#
#   #Because we only want to include music grob in one facet, we create a data set so that it has corresponding aesthetic data
#   bird_locations <- tibble(longitude = as.numeric(paste(singlepop$longitude + c(4, 12))),
#                            latitude = as.numeric(paste(singlepop$latitude + c(5, 5)))) %>%
#     dplyr::mutate(grob = pngs)
#
#   world_map <- map_data("world") %>%
#     dplyr::filter(region == singlepop$country)
#
#   ggplot()+
#     geom_polygon(data = GT_dist_gg, aes(x = long, y = lat, group = group), fill = "light grey") +
#     geom_polygon(data = world_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
#     coord_map(xlim = c(singlepop$longitude - 2, singlepop$longitude + 20),
#                 ylim = c(singlepop$latitude - 2, singlepop$latitude + 8)) +
#     geom_point(data = dplyr::filter(pop_locations, site_name == pop_name), aes(x = longitude, y = latitude), fill = "green",
#                shape = 21, size = 4, stroke = 1.5)+
#     egg::geom_custom(data = bird_locations,
#                      aes(x = longitude, y = latitude, data = grob), grob_fun = "identity") +
#     geom_label(aes(x = bird_locations$longitude, y = bird_locations$latitude - 1.5,
#                    label = c("Blue Tit", "Great Tit")))+
#     theme_classic() +
#     theme(axis.line = element_blank(),
#           axis.text = element_blank(),
#           axis.title = element_blank(),
#           axis.ticks = element_blank(),
#           legend.position = "none")
#
#   ggplot2::ggsave(filename = ifelse(is.null(filename), "Population_map.jpeg", filename),
#                   height = (3.58 * scale),
#                   width = (5.18 * scale))
#
# }
#
#
# #Make data into count per species per year
# plot_dat <- Brood_data_output %>%
#   group_by(SampleYear, Species) %>%
#   count() %>%
#   filter(SampleYear >= 1991)
#
# ggplot(plot_dat) +
#   geom_path(aes(x = SampleYear, y = n, colour = Species), size = 1)+
#   geom_point(aes(x = SampleYear, y = n, fill = Species), shape = 21, stroke = 1, size = 3) +
#   theme_classic() +
#   scale_x_continuous(breaks = seq(1990, 2020, 5), limits = c(1990, 2020))+
#   labs(x = "Year", y = "Number of broods") +
#   theme(legend.position = "bottom",
#         axis.text = element_text(colour = "black", size = 12),
#         axis.title = element_text(colour = "black", size = 14)) +
#   #Start gganimate code
#   gganimate::transition_reveal(SampleYear)+
#   gganimate::shadow_mark(alpha = 0.25, wrap = FALSE, size = 2, exclude_layer = 2:3)+
#   gganimate::ease_aes("linear")

