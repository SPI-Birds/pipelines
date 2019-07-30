#' Create plots to introduce the Hoge Veluwe population on twitter
#'
#' @return Generate two plots. One .jpeg showing the population on the global map and study species.
#' One .gif showing number of nests sampled for each species over time.
#' @export
VLI_twitter <- function(){

  if(!"Brood_data_NIOO.csv" %in% list.files()){

    #First run the pipeline to extract NIOO data
    format_NIOO()

  }

  #Then load the Brood_data table
  VLI_data <- utils::read.csv("Brood_data_NIOO.csv", stringsAsFactors = FALSE) %>%
    dplyr::filter(PopID == "VLI")

  #Load pop location info
  pop_locations <- utils::read.csv(system.file("extdata", "pop_locations.csv", package = "HNBStandFormat", mustWork = TRUE))

  #Load file of world boundaries
  world_map <- map_data("world")

  ##################
  # GENERATE PLOT1 #
  ##################

  #Get all species associated with this population
  list_imgs <- list.files("./inst/extdata", pattern = ".png") %>%
    {`[`(., grepl(pattern = paste(unique(VLI_data$Species), collapse = "|"), x = .))}

  #Add locations for bird pngs
  bird_locations <- bird_png_data %>%
    dplyr::right_join(purrr::map_df(.x = list_imgs,
                     .f = ~{

                       scale <- bird_png_data$scale[grepl(pattern = substr(.x, 1, 6),
                                                          bird_png_data$species)]

                      tibble::tibble(species = substr(.x, 1, 6),
                                     grob = list(grid::rasterGrob(png::readPNG(source = system.file("extdata", .x, package = "HNBStandFormat", mustWork = TRUE)),
                                                             width = unit(4.5*scale*1.25, "cm"), height = unit(3*scale*1.25, "cm"))))

                     }), by = "species") %>%
    dplyr::mutate(longitude = seq(from = 11, to = 20, length.out = 3),
                  latitude = rep(52.5, 3))

  (plot1 <- ggplot()+
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), color = "black", fill = "light grey") +
      coord_cartesian(xlim = c(2, 25), ylim = c(47, 57)) +
      geom_point(data = dplyr::filter(pop_locations, site_name != "Vlieland"), aes(x = longitude, y = latitude), fill = "white",
                 shape = 21, size = 4) +
      geom_segment(data = dplyr::filter(pop_locations, site_name == "Vlieland"),
                   aes(x = longitude, y = latitude,
                       xend = min(bird_locations$longitude) - 2.5,
                       yend = min(bird_locations$latitude) - 2),
                   size = 1) +
      geom_segment(data = dplyr::filter(pop_locations, site_name == "Vlieland"),
                   aes(x = longitude, y = latitude,
                       xend = min(bird_locations$longitude) - 2.5,
                       yend = max(bird_locations$latitude) + 2),
                   size = 1) +
      geom_point(data = dplyr::filter(pop_locations, site_name == "Vlieland"), aes(x = longitude, y = latitude),
                 fill = "green",
                 shape = 21, size = 7, stroke = 1.5) +
      geom_curve(data = dplyr::filter(pop_locations, site_name == "Hoge Veluwe"),
                 curvature = -0.5,
                 aes(x = longitude - 1.5, y = latitude - 2,
                     xend = longitude, yend = latitude),
                 arrow = arrow(length = unit(7, "pt")), size = 1)+
      geom_label(data = dplyr::filter(pop_locations, site_name == "Hoge Veluwe"),
                 aes(x = longitude - 1.5, y = latitude - 2, label = "Hoge Veluwe"),
                 size = 6, colour = "black", label.padding = unit(0.3, "cm"),
                 label.size = 1, family = "Ubuntu")+
      geom_label(data = bird_locations,
                 aes(x = mean(longitude), y = max(latitude) + 3, label = "Vlieland"),
                 size = 14, colour = "black", label.padding = unit(0.4, "cm"),
                 label.size = 1, family = "Ubuntu")+
      geom_rect(data = bird_locations, aes(xmin = min(bird_locations$longitude) - 2.5,
                                           xmax = max(bird_locations$longitude) + 2.5,
                                           ymin = min(latitude) - 2,
                                           ymax = max(latitude) + 2),
                fill = "white", alpha = 0.2, colour = "black") +
      egg::geom_custom(data = bird_locations,
                       aes(x = longitude, y = latitude, data = grob), grob_fun = "identity") +
      geom_label(data = bird_locations, aes(x = longitude, y = latitude - 1.3, label = label),
                 size = 6, family = "Ubuntu") +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank()))

  ggplot2::ggsave(plot = plot1,
                  filename = "VLI_twitter_plot1.jpg",
                  height = (3.58 * 2),
                  width = (5.18 * 2))

  timeline <- utils::read.csv("Brood_data_NIOO.csv", stringsAsFactors = FALSE) %>%
    dplyr::filter(PopID %in% c("VLI", "HOG") & Species %in% unique(VLI_data$Species)) %>%
    dplyr::mutate(PopID = forcats::fct_recode(PopID, `Hoge Veluwe` = "HOG", `Vlieland` = "VLI")) %>%
    dplyr::group_by(PopID, Species, BreedingSeason) %>%
    dplyr::count() %>%
    dplyr::filter(BreedingSeason < 2019)

  anim_plot <- ggplot(timeline) +
    geom_path(aes(x = BreedingSeason, y = n, colour = Species), size = 1)+
    geom_point(aes(x = BreedingSeason, y = n, fill = Species), shape = 21, stroke = 1, size = 4) +
    theme_classic() +
    scale_fill_manual(values = bird_locations$base_colour,
                      labels = bird_locations$label) +
    scale_colour_manual(values = bird_locations$base_colour) +
    scale_x_continuous(breaks = seq(1950, 2020, 10), limits = c(1948, 2022))+
    scale_y_continuous(breaks = seq(0, 300, 50)) +
    labs(x = "Year", y = "Number of broods") +
    guides(fill = guide_legend(override.aes = list(size = 5, stroke = 1)),
           color = "none") +
    theme(legend.position = "bottom",
          axis.text = element_text(colour = "black", size = 13, family = "Ubuntu"),
          axis.title = element_text(colour = "black", size = 16, family = "Ubuntu"),
          legend.title = element_text(size = 16, family = "Ubuntu"),
          legend.text = element_text(size = 14, family = "Ubuntu"),
          strip.text = element_text(size = 14, family = "Ubuntu")) +
    facet_wrap(~PopID) +
    #Start gganimate code
    gganimate::transition_reveal(BreedingSeason)+
    gganimate::shadow_mark(alpha = 0.25, wrap = FALSE, size = 2, exclude_layer = 2:3)+
    gganimate::ease_aes("linear")

  options(gganimate.dev_args = list(width = 600, height = 520))

  gganimate::anim_save("VLI_twitter.gif",
                       animation = gganimate::animate(anim_plot, duration = 10, end_pause = 10))

  #Satisfy RCMD Checks
  PopID <- `.` <- GT_dist_gg <- long <- lat <- group <- site_name <- longitude <- NULL
  latitude <- grob <- label <- Species <- BreedingSeason <- NULL
  bird_png_data <- NULL

}
