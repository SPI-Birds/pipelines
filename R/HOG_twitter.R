#' Create plots to introduce the Hoge Veluwe population on twitter
#'
#' @return Generate two plots. One .jpeg showing the population on the global map and study species.
#' One .gif showing number of nests sampled for each species over time.
#' @export
#' @import extrafont
#'
#' @examples
#' HOG_twitter()
HOG_twitter <- function(){

  if(!"Brood_data_NIOO.csv" %in% list.files()){

    #First run the pipeline to extract NIOO data
    format_NIOO()

  }

  #Then load the Brood_data table
  HOG_data <- read.csv("Brood_data_NIOO.csv", stringsAsFactors = FALSE) %>%
    dplyr::filter(PopID == "HOG")

  #Load pop location info
  pop_locations <- read.csv(system.file("extdata", "pop_locations.csv", package = "HNBStandFormat", mustWork = TRUE))

  #Load file of world boundaries
  world_map <- map_data("world")

  ##################
  # GENERATE PLOT1 #
  ##################

  #Get all species associated with this population
  list_imgs <- list.files("./inst/extdata", pattern = ".png") %>%
    {`[`(., grepl(pattern = paste(unique(HOG_data$Species), collapse = "|"), x = .))}

  #Create a file that varies the size of the images
  bird_locations <- tibble::tibble(species = c("CYACAE", "FICHYP", "PARMAJ",
                                              "PASMON", "PERATE", "SITEUR"),
                                label = c("Blue tit", "Pied flycatcher", "Great tit",
                                          "Tree sparrow", "Coal tit", "Nuthatch"),
                                  scale = c(12/15,
                                           13.5/15,
                                           15/15,
                                           14/15,
                                           11.5/15,
                                           14.5/15),
                                base_colour = c("#164dc3", "black", "#ecd252",
                                                "#623f23", "#7a7270", "#af6924"),
                                top_colour = c("white", "white", "black",
                                               "#bab2a1", "white", "#7e808c"),
                                longitude = c(50, 80, 110, 50, 80, 110),
                                latitude = c(50, 50, 50, 30, 30, 30))

  pngs <- purrr::map(.x = list_imgs,
                     .f = ~{

                       scale <- bird_locations$scale[grepl(pattern = substr(.x, 1, 6),
                                                           bird_locations$species)]

                       grid::rasterGrob(png::readPNG(source = system.file("extdata", .x, package = "HNBStandFormat", mustWork = TRUE)),
                                        width = unit(4.5*scale, "cm"), height = unit(3*scale, "cm"))

                     })

  #Because we only want to include music grob in one facet, we create a data set so that it has corresponding aesthetic data
  bird_locations <- bird_locations %>%
    dplyr::mutate(grob = pngs)

  (plot1 <- ggplot()+
    geom_polygon(data = GT_dist_gg, aes(x = long, y = lat, group = group), fill = "light grey") +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
    coord_cartesian(xlim = c(-20, 145), ylim = c(-10, 70)) +
    geom_point(data = dplyr::filter(pop_locations, site_name != "Hoge Veluwe"), aes(x = longitude, y = latitude), fill = "white",
               shape = 21, size = 4) +
    geom_segment(data = dplyr::filter(pop_locations, site_name == "Hoge Veluwe"),
                 aes(x = longitude, y = latitude,
                     xend = min(bird_locations$longitude) - 15, yend = min(bird_locations$latitude) - 15),
                 size = 1) +
    geom_segment(data = dplyr::filter(pop_locations, site_name == "Hoge Veluwe"),
                 aes(x = longitude, y = latitude,
                     xend = min(bird_locations$longitude) - 15, yend = max(bird_locations$latitude) + 10),
                 size = 1) +
    geom_point(data = dplyr::filter(pop_locations, site_name == "Hoge Veluwe"), aes(x = longitude, y = latitude), fill = "green",
               shape = 21, size = 7, stroke = 1.5) +
    # geom_curve(data = dplyr::filter(pop_locations, site_name == "Hoge Veluwe"),
    #            curvature = -0.5,
    #            aes(x = longitude + 10, y = latitude + 7,
    #                xend = longitude, yend = latitude),
    #            arrow = arrow(length = unit(7, "pt")), size = 1)+
    geom_label(data = bird_locations,
               aes(x = mean(longitude), y = max(latitude) + 17, label = "Hoge Veluwe"),
               size = 14, colour = "black", label.padding = unit(0.4, "cm"),
               label.size = 1, family = "Ubuntu")+
    geom_rect(data = bird_locations, aes(xmin = min(longitude) - 15,
                                         xmax = max(longitude) + 17,
                                         ymin = min(latitude) - 15,
                                         ymax = max(latitude) + 10),
              fill = "white", alpha = 0.2, colour = "black") +
    egg::geom_custom(data = bird_locations,
                     aes(x = longitude, y = latitude, data = grob), grob_fun = "identity") +
    geom_label(data = bird_locations, aes(x = longitude, y = latitude - 10, label = label),
               size = 6, family = "Ubuntu") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()))

  ggplot2::ggsave(plot = plot1,
                  filename = "HOG_twitter_plot1.jpg",
                  height = (3.58 * 2),
                  width = (5.18 * 2))

  timeline <- HOG_data %>%
    dplyr::group_by(Species, SampleYear) %>%
    dplyr::count() %>%
    dplyr::filter(SampleYear < 2019)

  anim_plot <- ggplot(timeline) +
    geom_path(aes(x = SampleYear, y = n, colour = Species), size = 1)+
    geom_point(aes(x = SampleYear, y = n, fill = Species), shape = 21, stroke = 1, size = 4) +
    theme_classic() +
    scale_fill_manual(values = bird_locations$base_colour,
                      labels = bird_locations$label) +
    scale_colour_manual(values = bird_locations$base_colour) +
    scale_x_continuous(breaks = seq(1960, 2020, 10), limits = c(1960, 2022))+
    scale_y_continuous(breaks = seq(0, 300, 50)) +
    labs(x = "Year", y = "Number of broods") +
    guides(fill = guide_legend(override.aes = list(size = 5, stroke = 1)),
           color = "none") +
    theme(legend.position = "bottom",
          axis.text = element_text(colour = "black", size = 14, family = "Ubuntu"),
          axis.title = element_text(colour = "black", size = 16, family = "Ubuntu"),
          legend.title = element_text(size = 16, family = "Ubuntu"),
          legend.text = element_text(size = 14, family = "Ubuntu")) +
    #Start gganimate code
    gganimate::transition_reveal(SampleYear)+
    gganimate::shadow_mark(alpha = 0.25, wrap = FALSE, size = 2, exclude_layer = 2:3)+
    gganimate::ease_aes("linear")

  options(gganimate.dev_args = list(width = 600, height = 520))

  gganimate::anim_save("HOG_twitter.gif",
                       animation = gganimate::animate(anim_plot, duration = 10, end_pause = 10))

}
