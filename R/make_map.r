get_river_map <- function(eu_riv, bbox, p) {

  eu_riv <- get_rivers()
  bbox <- get_bounding_box()

  p <- 
    ggplot() +
    geom_sf(data=eu_riv, aes(color=factor(ORD_FLOW), size=width)) +
    coord_sf( 
      xlim = c(bbox["xmin"], bbox["xmax"]), 
      ylim = c(bbox["ymin"], bbox["ymax"])) +
    labs(y="", subtitle="",
         x = "",
         title="Rivers of Europe",
         caption="©2022 Milos Popovic https://milospopovic.net\nSource: ©World Wildlife Fund, Inc. (2006-2013)\n HydroSHEDS database http://www.hydrosheds.org") +
    scale_color_manual(
        name = "",
        values = c('#08306b', '#08519c', '#2171b5', '#4292c6', '#6baed6', '#9ecae1', '#c6dbef', '#deebf7')) +
    scale_size(range=c(0, .3)) +
    scale_alpha_manual(values=c("3" = 1, "4" = 1, "5" = .7, "6" = .6, "7" = .4, "8" = .3, "9" = .2, "10" = .1)) +
    theme_minimal() +
    theme(text = element_text(family = "georg"),
      panel.background = element_blank(), 
      legend.background = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(size=40, color="#2171b5", hjust=0.5, vjust=0),
      plot.subtitle = element_text(size=14, color="#ac63a0", hjust=0.5, vjust=0),
      plot.caption = element_text(size=10, color="grey60", hjust=0.5, vjust=10),
      axis.title.x = element_text(size=10, color="grey20", hjust=0.5, vjust=-6),
      legend.text = element_text(size=9, color="grey20"),
      legend.title = element_text(size=10, color="grey20"),
      strip.text = element_text(size=12),
      plot.margin = unit(c(t=1, r=-2, b=-1, l=-2),"lines"), #added these narrower margins to enlarge maps
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank())

  return(p)
}
