################################################################################
#                 Map rivers with sf and ggplot2 in R
#                 Milos Popovic
#                 2022/03/27
################################################################################

windowsFonts(georg = windowsFont('Georgia'))

# libraries we need
libs <- c("httr", "tidyverse", "sf")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET RIVERS DATA
#---------

get_data <- function(url, res, filenames) {

  url <- "https://m210.syncusercontent1.com/mfs-60:cb0f370cfc9aad110e47af0c14c6d6a0=============================/p/HydroRIVERS_v10_eu_shp.zip?allowdd=0&datakey=MEK8JcKMJoznBg4K13Vm2mCHctdtCNAEQ3ZFYKbRGEyA9uAIMf0R+BuUD0rop5VAjOns+y1t8ozNN5amjPzsOyT1g3nUTGvjPAlBbPNZkbTSBBUyR5+MLeJtYFcZl2Y4RWV0KTd6yOL0/I9wra00C0HJXi92Py0YRdIwxelyVBEoeGCMFb+v1Adrdt6v3ag8JaF44CI9Kv+GvvUfMufW/Hgkp06uA4agdhFpWT72X2p5ikcg60g2VItRGSNlR0NAa7okEF3VJVip5KxXH1aWp0d4yObz1n7d6RzLsrgqf5CyQ2KCRzVu3oaXowzOFo8OtjZFQM5fbWvct0Y4xrG5+Q&engine=ln-1.11.18&errurl=Sjhv0TxOrDthwEU0ko9wmfrvcN2UpD5PHdpVMOimj9EWZRPtmLTp/mq8O44yPH9vLj5QVjm9eGnV5ODhjEttwu9XvoB+86a9Ndw3vTHF2FXzr0XErSV5ULM0b8TeYHrZlaBWTcCswwBwF5qpQBih502dXlu23yTdQyMLvxFOs9CpGGnIA1t8rWe0g9oOjs++LypSkx3Jq/9T/jA85aAjFCGiJxzvtTIFE7UATfePptyXmziIdHpX9PJplhSNr19K9B9MejBMGZayTC9yg9ynVV+12rIU4u0cg2AsppPWXtdaFmN+gThzrHlBSwergBaWxrxrqy++wMPYUkske1dRjQ==&header1=Q29udGVudC1UeXBlOiBhcHBsaWNhdGlvbi96aXA&header2=Q29udGVudC1EaXNwb3NpdGlvbjogYXR0YWNobWVudDsgZmlsZW5hbWU9Ikh5ZHJvUklWRVJTX3YxMF9ldV9zaHAuemlwIjtmaWxlbmFtZSo9VVRGLTgnJ0h5ZHJvUklWRVJTX3YxMF9ldV9zaHAuemlwOw&ipaddress=1398165091&linkcachekey=89159c590&linkoid=51000013&mode=101&sharelink_id=3443504720013&timestamp=1648371415033&uagent=5186dac1dd506c3487aa39a6c8a4a57637399ef6&signature=d7312dd79de60503dbd01d82ccda0b0f05411575&cachekey=60:cb0f370cfc9aad110e47af0c14c6d6a0============================="
  res <- GET(url,
             write_disk("eu_rivers.zip"),
             progress())
  unzip("eu_rivers.zip") #unzip
  filenames <- list.files("HydroRIVERS_v10_eu_shp", pattern="*.shp", full.names=T)

  return(filenames)
}

# 2. CREATE RIVER WIDTH
#---------

get_rivers <- function(filenames, list_riv, eu_riv) {

  filenames <- get_data()
  list_riv <- lapply(filenames, st_read)
  eu_riv <- list_riv[[1]] %>% 
  st_cast("MULTILINESTRING") %>% 
  mutate(width = as.numeric(ORD_FLOW),
         width = case_when(width == 3 ~ 1,
                           width == 4 ~ 0.8,
                           width == 5 ~ 0.6,
                           width == 6 ~ 0.4,
                           width == 7 ~ 0.2,
                           width == 8 ~ 0.2,
                           width == 9 ~ 0.1,
                           width == 10 ~ 0.1,
                           TRUE ~ 0)) %>% 
  st_as_sf()

  return(eu_riv)
}

# 3. MAKE BOUNDING BOX
#---------

get_bounding_box <- function(crsLONGLAT, bbox, new_prj, bb) {

  crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

  bbox <- st_sfc(
  st_polygon(list(cbind(
    c(-10.5, 48.5, 48.5, -10.5, -10.5), # x-coordinates (longitudes) of points A,B,C,D
    c(35.000, 35.000, 69.5, 69.5, 35.000)     # y-coordinates (latitudes) of points A,B,C,D
    ))),
  crs = crsLONGLAT)

  new_prj <- st_transform(bbox, crs = 4087)
  bb <- st_bbox(new_prj)

  return(bb)
}

# 4. MAP
#---------

map_url <- "https://raw.githubusercontent.com/milos-agathon/map-rivers-with-sf-and-ggplot2-in-R/main/R/make_map.r"
source(map_url) # load script
p1 <- get_river_map()

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


p1 <- get_river_map()
ggsave(filename="european_rivers_new.png", width=7, height=8.5, dpi = 600, device='png', p1)
