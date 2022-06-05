################################################################################
#                 Map rivers with sf and ggplot2 in R
#                 Milos Popovic
#                 2022/03/27
################################################################################

windowsFonts(georg = windowsFont("Georgia"))

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
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"
  res <- GET(
    url,
    write_disk("eu_rivers.zip"),
    progress()
  )
  unzip("eu_rivers.zip") # unzip
  filenames <- list.files("HydroRIVERS_v10_eu_shp",
    pattern = "*.shp", full.names = T
  )

  return(filenames)
}

# 2. CREATE RIVER WIDTH
#---------

get_rivers <- function(filenames, list_riv, eu_riv) {
  filenames <- get_data()
  list_riv <- lapply(filenames, sf::st_read)
  eu_riv <- list_riv[[1]] %>%
    st_cast("MULTILINESTRING") %>%
    mutate(
      width = as.numeric(ORD_FLOW),
      width = case_when(
        width == 3 ~ 1,
        width == 4 ~ 0.8,
        width == 5 ~ 0.6,
        width == 6 ~ 0.4,
        width == 7 ~ 0.2,
        width == 8 ~ 0.2,
        width == 9 ~ 0.1,
        width == 10 ~ 0.1,
        TRUE ~ 0
      )
    ) %>%
    st_as_sf()

  eu_riv$geometry <- eu_riv$geometry %>%
    s2::s2_rebuild() %>%
    sf::st_as_sfc()

  return(eu_riv)
}

# 3. MAKE BOUNDING BOX
#---------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_bounding_box <- function(bbox, new_prj, bb) {
  bbox <- st_sfc(
    st_polygon(list(cbind(
      c(-10.5, 48.5, 48.5, -10.5, -10.5),
      c(35.000, 35.000, 69.5, 69.5, 35.000)
    ))),
    crs = crsLONGLAT
  )

  new_prj <- sf::st_transform(bbox, crs = 4087)
  bb <- sf::st_bbox(new_prj)

  return(bb)
}

# 4. MAP
#---------

map_url <- "https://raw.githubusercontent.com/milos-agathon/map-rivers-with-sf-and-ggplot2-in-R/main/R/make_map.r"
source(map_url) # load script
p1 <- get_river_map()
ggsave(filename="european_rivers_new.png", width=7, height=8.5, dpi = 600, device='png', p1)
