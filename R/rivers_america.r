# libraries we need
libs <- c(
    "httr", "tidyverse", "sf"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

sysfonts::font_add_google("Montserrat", "Montserrat")
showtext::showtext_auto()

# 1. GET RIVERS DATA
#---------

na_riv <- sf::st_read("HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp") %>%
    st_cast("MULTILINESTRING")

names(na_riv)


ar_riv <- sf::st_read("HydroRIVERS_v10_ar_shp/HydroRIVERS_v10_ar_shp/HydroRIVERS_v10_ar.shp") %>%
    st_cast("MULTILINESTRING")

summary(ar_riv$ORD_FLOW)

nariv <- na_riv %>%
    mutate(
        width = as.numeric(ORD_FLOW),
        width = case_when(
            width == 2 ~ 1,
            width == 3 ~ 0.8,
            width == 4 ~ 0.6,
            width == 5 ~ 0.45,
            width == 6 ~ 0.35,
            width == 7 ~ 0.25,
            width == 8 ~ 0.15,
            width == 9 ~ 0.1,
            width == 10 ~ 0.1,
            TRUE ~ 0
        )
    ) %>%
    st_as_sf()

nariv$geometry <- nariv$geometry %>%
    s2::s2_rebuild() %>%
    sf::st_as_sfc()

ariv <- ar_riv %>%
    mutate(
        width = as.numeric(ORD_FLOW),
        width = case_when(
            width == 3 ~ 0.8,
            width == 4 ~ 0.6,
            width == 5 ~ 0.45,
            width == 6 ~ 0.35,
            width == 7 ~ 0.25,
            width == 8 ~ 0.15,
            width == 9 ~ 0.1,
            width == 10 ~ 0.1,
            TRUE ~ 0
        )
    ) %>%
    st_as_sf()

ariv$geometry <- ariv$geometry %>%
    s2::s2_rebuild() %>%
    sf::st_as_sfc()


crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

bbox <- st_sfc(
    st_polygon(list(cbind(
        c(-178.2, 6.6, 6.6, -178.2, -178.2),
        c(-49.000, -49.000, 83.3, 83.3, -49.000)
    ))),
    crs = crsLONGLAT
)

prj <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

new_prj <- sf::st_transform(bbox, crs = prj)
bb <- sf::st_bbox(new_prj)

p <-
    ggplot() +
    geom_sf(data = nariv, aes(
        color = factor(ORD_FLOW), size = width, alpha = width
    )) +
    geom_sf(data = ariv, aes(
        color = factor(ORD_FLOW), size = width, alpha = width
    )) +
    coord_sf(
        crs = prj,
        xlim = c(bb["xmin"], bb["xmax"]),
        ylim = c(bb["ymin"], bb["ymax"])
    ) +
    labs(
        y = "", subtitle = "",
        x = "",
        title = "Rivers of North and Central America",
        caption = "©2022 Milos Popovic https://milospopovic.net Source: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org"
    ) +
    scale_color_manual(
        name = "",
        values = c(
            "#08306b", "#1c4680", "#305d94", "#4574a7",
            "#5d8cb9", "#77a4cb", "#deebf7", "#deebf7", "#deebf7"
        ),
        drop = F
    ) +
    scale_alpha(range = c(0, 1)) +
    scale_size(range = c(0, .45)) +
    theme_minimal() +
    theme(
        text = element_text(family = "Montserrat"),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(
            size = 100, color = "#2171b5", hjust = 0.5, vjust = -22
        ),
        plot.subtitle = element_text(
            size = 14, color = "#ac63a0", hjust = 0.5, vjust = 0
        ),
        plot.caption = element_text(
            size = 40, color = "grey60", hjust = 0.5, vjust = 120
        ),
        axis.title.x = element_text(
            size = 10, color = "grey20", hjust = 0.5, vjust = -6
        ),
        legend.text = element_text(size = 9, color = "grey20"),
        legend.title = element_text(size = 10, color = "grey20"),
        strip.text = element_text(size = 12),
        plot.margin = unit(c(t = -2.5, r = -10, b = -10, l = -10), "lines"),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
    )

ggsave(
    filename = "nca_rivers.png",
    width = 9, height = 6, dpi = 600,
    bg = "white", device = "png", p
)
