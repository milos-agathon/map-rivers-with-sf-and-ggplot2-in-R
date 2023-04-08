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

# 1. GET RIVERS DATA
#---------

get_data <- function() {
    url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_af_shp.zip"
    res <- httr::GET(
        url,
        write_disk("africa_rivers.zip"),
        progress()
    )
    unzip("africa_rivers.zip") # unzip
    filenames <- list.files("HydroRIVERS_v10_af_shp",
        pattern = "*.shp", full.names = T
    )

    return(filenames)
}

filenames <- get_data()

# 2. CREATE RIVER WIDTH
#---------

load_rivers <- function() {
    list_riv <- lapply(filenames, sf::st_read)
    africa_riv <- list_riv[[1]] |>
        sf::st_cast("MULTILINESTRING")

    return(africa_riv)
}

africa_riv <- load_rivers()
summary(africa_riv$ORD_FLOW)

get_river_width <- function() {
    africa_riv_width <- africa_riv |>
        dplyr::mutate(
            width = as.numeric(ORD_FLOW),
            width = dplyr::case_when(
                width == 2 ~ .8,
                width == 3 ~ .6,
                width == 4 ~ .45,
                width == 5 ~ .35,
                width == 6 ~ .25,
                width == 7 ~ .2,
                width == 8 ~ .15,
                width == 9 ~ .1,
                width == 10 ~ .1,
                TRUE ~ 0
            )
        ) |>
        sf::st_as_sf()

    return(africa_riv_width)
}

africa_riv_width <- get_river_width()

cols <- c(
    "#440154", "#472a62", "#474770",
    "#42627b", "#3a7d83", "#2c9986",
    "#48b279", "#83c760", "#bdd946"
)

p <-
    ggplot() +
    geom_sf(
        data = africa_riv_width,
        aes(
            color = factor(ORD_FLOW), size = width,
            alpha = factor(ORD_FLOW)
        )
    ) +
    scale_alpha_manual(values = c(
        "2" = 1, "3" = .8, "4" = .8, "5" = .6, "6" = .6,
        "7" = .4, "8" = .4, "9" = .2, "10" = .2
    )) +
    scale_color_manual(
        name = "",
        values = cols,
        drop = F
    ) +
    scale_size(range = c(0, .3)) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(
            size = 50, color = "grey20", hjust = 0.5, vjust = 15
        ),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold", size = 150, color = "#3a7d83", hjust = -.1,
            vjust = -70
        ),
        plot.caption = element_text(
            size = 50, color = "grey20", hjust = .5, vjust = 2
        ),
        plot.margin = unit(
            c(t = 0, r = 0, b = 0, l = 0), "lines"
        ),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank()
    ) +
    labs(
        x = "©2023 Milos Popovic (https://milospopovic.net) | Data: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org",
        y = NULL,
        title = "Rivers of Africa",
        subtitle = "",
        caption = ""
    )

ggsave(
    filename = "africa-rivers.png",
    width = 8.5, height = 7, dpi = 600,
    bg = "white", device = "png", p
)
