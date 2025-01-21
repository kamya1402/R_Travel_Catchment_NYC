#Install packages
install.packages("remotes")
remotes::install_github(
  "GIScience/openrouteservice-r"
)
install.packages("tidyverse")
install.packages("openrouteservice")
install.packages("sf")
install.packages("leaflet")
install.packages("maptiles")
install.packages("tidyterra")

library(tidyverse)
library(openrouteservice)
library(sf)
library(leaflet)
library(maptiles)
library(tidyterra)

#defining main parameters

openrouteservice::ors_profile()
lat <- 40.7527
lon <- -73.9772
api_key <- "5b3ce3597851110001cf62485a3be0b514ce422da20522c359c787e5"

#query

coords <- data.frame(lon, lat)
cycling_nyc <- openrouteservice::ors_isochrones(
  locations = coords,
  profile = "cycling-regular",
  range = 3600,
  interval = 600,
  api_key = api_key,
  output = "sf"
)

#data transformation

sf::sf_use_s2(F)
cycling_nyc$mins <- cycling_nyc$value/60
cycling_nyc$mins <- factor(
  cycling_nyc$mins
)

cycling_nyc_cropped <- cycling_nyc %>%
  dplyr::group_by(mins) %>%
  sf::st_intersection() %>%
  dplyr::ungroup()


#interactive map of cycling catchment area

pal_fact <- leaflet::colorFactor(
  "YlOrRd",
  domain = cycling_nyc_cropped$mins,
  reverse = T,
  na.color = "transparent"
)

leaflet::leaflet(
  cycling_nyc_cropped
) %>%
  leaflet::addPolygons(
    fill = T,
    stroke = T,
    color = pal_fact,
    weight = 0.3,
    fillColor = ~pal_fact(mins),
    fillOpacity = 0.3
  ) %>%
  leaflet::addProviderTiles(
    "CartoDB.Voyager"
  ) %>%
  leaflet::addLegend(
    "bottomright",
    pal = pal_fact,
    values = cycling_nyc_cropped$mins,
    labels = cycling_nyc_cropped$mins,
    opacity = 0.5,
    title = "Cycling Distance in NYC"
  )

#static map of cycling catchment area

cycling_nyc_merc <- sf::st_transform(
  cycling_nyc_cropped,
  3857
)

nyc_layer <- maptiles::get_tiles(
  cycling_nyc_merc,
  provider = "CartoDB.Positron",
  zoom = 10
)

cycling_map <- ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = nyc_layer
  ) +
  geom_sf(
    data = cycling_nyc_merc,
    aes(
      fill = factor(mins),
      color = factor(mins),
      geometry = geometry
    ),
    size = 0.2,
    alpha = 0.5,
    inherit.aes = F
  ) +
  scale_fill_manual(
    name = "Minutes",
    values = hcl.colors(
      6, "YlGnBu"
    )
  ) +
  scale_color_manual(
    name = "Minutes",
    values = hcl.colors(
      6, "YlGnBu"
    )
  ) +
  guides(
    color = "none",
    fill = guide_legend(
      nrow = 1,
      byrow = T,
      keyheight = unit(7, "mm"),
      keywidth = unit(5, "mm"),
      title.position = "top",
      label.position = "bottom",
      label.hjust = 0.5
    )
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.margin = unit(
      c(
        t=0, r=0,
        b=0, l=0
      ), "lines"
    )
  ) +
  labs(
    title = "Cycling Distance in NYC from Grand Central Station"
  )

cycling_map

#query multiple travel modes

openrouteservice::ors_profile()

travel_modes <- c(
  "foot-walking",
  "cycling-regular",
  "driving-car"
)

travel_list <- c()

for(mode in travel_modes){
  travel_list[[mode]] <- {
    travel_nyc <- openrouteservice::ors_isochrones(
      locations = coords,
      profile = mode,
      range = 1800,
      interval = 1800,
      api_key = api_key,
      output = "sf"
    )
    travel_nyc
  }
}

travel_mode_nyc <- do.call(
  rbind, travel_list
)

travel_mode_nyc

#panel map of travel modes

travel_mode_nyc$mode <- factor(
  rownames(travel_mode_nyc),
  labels = c(
    "cycling",
    "driving",
    "walking"
  )
)

travel_nyc_merc <- sf::st_transform(
  travel_mode_nyc,
  3857
)

travel_map <- ggplot() +
  tidyterra::geom_spatraster_rgb(
    data = nyc_layer
  ) +
  geom_sf(
    data = travel_nyc_merc,
    aes(
      fill = factor(mode),
      color = factor(mode),
      geometry = geometry
    ),
    size = 0.2,
    alpha = 0.6,
    inherit.aes = F
  ) +
  scale_fill_manual(
    name = "Travel Mode",
    values = hcl.colors(
      3, "Set 3"
    )
  ) +
  scale_color_manual(
    values = hcl.colors(
      3, "Set 3"
    )
  ) +
  facet_wrap(~mode) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(
      c(
        t=0, r=0,
        b=0, l=0
      ), "lines"
    ),
    plot.title = element_text(
      size = 12,
      face = "bold",
      color = "grey20",
      hjust = 0.5
    ),
    strip.text = element_text(
      size = 10,
      color = "grey40",
      hjust = 0.5
    )
  ) +
  labs(
    title = "Travel Distance in NYC from Grand Central Station"
  )

travel_map






















