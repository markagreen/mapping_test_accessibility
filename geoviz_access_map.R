### GeoViz plot ###

# Libraries
library(tidyverse)
library(osmdata)
library(ggplot2)
library(ggmap)
library(sf)

# Load access data
access <- read_sf(dsn = "./liverpool_sites_walk_15m.shp") # Load
access_p <- access %>% # Convert to polygon format for mapping
  st_cast("POLYGON")

# Grab OpenStreetMap base layer
osm_map <- get_map(getbb("Liverpool"), maptype = "satellite", source = "osm") # maptypes toner-background also good

# Create map
map <- ggmap(osm_map) + # Plot OSM base later
        geom_sf(data = access_p, # Plot access data
            alpha = .5, # Set half see through
            inherit.aes = FALSE) +
        labs(x = "Latitude", y = "Longitude") # Labels
map # Plot

# Save
ggsave(map, filename = "./Maps/geovisualisation_plot.jpeg", dpi = 1200)


