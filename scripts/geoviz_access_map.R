###############################
### GeoVisualisation plots ####
###############################

# Purpose: Code for recreating the maps on the RGS geovisualisation blog

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

# Location of test sites
sites <- read.csv("./ltc.csv") # Load
sites_sp <- sites %>% # Convert sites to spatial points format
  st_as_sf(coords = c("X", "Y")) %>% # Define co-ordinates (or X / Y)
  st_set_crs(4326)


## Create map with standard geographic background ##

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


## Create fancier map for RGS blog ##

# Grab Liverpool
box <- getbb("Liverpool, UK") # Define bounding box for Liverpool

# Collect road network in Liverpool
roads <- box %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value=c("motorway", "trunk",
                          "primary","secondary", 
                          "tertiary","motorway_link",
                          "trunk_link","primary_link",
                          "secondary_link",
                          "tertiary_link")) %>%
  osmdata_sf()

# # Map road networks
# ggplot() +
#   geom_sf(data = roads$osm_lines,
#           aes(color=highway),
#           size = .4,
#           alpha = .65)+
#   theme_void()

# Collect street networks
streets <- box %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "service","unclassified",
                            "pedestrian", "footway",
                            "track","path")) %>%
  osmdata_sf()

# # Map streets
# ggplot() +
#   geom_sf(data = streets$osm_lines,
#           aes(color=highway),
#           size = .4,
#           alpha = .65)+
#   theme_void()

# Load Great Britain outline for basemap
gb_outline <- read_sf(dsn = "./Shapefiles/GB outline/Local_Authority_Districts_(December_2017)_Boundaries_in_Great_Britain.shp") # Load GB outline (from https://geoportal.statistics.gov.uk/datasets/ae90afc385c04d869bc8cf8890bd1bcd_1)
st_crs(gb_outline) # Check co-ordinate reference system
gb_outline <- st_transform(gb_outline, 4326) # Set as same CRS as access shapefile

# Final map
makeitgrey <- rgb(0.42,0.449,0.488)
map2 <- ggplot() +
  geom_sf(data = gb_outline, # Plot basemap of land
          inherit.aes = FALSE,
          lwd = 0, # Get rid of boundary lines
          #fill = "white") + # Make white
          fill = rgb(0.203,0.234,0.277)) + # Make dark grey
  geom_sf(data = streets$osm_lines, # Plot streets
          inherit.aes = FALSE,
          col = makeitgrey, # Define colour
          size = .2, # Set size of streets
          alpha = .65) + # Make partial see through
  geom_sf(data = roads$osm_lines, # Plot roads
          inherit.aes = FALSE,
          col = makeitgrey,
          size = .6,
          alpha = .65) +
  geom_sf(data = access_p, # Plot accessibility to test sites
          alpha = 0, # Set see through
          lwd = 0.5, 
          col = "red") + # Just colour in outline
  geom_sf(data = sites_sp, # Plot accessibility to test sites
          alpha = 1, # Set see through
          lwd = 0.5, 
          col = "white") + 
  coord_sf(xlim = c(min(box[1,]), max(box[1,])), # Define co-ordinates to plot
           ylim = c(min(box[2,]), max(box[2,])),
           expand = FALSE) +
  theme(legend.position = F) + theme_void() + # Get rid of ggplot features
  #theme(panel.background=element_rect(fill = rgb(0.92,0.679,0.105))) # Set background colour yellow
  # theme(panel.background=element_rect(fill = rgb(0.41,0.70,0.91))) # Set background dark grey blue
  theme(panel.background=element_rect(fill = rgb(0.38,0.70,0.83))) # Set background to teal blue as per Alex's suggestion - well he is Prof of GIS or something
  #theme(panel.background=element_rect(fill = rgb(0.9,0.9,0.9))) # Set background colour grey
map2

# Save - general for me
ggsave(map2, filename = "./Maps/geovisualisation_plot3.jpeg", dpi = 1200)

# Save - for RGS requirements
ggsave("./Maps/main_image.jpeg", width = 9.691666666666666, height = 5.441666666666666, dpi = 1200) # Main image (width = 1163 px, height = 653 px)
ggsave(map2, filename = "./Maps/page_banner.jpeg", width = 12.833333333333334, height = 8.791666666666666, dpi = 1200) # Page banner (width = 1540 px, height = 1055 px)
ggsave(map2, filename = "./Maps/instagram.jpeg", width = 9, height = 9, dpi = 1200) # Instagram (width = 1080 px, height = 1080 px)
ggsave(map2, filename = "./Maps/twitter.jpeg", width = 10, height = 5.625, dpi = 1200) # Twitter (width = 1200 px, height = 675 px)
ggsave(map2, filename = "./Maps/content_card.jpeg", width = 12.833333333333334, height = 12.833333333333334, dpi = 1200) # Twitter (width = 1540 px, height = 1540 px)
