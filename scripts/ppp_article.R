#######################################
### People, Places and Policy plots ###
#######################################

# Purpose: Code for recreating the maps on the PPP article.

# Libraries
library(tidyverse)
library(osmdata)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(ggmap)
library(dplyr)
library(sf)

# Load access data
access <- read_sf(dsn = "./liverpool_sites_walk_15m.shp") # Load
access_p <- access %>% # Convert to polygon format for mapping
  st_cast("POLYGON") %>%
  st_set_crs(4326)

# Location of test sites
sites <- read.csv("./ltc.csv") # Load
sites_sp <- sites %>% # Convert sites to spatial points format
  st_as_sf(coords = c("X", "Y")) %>% # Define co-ordinates (or X / Y)
  st_set_crs(4326)


## Create map with standard geographic background ##

# Grab OpenStreetMap base layer
osm_map <- get_map(getbb("Liverpool"), maptype = "satellite", source = "osm") # maptypes toner-background also good

# Create map
map1 <- ggmap(osm_map) + # Plot OSM base later
  geom_sf(data = access_p, # Plot access data
          alpha = .5, # Set half see through
          inherit.aes = FALSE) +
  geom_sf(data = sites_sp, # Repeat but plot points of site location
          alpha = 1, # Set opaque
          lwd = 0.5, 
          col = "black",
          inherit.aes = FALSE) + 
  #labs(x = "Latitude", y = "Longitude") # Labels
  theme(axis.title.x = element_blank(), # Drop axis labels
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
map1 # Plot


## Create uptake map ##

# Load and clean uptake data (apologies but cannot share due to data access requirements)
uptake <- read.csv("./uptake data/uptake_lsoa.csv") # Load data
pop <- read.csv("./uptake data/lsoa_pop_2019_small.csv") # Load population data (2019 mid year ONS population estimates)
uptake <- merge(uptake, pop, by = "lsoa11cd", all.x = TRUE) # Join together
uptake$prop_test <- uptake$total / uptake$population # Calculate proportion of pop who got a test
uptake$quartile <- ntile(uptake$prop_test, 4) # Define quartiles

# Join and join data onto LSOA shapefiles
lsoas <- read_sf("./liverpool_lsoa/england_lsoa_2011.shp") # Load shapefiles
lsoas <- merge(lsoas, uptake, by.x = "code", by.y = "lsoa11cd", all.x = TRUE) # Join data

# Plot
map2 <- ggplot() +
  geom_sf(data = lsoas, aes(fill = factor(quartile)), lwd = 0.1) +
  scale_fill_brewer(palette = "Greens", name = "", labels = c("1", "2", "3", "4")) +
  theme(axis.title.x = element_blank(), # Drop axis labels
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(legend.position = "bottom")
map2


## Create location allocation map ##

# Run location allocation model
source("./location_allocation_model.R") 

# Save proposed new locations as spatial object
newsites_sp <- mc_5$facility_selected[[1]] %>% # Convert sites to spatial points format
  st_as_sf(coords = c("long", "lat")) %>% # Define co-ordinates
  st_set_crs(4326) # Set to CRS that recognises distance as m

# Create map
map3 <- ggmap(osm_map) + # Plot OSM base later
  geom_sf(data = access_p, # Plot access data
          alpha = .5, # Set half see through
          inherit.aes = FALSE) +
  geom_sf(data = sites_sp, # Repeat but plot points of site location
          alpha = 1, # Set opaque
          lwd = 0.5, 
          col = "black",
          inherit.aes = FALSE) + 
  geom_sf(data = newsites_sp, # Repeat but plot proposed new sites
          alpha = 1, # Set opaque
          lwd = 1, 
          col = "blue",
          inherit.aes = FALSE) +
  theme(axis.title.x = element_blank(), # Drop axis labels
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
map3 # Plot


## Put all three maps together into final plot ##

# Combine maps
final_map <- map1 + map2 + map3 + plot_annotation(tag_levels = 'A', # Define plots
  title = 'Assessing asymptomatic test site coverage in Liverpool (November 2020)', # Add titles
  subtitle = 'A: 15 min walking distance around each test site (black). B: Test uptake (quartiles with 1 = low, 4 = high).
C: Proposed new sites to maximise geographic coverage (blue) compared to existing coverage (black).', 
  theme = theme(plot.title = element_text(face = "bold"))) # Make title bold
final_map # Print

# Save
ggsave(final_map, filename = "./Maps/ppp_plot.jpeg", dpi = 1200)




