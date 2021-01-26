###############################################
### Identifying new locations of test sites ###
###############################################

# Libraries
library(maxcovr) # For Location-Allocation modelling
library(dplyr) # For data wrangling
library(purrr)
library(leaflet) # For mapping
library(sf)
library(ggplot2) # For plotting

# Load data
sites <- read.csv("./ltc.csv") # Location of test sites
oas <- read_sf(dsn = "./liverpool_oa/england_oa_2011.shp") # Output areas
# st_crs(oas) # Check CRS - see 'user input' at top
oas <- st_transform(oas, 4326) # Transform to same CRS as lat/long

# Calculate centroid of output areas
oa_centres <- st_centroid(oas) # Find centroid
oas_sf <- st_as_sf(oa_centres, coords = c("lon","lat")) %>% # Re-format data
  st_set_crs(4326)
oas_coords <- unlist(st_geometry(oas_sf)) %>% # Convert to Lat/Long
  matrix(ncol=2,byrow=TRUE) %>% 
  as_tibble() %>% 
  setNames(c("lon","lat"))

# Map existing sites
leaflet() %>%
  addCircleMarkers(data = sites, # Data for plotting points
                   lng = sites$X, # Longitude
                   lat = sites$Y, # Latitude
                   radius = 1, # Display as point
                   color = "steelblue") %>% # Colour
  addCircles(data = sites, # Data for plotting points with radius around them
             lng = sites$X, # Longitude
             lat = sites$Y, # Latitude
             radius = 1000, # Add 1km buffer radius (straight line distance) around each site
             stroke = TRUE, # Draw border of radius
             fill = NULL, # Do not fill within
             opacity = 0.8, # How see through it is
             weight = 2, # Width of border/lines
             color = "coral") %>% # Colour
  addProviderTiles("CartoDB.Positron") %>% # Add on map layer below
  setView(lng = median(sites$X), # Define where to centre the map
          lat = median(sites$Y),
          zoom = 12)

# Maximise coverage of test sites by population

# Describe current coverage
coverage(current_sites, 
         oas_coords,
         distance_cutoff = 100)

# Edit inputs to match required format
current_sites <- sites
current_sites$Location <- NULL
names(current_sites)[names(current_sites)=="X"] <- "long"
names(current_sites)[names(current_sites)=="Y"] <- "lat"
names(oas_coords)[names(oas_coords)=="lon"] <- "long"

# Load in possible new test site locations
postcodes <- read.csv("./l_postcodes/nspd_2020_05.csv") # All 'L' postcodes as of May 2020
pcs <- postcodes[,c("lat", "long")] # Subset all postcodes to match required format

# Run location-allocation model to select five best new sites
mc_5 <- max_coverage(existing_facility = current_sites, # List of current test sites
                      proposed_facility = pcs, # List of all possible postcodes
                      user = oas_coords, # Data to optimise to (i.e. output area centroids)
                      n_added = 12, # How many facilities
                      distance_cutoff = 1000) # 1km bufer radius 
summary(mc_5) # Plot summary statistics
mc_5$facility_selected[[1]] # List sites selected

# Map new and existing sites
leaflet() %>%
  ## Plot exsting site locations ##
  addCircleMarkers(data = sites, # Data for plotting points
                   lng = sites$X, # Longitude
                   lat = sites$Y, # Latitude
                   radius = 1, # Display as point
                   color = "steelblue") %>% # Colour
  addCircles(data = sites, # Data for plotting points with radius around them
             lng = sites$X, # Longitude
             lat = sites$Y, # Latitude
             radius = 1000, # Add 1km buffer radius (straight line distance) around each site
             stroke = TRUE, # Draw border of radius
             fill = NULL, # Do not fill within
             opacity = 0.8, # How see through it is
             weight = 2, # Width of border/lines
             color = "coral") %>% # Colour
  ## Plot proposed new site locations ##
  addCircleMarkers(data = mc_5$facility_selected[[1]], # Data for plotting new sites as points
                   radius = 1, 
                   color = "green") %>% 
  addCircles(data = mc_5$facility_selected[[1]], 
             radius = 1000,
             fill = FALSE,
             weight = 3,
             color = "green") %>%
  addProviderTiles("CartoDB.Positron") %>% # Add on map layer below
  setView(lng = median(sites$X), # Define where to centre the map
          lat = median(sites$Y),
          zoom = 12)

# Can assess performance of a range of number of facilities
n_add_vec <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) # Select range of new sites
map_mc_model <- map_df(.x = n_add_vec, # Model
                         .f = ~max_coverage(existing_facility = current_sites,
                                            proposed_facility = pcs, 
                                            user = oas_coords,
                                            distance_cutoff = 800,
                                            n_added = .))

# Assess model performance
map_cov_results <- bind_rows(map_mc_model$model_coverage) # Sort data for required format
bind_rows(map_mc_model$existing_coverage[[1]], # Plot
          map_cov_results) %>%
  ggplot(aes(x = factor(n_added),
             y = pct_cov)) + 
  geom_point() +
  geom_line(group = 1) + 
  xlab("Number of new test sites") +
  ylab("Percentage of output areas covered")

# What if just want to priortise new test sites in deprived areas?
deprived_pcs <- postcodes[postcodes$imd < 6569,] # Select deprived postcodes only (20% most deprived areas)
deprived_pcs <- deprived_pcs[,c("lat", "long")] # Subset all postcodes to match required format

# Run location-allocation model to select five best new sites
dep_5 <- max_coverage(existing_facility = current_sites, 
                     proposed_facility = deprived_pcs,
                     user = oas_coords, 
                     n_added = 5, 
                     distance_cutoff = 1000)  

# Map existing and targeted sites
leaflet() %>%
  ## Plot exsting site locations ##
  addCircleMarkers(data = sites, # Data for plotting points
                   lng = sites$X, # Longitude
                   lat = sites$Y, # Latitude
                   radius = 1, # Display as point
                   color = "steelblue") %>% # Colour
  addCircles(data = sites, # Data for plotting points with radius around them
             lng = sites$X, # Longitude
             lat = sites$Y, # Latitude
             radius = 1000, # Add 1km buffer radius (straight line distance) around each site
             stroke = TRUE, # Draw border of radius
             fill = NULL, # Do not fill within
             opacity = 0.8, # How see through it is
             weight = 2, # Width of border/lines
             color = "coral") %>% # Colour
  ## Plot proposed new site locations ##
  addCircleMarkers(data = dep_5$facility_selected[[1]], # Data for plotting new sites as points
                   radius = 1, 
                   color = "green") %>% 
  addCircles(data = dep_5$facility_selected[[1]], 
             radius = 1000,
             fill = FALSE,
             weight = 3,
             color = "green") %>%
  addProviderTiles("CartoDB.Positron") %>% # Add on map layer below
  setView(lng = median(sites$X), # Define where to centre the map
          lat = median(sites$Y),
          zoom = 12)

