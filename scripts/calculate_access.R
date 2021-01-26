### Calculating access to test sites ###

# Purpose: To calculate the mean distance of each postcode to its nearest test site and aggregate to Lower Super Output Areas

# Libraries
library(sf)
library(hereR) 

# Set API key for 'here'
set_key("") # You will need to register and request a key - is free

# Load data
sites <- read.csv("./ltc_18Jan.csv") # Location of test sites 

# Convert sites to spatial points format
sites_sp <- sites %>% 
  st_as_sf(coords = c("X", "Y")) %>% # Define co-ordinates
  st_set_crs(4326)

# Load in all postcodes
postcodes <- read.csv("./l_postcodes/nspd_2020_05.csv") # All 'L' postcodes as of May 2020 (from UKBorders)

# Convert postcodes to spatial points format
postcodes_sp <- postcodes %>% 
  st_as_sf(coords = c("long", "lat")) %>% # Define co-ordinates
  st_set_crs(4326)

# Calculate distance to each site for all postcodes
output <- route_matrix(origin = postcodes_sp, # Origin points - postcodes
             destination = sites_sp, # Where routing to (test sites)
             mode = "pedestrian", # Walking
             type = "shortest",
             traffic = FALSE, # If set as false then no need to define time for routing
             attribute = "distance") # Save distance

# Aggregate to postcodes
library(data.table)
output <- data.table(output)
postcode_agg <- output[, list(distance = min(distance), time = min(travelTime)), by = "origIndex"] # Select minimum time (i.e. nearest site) per postcode

# Join back onto postcodes
postcodes <- cbind(postcodes, postcode_agg)

# Save postcode distances
write.csv(postcodes, "./l_postcodes/nspd_nearest_site_18Jan.csv")

# Aggregate to LSOAs
postcodes <- data.table(postcodes)
lsoa_agg <- postcodes[, list(distance = mean(distance), time = mean(time)), by = "lsoa11"] # Take average for LSOAs
write.csv(lsoa_agg, "./l_postcodes/distance_to_nearest_site_18Jan21.csv") # Save
