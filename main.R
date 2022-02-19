remotes::install_github("walkerke/tidycensus")
library(tidyverse)
library(tidycensus)
library(janitor)
library(raster)
library(rgdal)
library(sf)
library(nngeo)
library(sp)
library(censusapi)
library(stringr)
library(tigris)
library(usmap)

options(tigris_use_cache = TRUE)

# Set Census API key
census_api_key("e2a0be8ff20ee18fd0d7eeac46bf8eda7de4a2d6", install = TRUE, overwrite = TRUE)
Sys.getenv("CENSUS_KEY")

# Collect cities' boundaries
wd <- "/Volumes/Yun Choi/columbia_university/gerrymandering-project/"
city_boundary_filepath <- paste0(wd, "RawData/CityCouncilFiles/District/")

# Cities sorted by population
cities <- c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", # 1-5
            "Philadelphia", "San Antonio", "San Diego", "Dallas", "San Jose", # 6-10 
            "Austin", "Jacksonville", "Fort Worth") # 11-13

# Cities' counties
# Four cities have multiple counties: New York (5); Houston (3); Dallas (5); Austion (3)
cities_counties <- list(new_york = c("New York", "Kings", "Bronx", "Richmond", "Queens"),
                        los_angeles = c("Los Angeles"),
                        chicago = c("Cook"),
                        houston = c("Harris$", "Fort Bend", "Montgomery"), # 201 - Harris (as opposed to Harrison)
                        phoenix = c("Maricopa"),
                        philadelphia = c("Philadelphia"),
                        san_antonio = c("Bexar"),
                        san_diego = c("San Diego"),
                        dallas = c("Dallas", "Collin$", "Denton", "Kaufman", "Rockwall"), # 085 - Collin (as opposed to Collingsworth)
                        san_jose = c("Santa Clara"),
                        austin = c("Travis", "Hays", "Williamson"),
                        jacksonville = c("Duval"),
                        fort_worth = c("Tarrant"))

# Cities' states
cities_states <- c("NY", "CA", "IL", "TX", "AZ", # 1-5
                   "PA", "TX", "CA", "TX", "CA", # 6-10
                   "TX", "FL", "TX") # 11-13

# Create a numeric version of county and state list
cities_counties_codes <- list()
cities_states_codes <- c()

for (i in 1:length(cities)) {
  
  state <- cities_states[i]
  
  cities_counties_codes[[i]] <- fips(state, 
       county = cities_counties[[i]]) %>%
    substr(3, 5)
  
  cities_states_codes[i] <- unique(fips_codes$state_code[fips_codes$state == state])
}

names(cities_counties_codes) <- str_replace_all(tolower(cities), " ", "_")


# Create a for loop that (1) reads individual cities' council districts shapefiles; (2) dissolves all districts; and (3) assign th
for (c in cities) {
  # Read city districts shape file
  city_council_districts <- st_read(
    paste0(city_boundary_filepath, c, "/", c, ".shp")
    ) %>%
    clean_names() %>%
    st_make_valid()
  
  # Dissolve polygons
  city_boundary <- st_union(city_council_districts$geometry) %>%
    # Remove unnecessary holes in the dissolved city boundary
    nngeo::st_remove_holes()
    # Change object class from 'sfc' to 'sf' as 'sfc' cannot be fed to spTransform function
    #st_sf() %>% st_cast()
  
  city_boundary_NAD83 <- st_transform(city_boundary,
                                # Census data CRS
                                "+proj=longlat +datum=NAD83 +no_defs")
  
  # Assign the city boundary as a new object
  # i.e.) "new_york_city_boundary", "chicago_city_boundary"
  assign(paste0(str_replace_all(tolower(c), " ", "_"), "_city_boundary"), 
         city_boundary_NAD83)
}

# Check if s2 is being used
# Documentation on s2 can be found here: https://r-spatial.github.io/sf/articles/sf7.html
sf_use_s2()

# Check if every city bounrary file has the correct CRS
# Print cities that has a CRS different from "+proj=longlat +datum=NAD83 +no_defs"
for (i in 1:length(cities)) {
  city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), "_city_boundary$")))[[1]]
  if (st_crs(city_boundary) == st_crs(new_york_city_boundary)) {
  } else {
    print(cities[i])
  }
}

# Load 2010 decennial variables table
census_decennial_variables <- load_variables(2010, "sf1", cache = TRUE)

# Prepare tract and block-level demographics data with geometry
for (i in 1:1) {
  for (l in c("tract", "block")) {
    data <- get_decennial(state = cities_states_codes[i],
                          county = cities_counties_codes[[i]],
                          geography = l,
                          year = 2010,
                          variables = c("black_p" = "P003003",
                                        "white_p" = "P003002",
                                        "hispanic_p" = "P005010",
                                        "asian_p" = "P003005",
                                        "total_p" = "P003001"),
                          geometry = TRUE)
    
    # Assign data as a new object
    # i.e.) "new_york_county_tract", "chicago_county_block"
    assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_county_", l), 
           data)
    
    # Rest for 5 seconds
    # Census API 
    # Sys.sleep(10)
  }
}

save(list = ls(pattern = "_county_tract$"),
     file = paste0(wd, "CleanedData/01_cities_county_tract.RData"))
save(list = ls(pattern = "_county_block$"),
     file = paste0(wd, "CleanedData/02_cities_county_block.RData"))

load(paste0(wd, "CleanedData/01_cities_county_tract.RData"))
load(paste0(wd, "CleanedData/02_cities_county_block.RData"))

# 
for (i in 1:length(cities)) {
  city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), "_city_boundary$")))[[1]]
  if (st_crs(city_boundary) == st_crs(new_york_city_boundary)) {
  } else {
    print(cities[i])
  }
}

for (i in 1:length(cities)) {
  # Retrieve city boundary
  city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                            "_city_boundary$")))[[1]]
  county_tracts <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                            "_county_tracts$")))[[1]]
  #county_blocks <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                            #"_county_blocks$")))[[1]]
  
  # Filter tracts and blocks within each city boundary only
  tract_index <- apply(st_intersects(county_tracts, city_boundary, sparse = FALSE), 1, any)
  #block_index <- apply(st_intersects(county_blocks, city_boundary, sparse = FALSE), 1, any)
  
  # 
  city_tracts_intersections <- st_intersection(county_tracts[tract_index, ], 
                                               city_boundary)
  #city_blocks_intersections <- st_intersection(county_blocks[block_index, ], 
                                               #city_boundary)
  
  # Assign the tracts and blocks boundaries as new objects
  # i.e.) "new_york_tracts_boundary", "chicago_tracts_boundary"
  assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_tracts_boundary"), 
         city_tracts_intersections)
  # i.e.) "new_york_blocks_boundary", "chicago_blocks_boundary"
  #assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_blocks_boundary"), 
         #city_blocks_intersections)
}

# Create an id column
new_york_city_tracts_intersections$id <- 1:nrow(new_york_city_tracts_intersections)

for (int in seq_along(new_york_city_tracts_intersections$id)) {
  new_york_city_tracts_intersections$value[int] = sum(sf[new_york_city_tracts_intersections$origins[[int]], ]$value, na.rm = TRUE)
}

new_york_city_tracts_intersections %>%
  #--- get area ---#
  mutate(area = as.numeric(st_area(.))) 



# Add partisanship data
precinct_returns_filepath <- "/Volumes/Yun Choi/columbia_university/gerrymandering-project/RawData/ElectionPrecinctLevelReturns/"

for (i in 1:length(cities)) {
  
  # Read state precinct returns shape file
  state_precinct_returns <- st_read(
    paste0(precinct_returns_filepath, 
           tolower(cities_states[i]), "_vest_20/", tolower(cities_states[i]), "_vest_20.shp")
  ) %>%
    clean_names() %>%
    filter(county %in% cities_counties[[i]]) %>%
    gather(key = "ballot_type", value = "ballot_count", -statefp, -countyfp, -county, -precinct, -geometry) %>%
    st_make_valid()
  
  state_precinct_returns_NAD83 <- st_transform(state_precinct_returns,
                                               "+proj=longlat +datum=NAD83 +no_defs")
  
  # Assign the city boundary as a new object
  # i.e.) "new_york_city_boundary", "chicago_city_boundary"
  assign(paste0(str_replace_all(tolower(s), " ", "_"), "_state_precinct_returns"), 
         state_precinct_returns_NAD83)
}

# Example - New York
## Tract-level first
new_york_tract_index <- apply(st_intersects(new_york_county_tracts, new_york_city_boundary, sparse = FALSE), 1, any)
new_york_city_tracts_intersections <- st_intersection(new_york_county_tracts[new_york_tract_index, ], 
                                                      new_york_city_boundary)

# Create tract-level spatially weighted demographic and partisanship data
new_york_tract_precinct_intersections <- st_intersection(new_york_city_tracts_intersections, 
                                                         ny_state_precinct_returns) %>%
  mutate(id = paste0(GEOID, "-", precinct)) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  group_by(GEOID) %>%
  mutate(weight = area / sum(area)) %>% 
  # Calculate area-weighted corn acreage by GEOID
  summarize(aw_pop = sum(weight * value),
            aw_ballot = sum(weight * ballot_count))

## Block-level
new_york_block_index <- apply(st_intersects(new_york_county_blocks, new_york_city_boundary, sparse = FALSE), 1, any)
new_york_city_blocks_intersections <- st_intersection(new_york_county_blocks[new_york_block_index, ], 
                                                      new_york_city_boundary)

# Create block-level spatially weighted demographic and partisanship data
new_york_block_precinct_intersections <- st_intersection(new_york_city_blocks_intersections, 
                                                         ny_state_precinct_returns) %>%
  mutate(id = paste0(GEOID, "-", precinct)) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  group_by(GEOID) %>%
  mutate(weight = area / sum(area)) %>% 
  # Calculate area-weighted corn acreage by GEOID
  summarize(aw_pop = sum(weight * value),
            aw_ballot = sum(weight * ballot_count))