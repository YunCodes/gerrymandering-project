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
    nngeo::st_remove_holes() %>%
    # Change object class from 'sfc' to 'sf'
    st_sf() %>% st_cast()
  
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
  }
}

# Save county_tract and county_block datasets into RData
save(list = ls(pattern = "_county_tract$"),
     file = paste0(wd, "CleanedData/01_cities_county_tract.RData"))
save(list = ls(pattern = "_county_block$"),
     file = paste0(wd, "CleanedData/02_cities_county_block.RData"))

# Load county_tract and county_block datasets
load(paste0(wd, "CleanedData/01_cities_county_tract.RData"))
load(paste0(wd, "CleanedData/02_cities_county_block.RData"))


for (i in 1:length(cities)) {
  for (l in c("tract", "block")) {
    # Retrieve city boundary
    city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                              "_city_boundary$")))[[1]]
    # Retrieve tract- or bloack-level county data
    data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                     "_county_", l)))[[1]]
    
    # Filter tracts and blocks within each city boundary only
    index <- apply(st_intersects(data, city_boundary, sparse = FALSE), 1, any)
    filtered <- data[index, ]
    
    # Assign data as a new object
    # i.e.) "new_york_county_tract", "chicago_county_block"
    assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_city_", l), 
           filtered)
  }
}

# Save county_tract and county_block datasets into RData
save(list = ls(pattern = "_city_tract$"),
     file = paste0(wd, "CleanedData/03_cities_city_tract.RData"))
save(list = ls(pattern = "_city_block$"),
     file = paste0(wd, "CleanedData/04_cities_city_block.RData"))

# Load ccity_tract and city_block datasets
load(paste0(wd, "CleanedData/03_cities_city_tract.RData"))
load(paste0(wd, "CleanedData/04_cities_city_block.RData"))

# Import partisanship data
precinct_returns_filepath <- "/Volumes/Yun Choi/columbia_university/gerrymandering-project/RawData/ElectionPrecinctLevelReturns/"

for (i in 1:length(unique(cities_states))) {
  
  state <- unique(cities_states)[i]
  
  # Read state precinct returns shape file
  state_precinct_returns <- st_read(
    paste0(precinct_returns_filepath, 
           tolower(state), "_vest_20/", 
           tolower(state), "_vest_20.shp")
  ) %>%
    clean_names() %>%
    st_make_valid()
  
  # Convert CRS to NAD83
  state_precinct_returns_NAD83 <- st_transform(state_precinct_returns,
                                               "+proj=longlat +datum=NAD83 +no_defs")
  
  # Assign the city precinct_returns as a new object
  assign(paste0(str_replace_all(tolower(state), " ", "_"), "_state_precinct_returns"), 
         state_precinct_returns_NAD83)
}

# Save county_precinct_returns datasets into RData
save(list = ls(pattern = "_state_precinct_returns$"),
     file = paste0(wd, "CleanedData/05_cities_state_precinct_returns.RData"))

# Load ccity_tract and city_block datasets
load(paste0(wd, "CleanedData/05_cities_state_precinct_returns.RData"))

# This code identifies state_precinct_returns with invalid geometry
for (i in 1:length(cities)){
  state <- cities_states[i]
  data <- mget(ls(pattern = paste0("^", tolower(state), "_state_precinct_returns")))[[1]] 
  
  if (st_is_valid(data) == TRUE) {
  } else {
    print(cities[i])
  }
}
## Remove 'jacksonville', 'forth worth' for now

# Standardize city_precinct_returns data
# Data structure and column names vary by state
precinct_returns_column_names <- list()

for (i in 1:11) {
  city_precinct_returns <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                                    "_city_precinct_returns$")))[[1]]

  precinct_returns_column_names[[i]] <- colnames(city_precinct_returns)
}

names(precinct_returns_column_names) <- str_replace_all(tolower(cities[1:11]), " ", "_")

precinct_id_columns <- c()

select(starts_with("g20pre") | geometry)


# NY
new_york_city_precinct_returns %>% colnames()

new_york_city_precinct_returns %>% 
  create(precinct_id = )
  gather(key = "ballot_type", value = "count",
         "g20predbid", "g20prertru", "g20preljor", "g20preghaw", "g20preipie", "g20preowri")


# CA - 
los_angeles_city_precinct_returns %>% colnames()

los_angeles_city_precinct_returns %>%
  gather(key = "ballot_type", value = "count",
         "g20predbid", "g20prertru", "g20preljor", "g20preghaw", "g20preafue", "g20preplar")

chicago_city_precinct_returns %>%
  

# TX
houston_city_precinct_returns %>% colnames()
# PA
philadelphia_city_precinct_returns %>% colnames()
# AZ
phoenix_city_precinct_returns %>% colnames()





# Filter precincts within city boundary only
for (i in 1:length(cities)) {
  
  state <- cities_states[i]
    # Retrieve city boundary
  city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                            "_city_boundary$")))[[1]]
  # Retrieve state_precinct_returns data
  data <- mget(ls(pattern = paste0("^", tolower(state), "_state_precinct_returns")))[[1]] %>%
    st_make_valid()
  
  # Filter precincts within each city boundary only
  index <- apply(st_intersects(data, city_boundary, sparse = FALSE), 1, any)
  filtered <- data[index,]
  
  # Add 'precinct_id' column, and gather columns 
    
  # Assign data as a new object
  assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_city_precinct_returns"), 
         filtered)
}

# Save county_precinct_returns datasets into RData
save(list = ls(pattern = "_city_precinct_returns$"),
     file = paste0(wd, "CleanedData/06_cities_city_precinct_returns.RData"))

load(paste0(wd, "CleanedData/06_cities_city_precinct_returns.RData"))


# This code takes ages!!!
# Create tract-level spatially weighted demographic and partisanship data
for (i in 1:1){
  partisanship_data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                                "_city_precinct_returns$")))[[1]]
  for (l in c("tract")) {
    
    demographic_data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                                 "_city_", l, "$")))[[1]]

    demographic_partisanship_intersections <- st_intersection(demographic_data, 
                                                              partisanship_data) %>%
      mutate(id = paste0(GEOID, "-", precinct)) %>%
      mutate(area = as.numeric(st_area(.))) %>%
      group_by(GEOID) %>%
      mutate(weight = area / sum(area)) %>% 
      # Calculate area-weighted corn acreage by GEOID
      summarize(aw_pop = sum(weight * value),
                aw_ballot = sum(weight * ballot_count))
    
    assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_dem_partisan_intersections"), 
           demographic_partisanship_intersections)
  }
}

# Export tract- and block-level city demographic data as shp files 
for (i in 1:11) {
  for (l in c("tract", "block")) {
    demographic_data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                                 "_city_", l, "$")))[[1]] %>%
      # Reshape to a wider form
      spread(variable, value)
    
    st_write(demographic_data, 
             paste0(wd, "CleanedData/CityDemographic/", str_to_title(l), "/",
             str_replace_all(tolower(cities[i]), " ", "_"), "_city_demographic_", l, ".shp"),
             # Overwrite if the data already exists
             append=FALSE)
  }
}

new_york_city_block %>%
  spread(variable, value)

# Export precinct-level city election returns data as shp files
for (i in 1:11) {
  partisanship_data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                                "_city_precinct_returns$")))[[1]]
    st_write(partisanship_data, 
             paste0(wd, "CleanedData/CityElectionReturns/", 
                    str_replace_all(tolower(cities[i]), " ", "_"), "_city_election_returns.shp"))
}

# Format data in the way that can be easily calculated in QGIS
new_york_city_precinct_returns %>%
  mutate(GEOID = paste0(statefp, county))

# Import new_york_block_precinct_intersection exported from QGIS
new_york_block_precinct_intersection <- st_read("/Volumes/Yun Choi/columbia_university/gerrymandering-project/CleanedData/QGISOutput/Block/new_york_block_precinct_intersection.shp")

new_york_block_precinct_intersection_weighted <- new_york_block_precinct_intersection %>%
  as_tibble() %>%
  mutate(id = paste0(GEOID, "-", precinct)) %>%
  # block-level aggregation
  group_by(GEOID) %>%
  mutate(weight = area / sum(area)) %>% 
  # Calculate area-weighted value by GEOID
  summarize(sw_g20predbid = sum(weight * g20predbid),
            sw_g20prertru = sum(weight * g20prertru)) %>% 
  # Attach back block-level demographic data with geography
  left_join(new_york_city_block_wide, by = "GEOID") 

st_write(new_york_block_precinct_intersection_weighted, 
         "/Volumes/Yun Choi/columbia_university/gerrymandering-project/CleanedData/CityIntersection/Block/new_york_block_precinct_intersection_weighted.shp",
         append = FALSE)

view(new_york_block_precinct_intersection_weighted)

new_york_city_block_wide <- new_york_city_block %>%
  spread(variable, value)

## Example from Gustavo
example <- st_read("/Volumes/Yun Choi/columbia_university/gerrymandering-project/RawData/ExampleFromGustavo/83149113-68a5-4dca-8a89-a7909c97c54a2020329-1-104jw7r.ypxp.shp") %>%
  clean_names()
