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
                        houston = c("Harris", "Fort Bend", "Montgomery"), # 201 - Harris (as opposed to Harrison)
                        phoenix = c("Maricopa"),
                        philadelphia = c("Philadelphia"),
                        san_antonio = c("Bexar"),
                        san_diego = c("San Diego"),
                        dallas = c("Dallas", "Collin", "Denton", "Kaufman", "Rockwall"), # 085 - Collin (as opposed to Collingsworth)
                        san_jose = c("Santa Clara"),
                        austin = c("Travis", "Hays", "Williamson"),
                        jacksonville = c("Duval"),
                        fort_worth = c("Tarrant"),
                        columbus = c("Delaware", "Fairfield", "Franklin"),
                        indianapolis = c("Marion"),
                        charlotte = c("Mecklenburg"),
                        san_francisco = c("San Francisco"),
                        seattle = c("King"),
                        denver = c("Denver"),
                        washington = c(), # 
                        nashville = c("Davidson"),
                        oklahoma_city = c("Oklahoma", "Canadian", "Cleveland", "Pottawatomie"),
                        el_paso = c("El Paso"),
                        boston = c("Suffolk"),
                        portland = c("Multnomah", "Washington", "Clackamas"),
                        las_vegas = c("Clark"))

# Cities' states
cities_states <- c("NY", "CA", "IL", "TX", "AZ", # 1-5
                   "PA", "TX", "CA", "TX", "CA", # 6-10
                   "TX", "FL", "TX", "OH", "IN", # 11-15
                   "NC", "CA", "WA", "CO", "DC", # 16-20
                   "TN", "OK", "TX", "MA", "OR", # 20-25
                   "NV")

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
  
  city_council_districts_NAD83 <- st_transform(city_council_districts,
                                               "+proj=longlat +datum=NAD83 +no_defs")
  
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

  st_write(city_council_districts_NAD83, 
           paste0(wd, "CleanedData/CityCouncilDistrict/", 
                  str_replace_all(tolower(c), " ", "_"), "_city_council_district.shp"),
           append = FALSE)
  
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
for (i in 1) {
  for (l in c("block")) {
    data <- get_decennial(state = cities_states_codes[i],
                          county = cities_counties_codes[[i]],
                          geography = l,
                          year = 2010,
                          variables = c("black_p" = "P005004", #"P003003",
                                        "white_p" = "P005003", #"P003002",
                                        "hispanic_p" = "P005010",
                                        "asian_p" = "P005006", #"P003005",
                                        "total_p" = "P005001"), #"P003001"),
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


for (i in 1) {
  for (l in c("block")) {
    # Retrieve city boundary
    city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                              "_city_boundary$")))[[1]]
    # Retrieve tract- or block-level county data
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

# Load city_tract and city_block datasets
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

for (i in 12) {
  city_precinct_returns <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                                    "_city_precinct_returns$")))[[1]]

  precinct_returns_column_names[[i]] <- colnames(city_precinct_returns)
}

names(precinct_returns_column_names) <- str_replace_all(tolower(cities[12]), " ", "_")



# NY
new_york_city_precinct_returns %>% colnames()
# CA
los_angeles_city_precinct_returns %>% colnames()
# IL
chicago_city_precinct_returns %>% colnames()
# TX
houston_city_precinct_returns %>% colnames()
# PA
philadelphia_city_precinct_returns %>% colnames()
# AZ
phoenix_city_precinct_returns %>% colnames()

# Create list with columns of interest
demographic_columns <- c("GEOID", "NAME", "geometry", "asian_p", "black_p", "hispanic_p", "total_p", "white_p")
demographic_columns_abrv <- c("GEOID", "NAME", "geometry", "asian_p", "black_p", "hspnc_p", "total_p", "white_p")
partisan_columns <- c("g20predbid", "g20prertru")
partisan_columns_abrv <- c("sw_g20prd", "sw_g20prr")

# first element is the id for precinct; second is for council district 
cities_precinct_council_district_id_columns <- list(new_york = c("precinct", "coun_dist"), # NY
                                                    los_angeles = c("srprec", "district"), # CA
                                                    chicago = c("name20", "ward"), # IL
                                                    houston = c("pctkey", "district"),# TX
                                                    phoenix = c("pctnum", "district"), # AZ
                                                    philadelphia = c("name_2", "district"), # PA
                                                    san_antonio = c("pctkey", "district"),
                                                    san_diego = c("srprec", "district"),
                                                    dallas = c("pctkey", "district"), 
                                                    san_jose = c("srprec", "district"),
                                                    austin = c("pctkey", "council_di"),
                                                    jacksonville = c("", "district"),
                                                    fort_worth = c("", "district"))


# Filter precincts within city boundary only
for (i in 12) {
  
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



# Export tract- and block-level city demographic data as shp files 
for (i in 1) {
  for (l in c("block")) {
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

# Export precinct-level city election returns data as shp files
for (i in 1:11) {
  partisanship_data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                                "_city_precinct_returns$")))[[1]]
    st_write(partisanship_data, 
             paste0(wd, "CleanedData/CityElectionReturns/", 
                    str_replace_all(tolower(cities[i]), " ", "_"), "_city_election_returns.shp"))
}

for (i in 1:11) {
  
  precinct_id <- cities_precinct_council_district_id_columns[[i]][1]
  council_district_id <- cities_precinct_council_district_id_columns[[i]][2]
  
  # Import city_block_precinct_council_district intersection exported from QGIS
  block_precinct_council_district_intersection <- st_read(
    paste0(
      wd,
      "CleanedData/QGISOutput/Block_Precinct_CouncilDistrict/", # path to folder 
      str_replace_all(tolower(cities[i]), " ", "_"), 
      "_block_precinct_council_district_intersection.shp"
      )
    ) %>%
    rename(prct_id = all_of(precinct_id), cd_id = all_of(council_district_id))
  
  block_geometry <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                                "_city_block$")))[[1]] %>%
      spread(variable, value) %>%
      dplyr::select(GEOID)
  
  council_district_geometry <- st_read(
    paste0(
      wd,
      "CleanedData/CityCouncilDistrict/", 
      str_replace_all(tolower(cities[i]), " ", "_"), 
      "_city_council_district.shp")
    ) %>%
    dplyr::select(all_of(council_district_id))
  
  intersection_geometry <- block_precinct_council_district_intersection %>%
      dplyr::select(GEOID, prct_id, cd_id)
    
    # Add intersection-level spatially weighted estimates   
    block_precinct_council_district_intersection_final <- block_precinct_council_district_intersection %>%
      as_tibble() %>% 
      mutate(
        # Create weights
        block_wgt = area/block_area,
        prct_wgt = area/prct_area,
        cd_wgt = area/cd_area, 
        # SW - block
        sw_asian_p = asian_p * block_wgt,
        sw_black_p = black_p * block_wgt,
        sw_hispanic_p = hispanic_p * block_wgt,
        sw_white_p = white_p * block_wgt,
        sw_total_p = total_p * block_wgt,
        # SW - prct
        sw_g20predbid = g20predbid * prct_wgt,
        sw_g20prertru = g20prertru * prct_wgt
      ) %>%
      # Standardize data structure 
      dplyr::select(GEOID, all_of(demographic_columns), block_area, 
                    prct_id, all_of(partisan_columns), prct_area,
                    cd_id, cd_area,
                    area, block_wgt, prct_wgt, cd_wgt,
                    sw_asian_p, sw_black_p, sw_hispanic_p, sw_white_p, sw_total_p,
                    sw_g20predbid, sw_g20prertru) %>%
      # Attach back intersection-level geography
      left_join(intersection_geometry, by = c("GEOID", "prct_id", "cd_id"))
    
    # Create block-level intersection
    intersection_weighted_block <- block_precinct_council_district_intersection_final %>%
      as_tibble() %>%
      group_by(GEOID) %>%
      summarise(
        sw_asian_p = sum(sw_asian_p, na.rm = TRUE),
        sw_black_p = sum(sw_black_p, na.rm = TRUE),
        sw_hispanic_p = sum(sw_hispanic_p, na.rm = TRUE),
        sw_white_p = sum(sw_white_p, na.rm = TRUE),
        sw_total_p = sum(sw_total_p, na.rm = TRUE),
        sw_g20predbid = sum(sw_g20predbid, na.rm = TRUE),
        sw_g20prertru = sum(sw_g20prertru, na.rm = TRUE)
        ) %>%
      # Attach back block-level demographic data with geography
      left_join(block_geometry, by = "GEOID")
    
    # # Create council district-level intersection
    intersection_weighted_council_district <- block_precinct_council_district_intersection_final %>%
      as_tibble() %>% 
      group_by(cd_id) %>%
      summarise(
        sw_asian_p = sum(sw_asian_p, na.rm = TRUE),
        sw_black_p = sum(sw_black_p, na.rm = TRUE),
        sw_hispanic_p = sum(sw_hispanic_p, na.rm = TRUE),
        sw_white_p = sum(sw_white_p, na.rm = TRUE),
        sw_total_p = sum(sw_total_p, na.rm = TRUE),
        sw_g20predbid = sum(sw_g20predbid, na.rm = TRUE),
        sw_g20prertru = sum(sw_g20prertru, na.rm = TRUE)
      ) %>%
      # Attach back council district geography
      left_join(council_district_geometry, by = c("cd_id" = council_district_id))
    
    # Save new objects in the environment
    assign(str_replace_all(tolower(cities[i]), " ", "_"),
           "_intersection_final", 
           block_precinct_council_district_intersection_final)
    
    assign(str_replace_all(tolower(cities[i]), " ", "_"),
           "_intersection_weighted_block", 
           intersection_weighted_block)
    
    assign(str_replace_all(tolower(cities[i]), " ", "_"),
           "_intersection_weighted_council_district", 
           intersection_weighted_council_district)
    
    # Export new objects
    st_write(
      block_precinct_council_district_intersection_final,
      paste0(
        wd, 
        "CleanedData/CityIntersection/Intersection/",
        str_replace_all(tolower(cities[i]), " ", "_"),
        "_intersection"),
      driver = "ESRI Shapefile", append = FALSE)
    
    st_write(
      intersection_weighted_block, 
      paste0(
        wd, 
        "CleanedData/CityIntersection/Block/", 
        str_replace_all(tolower(cities[i]), " ", "_"), 
        "_intersection_weighted_block.shp"),
      driver = "ESRI Shapefile", append = FALSE)
    
    st_write(
      intersection_weighted_council_district, 
      paste0(
        wd, 
        "CleanedData/CityIntersection/CouncilDistrict/", 
        str_replace_all(tolower(cities[i]), " ", "_"), 
        "_intersection_weighted_council_district.shp"),
      driver = "ESRI Shapefile", append = FALSE)
    
}

for (i in 1:11) {
  
  intersection_weighted_block <-  mget(
    ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                       "_intersection_weighted_block$")))[[1]]
  
  intersection_weighted_council_district <- mget(
      ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                         "_intersection_weighted_council_district$")))[[1]]
  
  city_demographic <- mget(
    ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                        "_city_block$")))[[1]] %>%
    spread(variable, value)
  
  citywide_demographic <- city_demographic %>%
    as_tibble() %>%
    summarise(total_asian_p = sum(asian_p),
              total_black_p = sum(black_p),
              total_hispanic_p = sum(hispanic_p),
              total_white_p = sum(white_p),
              actual_total_p = sum(total_asian_p, total_black_p, total_hispanic_p, total_white_p), # 7.9 M
              total_p = sum(total_p)) 
  
  city_precinct_returns <- mget(
    ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                       "_city_precinct_returns$")))[[1]]
  
  citywide_partisanship <- city_precinct_returns %>% 
    as_tibble() %>%
    summarise(total_g20predbid = sum(g20predbid), 
              total_g20prertru = sum(g20prertru),
              total_g20_2candidates = sum(total_g20predbid, total_g20prertru)) 
  
  block_aggregates <- intersection_weighted_block %>%
    as_tibble() %>%
    summarise(total_asian_p = sum(sw_asian_p),
              total_black_p = sum(sw_black_p),
              total_hispanic_p = sum(sw_hispanic_p),
              total_white_p = sum(sw_white_p),
              total_p = sum(sw_total_p),
              total_g20predbid = sum(sw_g20predbid), 
              total_g20prertru = sum(sw_g20prertru),
              total_g20_2candidates = sum(total_g20predbid, total_g20prertru)) 
  
  council_district_aggregates <- intersection_weighted_council_district %>%
    as_tibble() %>%
    summarise(total_asian_p = sum(sw_asian_p),
              total_black_p = sum(sw_black_p),
              total_hispanic_p = sum(sw_hispanic_p),
              total_white_p = sum(sw_white_p),
              total_p = sum(sw_total_p),
              total_g20predbid = sum(sw_g20predbid), 
              total_g20prertru = sum(sw_g20prertru),
              total_g20_2candidates = sum(total_g20predbid, total_g20prertru)) 
  
  if (citywide_demographic$total_p < block_aggregates$total_p | 
      citywide_partisanship$total_g20predbid < block_aggregates$total_g20predbid){
    print(cities[i])
  } else {}
}