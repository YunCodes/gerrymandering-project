# This script is to prepare data to use in the second script 
# where we run simulations over the most populous 40 cities in the U.S.

# PART 0: Setup
# PART 1: Collect, Manipulate, and Export Cities' Boundaries
# PART 2: Collect, Manipulate, and Export Block-level Census Demographics Data
# PART 3: Collect, Manipulate, and Export Precinct-level Elections Retuns Data


###------------------------PART 0: Setup

# Import packages
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
library(data.table)
options(tigris_use_cache = TRUE)

# Set Census API key
census_api_key("e2a0be8ff20ee18fd0d7eeac46bf8eda7de4a2d6", install = TRUE, overwrite = TRUE)

# Collect cities' boundaries
wd <- "/Volumes/Yun Choi/columbia_university/gerrymandering-project/" # wd = local working directory
gd <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1gwDBDhcuHI7NApYU4JjUs8FjkHaiAINo/Cities - Local Redistricting/" # gd = Google directory
city_boundary_filepath_2010 <- paste0(gd, "City Council Files/District/")

# Ensure s2 is not being used although s2 provides more accurate mapping as using s2 throws errors for some cities
# Documentation on s2 can be found here: https://r-spatial.github.io/sf/articles/sf7.html
sf_use_s2(FALSE)

# Cities sorted by population
cities <- c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", # 1-5
            "Philadelphia", "San Antonio", "San Diego", "Dallas", "San Jose", # 6-10 
            "Austin", "Jacksonville", "Fort Worth", "Columbus", "Indianapolis", # 11-15
            "Charlotte", "San Francisco", "Seattle", "Denver", "Oklahoma City", # 16-20
            "Nashville", "El Paso", "Washington", "Boston", "Las Vegas", # 21-25
            #"Portland", # 26 does not have city council districts
            "Detroit", "Louisville", "Memphis", "Baltimore", # 27-30
            "Milwaukee", "Albuquerque", "Fresno", "Tucson", "Sacramento", # 31-35
            "Mesa", "Kansas City", "Atlanta", "Omaha", "Colorado Springs" # 36-40
)

# Cities' counties
# The order of the cities in this list should match the order of city list above
cities_counties <- list(new_york = c("New York", "Kings", "Bronx", "Richmond", "Queens"),
                        los_angeles = c("Los Angeles"),
                        chicago = c("Cook"),
                        houston = c("Harris", "Fort Bend", "Montgomery"),
                        phoenix = c("Maricopa"), # 5
                        philadelphia = c("Philadelphia"),
                        san_antonio = c("Bexar"),
                        san_diego = c("San Diego"),
                        dallas = c("Dallas", "Collin", "Denton", "Kaufman", "Rockwall"),
                        san_jose = c("Santa Clara"), # 10
                        austin = c("Travis", "Hays", "Williamson"),
                        jacksonville = c("Duval"),
                        fort_worth = c("Tarrant"),
                        columbus = c("Delaware", "Fairfield", "Franklin"),
                        indianapolis = c("Marion"), # 15
                        charlotte = c("Mecklenburg"),
                        san_francisco = c("San Francisco"),
                        seattle = c("King"),
                        denver = c("Denver"),
                        oklahoma_city = c("Oklahoma", "Canadian", "Cleveland", "Pottawatomie"), # 20 
                        nashville = c("Davidson"),
                        el_paso = c("El Paso"),
                        washington = c("District of Columbia"), 
                        boston = c("Suffolk"),
                        las_vegas = c("Clark"), # 25
                        # portland = c("Multnomah", "Washington", "Clackamas"), 
                        detroit = c("Wayne"),
                        louisville = c("Jefferson"),
                        memphis = c("Shelby"),
                        baltimore = c("Baltimore City"), # city of baltimore does not belong to any county 
                        milwaukee = c("Milwaukee", "Washington", "Waukesha"), # 30
                        albuquerque = c("Bernalillo"),
                        fresno = c("Fresno"), 
                        tucson = c("Pima"), 
                        sacramento = c("Sacramento"), 
                        mesa = c("Maricopa"), # 35
                        kansas_city = c("Jackson", "Clay", "Platte", "Cass"),
                        atlanta = c("Fulton", "DeKalb"),
                        omaha = c("Douglas"),
                        colorado_springs = c("El Paso") # 39
                        )

# Cities' states
cities_states <- c("NY", "CA", "IL", "TX", "AZ", # 1-5
                   "PA", "TX", "CA", "TX", "CA", # 6-10
                   "TX", "FL", "TX", "OH", "IN", # 11-15
                   "NC", "CA", "WA", "CO", "OK", # 16-20
                   "TN", "TX", "DC", "MA", "NV", # 21-25
                   # "OR", # 26
                   "MI", "KY", "TN", "MD", # 27-30
                   "WI", "NM", "CA", "AZ", "CA", # 31-35
                   "AZ", "MO", "GA", "NE", "CO" # 36-40
                   )

# Create a numeric version of county and state list
# This version is used to import Census demographics data with higher accuracy 
cities_states_codes <- c()
cities_counties_codes <- list()

for (i in 1:length(cities)) {
  
  state <- cities_states[i]
  
  cities_counties_codes[[i]] <- fips(state, 
                                     county = cities_counties[[i]]) %>%
    substr(3, 5)
  
  cities_states_codes[i] <- unique(fips_codes$state_code[fips_codes$state == state])
}

names(cities_counties_codes) <- str_replace_all(tolower(cities), " ", "_")



###------------------------PART 1: Collect and Manipulate Cities' Boundaries

# Create a for loop that (1) reads cities' council districts .shp files; (2) dissolves all districts; and (3) assign the city boundary as a new object
# For Columbus and Washington, council districts files were named differently 
# So the file-reading command has been adjusted to incorporate those idiosyncrasies

for (y in c(2010)) {
  for (c in cities) {
    
    if (c == "Columbus"){
      city_council_districts <- st_read(
        paste0(city_boundary_filepath_2010, "Columbus, Oh", "/", "Columbus, Oh", ".shp")
      ) %>%
        clean_names() %>%
        st_make_valid()
      
    } else if (c == "Washington") {
      city_council_districts <- st_read(
        paste0(city_boundary_filepath_2010, "Washington, Dc", "/", "Dc", ".shp")
      ) %>%
        clean_names() %>%
        st_make_valid()
      
    } else { # for the remaining other cities
      # Read city districts shape file
      city_council_districts <- st_read(
        paste0(city_boundary_filepath_2010, c, "/", c, ".shp")
      ) %>%
        clean_names() %>%
        st_make_valid()
    }
    
    # Convert CRS to NAD83
    # NAD83 is chosen in accordance with Census spatial data projection
    city_council_districts_NAD83 <- st_transform(city_council_districts,
                                                 "+proj=longlat +datum=NAD83")
    
    # Dissolve polygons
    city_boundary <- st_union(city_council_districts$geometry) %>%
      # Remove unnecessary holes in the dissolved city boundary
      nngeo::st_remove_holes() %>%
      # Change object class from 'sfc' to 'sf'
      st_sf() %>% st_cast()
    
    # Convert CRS to NAD83
    city_boundary_NAD83 <- st_transform(city_boundary,
                                        "+proj=longlat +datum=NAD83")
    
    # Assign the city boundary as a new object
    # i.e.) "new_york_city_boundary", "chicago_city_boundary"
    
    # Save the re-projected city council districts as .shp file
    st_write(city_council_districts_NAD83, 
             paste0(wd, "CleanedData/CityCouncilDistrict/", 
                    str_replace_all(tolower(c), " ", "_"), "_city_council_district_", y, ".shp"),
             append = FALSE)
    
    # Also save the outer city boundary in the local Rstudio environment for later use
    assign(paste0(str_replace_all(tolower(c), " ", "_"), "_city_boundary_", y), 
           city_boundary_NAD83)
  }
}

# Check if every city bounrary file has the correct CRS
# Print cities that has a CRS different from "+proj=longlat +datum=NAD83"
for (i in 1:length(cities)) {
  city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), "_city_boundary$")))[[1]]
  if (st_crs(city_boundary) == st_crs(new_york_city_boundary)) {
  } else {
    print(cities[i])
  }
}



###------------------------PART 2: Collect, Manipulate, and Export Block-level Census Demographics Data


# Load 2010 decennial variables table
census_decennial_variables_2010 <- load_variables(2010, "sf1", cache = TRUE)

# Load 2020 decennial variables table
census_decennial_variables_2020 <- load_variables(2020, "pl", cache = TRUE)

# Prepare block-level Census demographics data with geometry 2010
for (i in 1:length(cities)) {
  for (l in c("block")) {
    for (y in c(2010, 2020)) {
      if (y == 2010) { # 2010 data
        
        data <- get_decennial(state = cities_states_codes[i],
                              county = cities_counties_codes[[i]],
                              geography = l,
                              year = 2010,
                              variables = c("white_p" = "P005003", #"P003002",
                                            "black_p" = "P005004", #"P003003",
                                            "hispanic_p" = "P005010",
                                            "asian_p" = "P005006", #"P003005",
                                            "total_p" = "P005001" #"P003001")
                              ), 
                              geometry = TRUE,
                              output = "wide")
        
      } else { # 2020 data
        data <- get_decennial(state = cities_states_codes[i],
                              county = cities_counties_codes[[i]],
                              geography = l,
                              year = 2020,
                              variables = c("white_p" = "P2_005N", #"P003002",
                                            "black_p" = "P2_006N", #"P003003",
                                            "hispanic_p" = "P2_002N",
                                            "asian_p" = "P2_008N", #"P003005",
                                            "total_p" = "P2_001N" #"P003001")
                              ), 
                              geometry = TRUE,
                              output = "wide")
      }
      
      # Assign data as a new object
      # i.e.) "new_york_county_block"
      assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_county_", l, "_", y), 
             data)
    }
  }
}


# Save county_block datasets as RData
save(list = ls(pattern = "_county_block_2010$"),
     file = paste0(wd, "CleanedData/02_cities_county_block_2010.RData"))
save(list = ls(pattern = "_county_block_2020$"),
     file = paste0(wd, "CleanedData/02_cities_county_block_2020.RData"))

# Load county_block datasets
load(paste0(wd, "CleanedData/02_cities_county_block_2010.RData"))
load(paste0(wd, "CleanedData/02_cities_county_block_2020.RData"))

# This loop is to filter out blocks in each city's counties that do not exist within the city boundary
# This is for the practical reason to shorten the time QGIS takes to intersect different layers
for (i in 1:length(cities)) {
  for (l in c("block")) {
    for (y in c(2010))
      if (y == 2010) {
        # Retrieve city boundary
        city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                                  "_city_boundary_", y, "$")))[[1]]
        # Retrieve block-level city county data
        data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                         "_county_", l, "_", y)))[[1]]
        
        # Filter blocks within each city boundary only
        index <- apply(st_intersects(data, city_boundary, sparse = FALSE), 1, any)
        filtered <- data[index, ]
        
        # Assign data as a new object
        # i.e.) "chicago_city_block_2010"
        assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_city_", l, "_", y), 
               filtered)
      } else {}
  }
}

# Save county_block datasets into RData
save(list = ls(pattern = "_city_block_2010$"),
     file = paste0(wd, "CleanedData/04_cities_city_block_2010.RData"))

# Load city_block datasets
load(paste0(wd, "CleanedData/04_cities_city_block_2010.RData"))

# Export block-level city demographic data as shp files 
for (i in 12) {
  for (l in c("block")) {
    for (y in c(2010)){
      demographic_data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                                   "_city_", l, "_", y, "$")))[[1]] 
      
      st_write(demographic_data, 
               paste0(wd, "CleanedData/CityDemographic/", str_to_title(l), "/",
                      str_replace_all(tolower(cities[i]), " ", "_"), "_city_demographic_", l, "_", y, ".shp"),
               # Overwrite if the data already exists
               append=FALSE)
    }
  }
}



###------------------------PART 3: Collect, Manipulate, and Export Precinct-level Elections Returns Data

# Set file paths
precinct_returns_filepath_2016 <- paste0(gd, "Election Precinct Level Returns/2016/")
precinct_returns_filepath_2020 <- paste0(gd, "Election Precinct Level Returns/2020/")

for (i in 1:length(unique(cities_states))) {
  for (y in c(2016, 2020)) {
    
    state <- unique(cities_states)[i]
    
    if (y == 2016) {
      
      # Read state precinct returns shape file
      state_precinct_returns <- st_read(
        paste0(precinct_returns_filepath_2016, 
               tolower(state), "_2016/", 
               tolower(state), "_2016.shp")
      ) %>%
        clean_names() %>%
        st_make_valid()
      
    } else {
      
      # Read state precinct returns shape file
      state_precinct_returns <- st_read(
        paste0(precinct_returns_filepath_2020, 
               tolower(state), "_vest_20/", 
               tolower(state), "_2020.shp")
      ) %>%
        clean_names() %>%
        st_zm() %>% # Drop M from geometries
        st_make_valid()
    }
      # Convert CRS to NAD83
      state_precinct_returns_NAD83 <- st_transform(state_precinct_returns,
                                                   "+proj=longlat +datum=NAD83")
      
      # Assign the city precinct_returns as a new object
      assign(paste0(str_replace_all(tolower(state), " ", "_"), "_state_precinct_returns_", y), 
             state_precinct_returns_NAD83)
  }
}

# Save county_precinct_returns data sets into RData
save(list = ls(pattern = "_state_precinct_returns_2016"),
     file = paste0(wd, "CleanedData/05_cities_state_precinct_returns_2016.RData"))
save(list = ls(pattern = "_state_precinct_returns_2020"),
     file = paste0(wd, "CleanedData/05_cities_state_precinct_returns_2020.RData"))

# Load 2016 and 2020 city_block data sets
load(paste0(wd, "CleanedData/05_cities_state_precinct_returns_2016.RData"))
load(paste0(wd, "CleanedData/05_cities_state_precinct_returns_2020.RData"))

# This code identifies state_precinct_returns with invalid geometry
for (i in 1:length(cities)) {
  for (y in c(2016, 2020)) {
    
    state <- cities_states[i]
    
    if (y == 2016){
      data <- mget(ls(pattern = paste0("^", tolower(state), "_state_precinct_returns_2016")))[[1]] 
    } else {
      data <- mget(ls(pattern = paste0("^", tolower(state), "_state_precinct_returns_2020")))[[1]] 
    }
    
    if (st_is_valid(data) == TRUE) {
    } else {
      print(cities[i])
    }
  }
}

# Filter precincts within city boundary only
for (i in 1:length(cities)) {
  for (y in c(2010, 2020)) {
    if (y == 2010) {
      
      state <- cities_states[i]
      # Retrieve city boundary
      city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), 
                                                "_city_boundary_2010$")))[[1]]
      # Retrieve state_precinct_returns data
      data <- mget(ls(pattern = paste0("^", tolower(state), "_state_precinct_returns_2016")))[[1]] %>%
        st_make_valid()
  
    } else {}
    
    # Filter precincts within each city boundary only
    index <- apply(st_intersects(data, city_boundary, sparse = FALSE), 1, any)
    filtered <- data[index,]
    
    # Assign data as a new object
    assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_city_precinct_returns_", y), 
           filtered)
  }
}

# Save city_precinct_returns datasets into RData
save(list = ls(pattern = "_city_precinct_returns_2010$"),
     file = paste0(wd, "CleanedData/06_cities_city_precinct_returns_2010.RData"))
save(list = ls(pattern = "_city_precinct_returns_2020$"),
     file = paste0(wd, "CleanedData/06_cities_city_precinct_returns_2020.RData"))

load(paste0(wd, "CleanedData/06_cities_city_precinct_returns_2010.RData"))
load(paste0(wd, "CleanedData/06_cities_city_precinct_returns_2020.RData"))

# Standardize city_precinct_returns data
# Data structure and column names vary by state
precinct_returns_colnames_2010 <- list()
precinct_returns_colnames_2020 <- list()

for (i in 1:length(cities)) {
  for (y in c(2010, 2020)) {
    city_precinct_returns <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                                      "_city_precinct_returns_", y, "$")))[[1]]
    
    if (y == 2010) {
      precinct_returns_colnames_2010[[i]] <- colnames(city_precinct_returns)
    } else {
      precinct_returns_colnames_2020[[i]] <- colnames(city_precinct_returns)
    }
  }
}

names(precinct_returns_colnames_2010) <- str_replace_all(tolower(cities[i]), " ", "_")
names(precinct_returns_colnames_2020) <- str_replace_all(tolower(cities[i]), " ", "_")

colnames(los_angeles_city_precinct_returns_2010) <- str_remove_all(colnames(los_angeles_city_precinct_returns_2010), "_")
colnames(san_diego_city_precinct_returns_2010) <- str_remove_all(colnames(san_diego_city_precinct_returns_2010), "_")
colnames(san_jose_city_precinct_returns_2010) <- str_remove_all(colnames(san_jose_city_precinct_returns_2010), "_")
colnames(san_francisco_city_precinct_returns_2010) <- str_remove_all(colnames(san_francisco_city_precinct_returns_2010), "_")
colnames(detroit_city_precinct_returns_2010) <- str_remove_all(colnames(detroit_city_precinct_returns_2010), "_")
colnames(fresno_city_precinct_returns_2010) <- str_remove_all(colnames(fresno_city_precinct_returns_2010), "_")
colnames(sacramento_city_precinct_returns_2010) <- str_remove_all(colnames(sacramento_city_precinct_returns_2010), "_")

# Create list with columns of interest
demographic_columns <- c("GEOID", "NAME", "geometry", "asian_p", "black_p", "hispanic_p", "total_p", "white_p")
demographic_columns_abrv <- c("GEOID", "NAME", "geometry", "asian_p", "black_p", "hspnc_p", "total_p", "white_p")
partisan_columns_2016 <- c("g16predcli", "g16prertru")
partisan_columns_abrv_2016 <- c("sw_g20prd", "sw_g20prr") # Needs to be fixed
partisan_columns_2020 <- c("g20predbid", "g20prertru")
partisan_columns_abrv_2020 <- c("sw_g20prd", "sw_g20prr")

# This is to get the council district column name for each city
# Second element in the list below
city_cd <- sf::read_sf(
  paste0('CleanedData/CityCouncilDistrict/',
         str_replace_all(tolower(cities[i]), " ", "_"), '_city_council_district.shp'))

glimpse(city_cd)

# first element is the id for precinct; second is for council district 
# first is from 2016 election returns; Second is from 2010 city council geometries
cities_precinct_council_district_id_columns_2010 <- list(new_york = c("precinct", "coun_dist"), # NY
                                                         los_angeles = c("srpreckey", "district"), # CA
                                                         chicago = c("name", "ward"), # IL
                                                         houston = c("pctkey", "district"),# TX
                                                         phoenix = c("pctnum", "district"), # AZ # 5
                                                         philadelphia = c("name", "district"), # PA
                                                         san_antonio = c("pctkey", "district"), 
                                                         san_diego = c("srpreckey", "district"), # CA
                                                         dallas = c("pctkey", "district"), # TX
                                                         san_jose = c("srpreckey", "district"), # 10
                                                         austin = c("pctkey", "council_di"), 
                                                         jacksonville = c("pct", "district"),
                                                         fort_worth = c("pctkey", "distrct"), # has duplicate district 7
                                                         columbus = c("precinct16", "odt_dst"), # OH -> has only one district
                                                         indianapolis = c("p16", "council"), # IN # 15
                                                         charlotte = c("prec_id", "distrct"), # NC
                                                         san_francisco = c("srpreckey", "supdistpad"), # CA
                                                         seattle = c("preccode", "c_district"), # WA
                                                         denver = c("precinct", "dist_num"), 
                                                         oklahoma_city = c("pct_ceb", "council_wa"), # 20
                                                         nashville = c("name_2", "dstrct_d"),
                                                         el_paso = c("pctkey", "district"),
                                                         washington = c("name", "ward"),
                                                         boston = c("wp_name", "distrct"),
                                                         las_vegas = c("name", "clv_wardsw"), #25
                                                         detroit = c("vtd2016", "dstrct_"),
                                                         louisville = c("name", "coundist"),
                                                         memphis = c("name", "distrct"),
                                                         baltimore = c("preid", "area_name"),
                                                         milwaukee = c("objectid", "district"), #30
                                                         albuquerque = c("name_2", "districtnu"),
                                                         fresno = c("srpreckey", "council_di"),
                                                         tucson = c("pctnum", "ward"),
                                                         sacramento = c("srpreckey", "distnum"), 
                                                         mesa = c("pctnum", "district"), #35
                                                         kansas_city = c("name", "label"),
                                                         atlanta = c("precinct_n", "name"),
                                                         omaha = c("namelsad", "distrct"),
                                                         colorado_springs = c("precinct", "district")
)

sf::read_sf(
  paste0('CleanedData/CityCouncilDistrict/',
         str_replace_all(tolower(cities[36]), " ", "_"), '_city_council_district_', y, '.shp')) %>% glimpse()

# The first element is from 2020 election returns; Second is from 2010 city council geometries
cities_precinct_council_district_id_columns_2020 <- list(new_york = c("precinct", "coun_dist"), # NY
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
                                                         fort_worth = c("", "distrct"), # has duplicate district 7
                                                         columbus = c("", "odt_dst"), # OH -> has only one district
                                                         indianapolis = c("name20", "council"), # IN
                                                         charlotte = c("prec_id", "distrct"), # NC
                                                         san_francisco = c("srprec", "supdistpad"), # CA
                                                         seattle = c("preccode", "c_district"), # WA
                                                         denver = c("precinct", "dist_num"))


# Export precinct-level city election returns data as shp files
for (i in 1:length(cities)) {
  for (y in c(2010, 2020)) {
    partisanship_data <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                                  "_city_precinct_returns_", y, "$")))[[1]]
    
    if (i == 12) { # Jacksonville
      
      partisanship_data <- st_collection_extract(partisanship_data, "POLYGON")
        
    } else {}
    st_write(partisanship_data, 
             paste0(wd, "CleanedData/CityElectionReturns/", 
                    str_replace_all(tolower(cities[i]), " ", "_"), "_city_election_returns_", y, ".shp"),
             append=FALSE)
  }
}
