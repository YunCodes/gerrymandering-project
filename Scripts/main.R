library(tidyverse)
library(tidycensus)
library(janitor)
library(raster)
library(rgdal)
library(sf)
library(nngeo)

# Collect cities' boundaries
city_boundary_filepath <- "/Volumes/Yun Choi/columbia_university/gerrymandering-project/RawData/CityCouncilFiles/District/"

# Cities sorted by population
cities <- c("New York", "Los Angeles", "Chicago", #"Houston", 
            "Phoenix", # 1-5
            "Philadelphia", "San Antonio", "San Diego", "Dallas", "San Jose", # 6-10 
            #"Austin", 
            "Jacksonville", "Fort Worth") # 11-13

# Cities' counties
# Four cities have multiple counties: New York (5); Houston (3); Dallas (5); Austion (3)
cities_counties <- list(new_york = c("New York", "Kings", "Bronx", "Richmond", "Queens"),
                        los_angeles = c("Los Angeles"),
                        chicago = c("Cook"),
                        # houston = c("Harris", "Fort Bend", "Montgomery"),
                        phoenix = c("Maricopa"),
                        philadelphia = c("Philadelphia"),
                        san_antonio = c("Bexar"),
                        san_diego = c("San Diego"),
                        dallas = c("Dallas", "Collin", "Denton", "Kaufman", "Rockwall"),
                        san_jose = c("Santa Clara"),
                        # austin = c("Travis", "Hays", "Williamson"),
                        jacksonville = c("Duval"),
                        fort_worth = c("Tarrant"))

cities_states <- c("New York", "California", "Illinois", # "Texas", 
                   "Arizona", # 1-5
                   "Pennsylvania", "Texas", "California", "Texas", "California", # 6-10
                   # "Texas", 
                   "Florida", "Texas") # 11-13

# Create a for loop that (1) reads individual cities' council districts shapefiles; (2) dissolves all districts; and (3) assign th
for (c in cities) {
  # Read city districts shape file
  city_council_districts <- st_read(
    paste0(city_boundary_filepath, c, "/", c, ".shp")
    ) %>%
    clean_names()
  
  # Dissolve polygons
  city_boundary <- st_union(city_council_districts$geometry) %>%
    # Remove unnecessary holes in the dissolved city boundary
    nngeo::st_remove_holes()
  
  # Change object class from 'sfc' to 
  # city_boundary <- as(st_geometry(city_boundary), "Spatial") 
  
  # Assign the city boundary as a new object
  # i.e.) "new_york_city_boundary", "chicago_city_boundary"
  assign(paste0(str_replace_all(tolower(c), " ", "_"), "_city_boundary"), 
         city_boundary)
}

# Load 2010 decennial variables table
census_decennial_variables <- load_variables(2010, "sf1", cache = TRUE)

# Prepare tract and block-level demographics data with geometry
for (i in 1:length(cities)) {
  
  # tract-level
  tracts <- get_decennial(state = cities_states[i],
                          county = cities_counties[[i]],
                          geography = "tract",
                          year = 2010,
                          variables = c("black_p" = "P003003",
                                        "white_p" = "P003002",
                                        "hispanic_p" = "P005010",
                                        "asian_p" = "P003005",
                                        "total_p" = "P003001"),
                          geometry = TRUE)
  
  # block-level
  blocks <- get_decennial(state = cities_states[i],
                          county = cities_counties[[i]],
                          geography = "block",
                          year = 2010,
                          variables = c("black_p" = "P003003",
                                        "white_p" = "P003002",
                                        "hispanic_p" = "P005010",
                                        "asian_p" = "P003005",
                                        "total_p" = "P003001"),
                          geometry = TRUE)

  # Retrieve city boundary
  city_boundary <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"), "_city_boundary$")))[[1]]
  
  # Filter tracts and blocks within each city boundary only
  city_tracts_intersections <- st_intersection(city_boundary, tracts)
  city_blocks_intersections <- st_intersection(city_boundary, blocks)
  
  # Assign the tracts and blocks boundaries as new objects
  # i.e.) "new_york_tracts_boundary", "chicago_tracts_boundary"
  assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_tracts_boundary"), 
         city_tracts_intersections)
  # i.e.) "new_york_blocks_boundary", "chicago_blocks_boundary"
  assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_blocks_boundary"), 
         city_blocks_intersections)
}

for (c in cities) {
  st_crs()
} 



# Example 1 - nngeo::st_remove_holes
new_york <- st_read("/Volumes/Yun Choi/columbia_university/gerrymandering-project/RawData/CityCouncilFiles/District/New York/New York.shp") %>%
  clean_names()

new_york_boundary <- st_union(new_york$geometry) %>%
  st_remove_holes()

# Example 2 - Houston and Austin geometry issue 
houston <- st_read("/Volumes/Yun Choi/columbia_university/gerrymandering-project/RawData/CityCouncilFiles/District/Houston/Houston.shp") %>%
  clean_names()

houston_boundary <- st_union(houston$geometry) %>%
  st_remove_holes()

# Example 3 - Boundary files having different datum and projections
st_crs(cuny_subway_stations_geo) == st_crs(cuny_subway_lines_geo)
crs(new_york_city_boundary)
crs(san_jose_city_boundary)
crs(cuny_subway_lines_geo)