library(redist)
library(sf)
library(geomander)
library(tigris)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
options(tigris_use_cache = TRUE)

# Cities sorted by population
cities <- c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", # 1-5
            "Philadelphia", "San Antonio", "San Diego", "Dallas", "San Jose", # 6-10 
            "Austin", "Jacksonville", "Fort Worth") # 11-13

# Cities' states
cities_states <- c("NY", "CA", "IL", "TX", "AZ", # 1-5
                   "PA", "TX", "CA", "TX", "CA", # 6-10
                   "TX", "FL", "TX", "OH", "IN", # 11-15
                   "NC", "CA", "WA", "CO", "DC", # 16-20
                   "TN", "OK", "TX", "MA", "OR", # 20-25
                   "NV")

# Cities' counties
# Four cities have multiple counties: New York (5); Houston (3); Dallas (5); Austion (3)
cities_counties <- list(new_york = c("New York", "Kings", "Bronx", "Richmond", "Queens"),
                        los_angeles = c("Los Angeles"),
                        chicago = c("Cook"),
                        houston = c("Harris", "Fort Bend", "Montgomery"), # 201 - Harris (as opposed to Harrison)
                        phoenix = c("Maricopa"), #5
                        philadelphia = c("Philadelphia"),
                        san_antonio = c("Bexar"),
                        san_diego = c("San Diego"),
                        dallas = c("Dallas", "Collin", "Denton", "Kaufman", "Rockwall"), # 085 - Collin (as opposed to Collingsworth)
                        san_jose = c("Santa Clara"), #10
                        austin = c("Travis", "Hays", "Williamson"),
                        jacksonville = c("Duval"),
                        fort_worth = c("Tarrant"),
                        columbus = c("Delaware", "Fairfield", "Franklin"),
                        indianapolis = c("Marion"), #15
                        charlotte = c("Mecklenburg"),
                        san_francisco = c("San Francisco"),
                        seattle = c("King"),
                        denver = c("Denver"),
                        washington = c(), #20
                        nashville = c("Davidson"),
                        oklahoma_city = c("Oklahoma", "Canadian", "Cleveland", "Pottawatomie"),
                        el_paso = c("El Paso"),
                        boston = c("Suffolk"),
                        portland = c("Multnomah", "Washington", "Clackamas"), #25
                        las_vegas = c("Clark"))

# Create a numeric version of county and state list
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

# i - city index
# j - county index
for (i in 11:13) {
  for (j in 1:length(cities_counties_codes[[i]])){
    print(paste0(Sys.time(), ": ", cities[i], " - Loop starts"))
    
    # Load block-level demographic data 
    city_block <- create_block_table(state = cities_states_codes[i], 
                                     county = cities_counties_codes[[i]][j],
                                     year = 2010)
    
    print(paste0(Sys.time(), ": ", cities[i], " - Block data download complete"))
    
    # Load precinct-level partisanship data
    city_prct <- sf::read_sf(
      paste0('CleanedData/CityElectionReturns/', 
             str_replace_all(tolower(cities[i]), " ", "_"), '_city_election_returns.shp'))
    
    print(paste0(Sys.time(), ": ", cities[i], " - Precinct data download complete"))
    
    # Load city council district geometry
    city_cd <- sf::read_sf(
      paste0('CleanedData/CityCouncilDistrict/',
             str_replace_all(tolower(cities[i]), " ", "_"), '_city_council_district.shp'))
    
    print(paste0(Sys.time(), ": ", cities[i], " - Council district data download complete"))
    
    # Make sure the three data use the same CRS
    # 'city_block' is the benchmark b/c it uses the default CRS of the package
    city_prct <- st_transform(city_prct, st_crs(city_block))
    city_cd <- st_transform(city_cd, st_crs(city_block))
    
    #then clip blocks to match
    city_block <- city_block %>% 
      geo_filter(to = city_cd)
    
    print(paste0(Sys.time(), ": ", cities[i], " - Filtering complete"))
    
    city_block$trim <- city_block %>% 
      geo_trim(to = city_cd, bool = TRUE)
    
    city_block <- city_block %>%
      filter(trim)
    
    print(paste0(Sys.time(), ": ", cities[i], " - Trimming complete"))
    # Match voting blocks to the precincts
    # Put in the samller unit in from, bigger unit in to
    matchprct <- geo_match(from = city_block, to = city_prct)
    
    print(paste0(Sys.time(), ": ", cities[i], " - Geomatching complete"))
    # Create spatially-weighted estimates
    city_block$biden <-estimate_down(wts = city_block$vap, value = city_prct$g20predbid, group=matchprct)
    city_block$trump<-estimate_down(wts = city_block$vap, value = city_prct$g20prertru, group=matchprct)
    
    print(paste0(Sys.time(), ": ", cities[i], " - Spatial weighting complete"))
    
    assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_block_county_", j), city_block)
    
  }
  
  city_block <- bind_rows(mget(ls(pattern = paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_block_county_[1-5]"))))
  
  # Save the new object in the environment
  assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_block"), city_block)
  
  # Export the new object as shapefile
  st_write(
    city_block,
    paste0(
      wd, "CleanedData/geomander/",
      str_replace_all(tolower(cities[i]), " ", "_"),
      "_block"),
    driver = "ESRI Shapefile", append = FALSE)
  
}

for (i in 1:length(cities)) {
  # The resultant data from the previous for loop
  city_block <- mget(ls(pattern = paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_block")))[[1]]
  
  # Get the name of council district id column
  council_district_id <- cities_precinct_council_district_id_columns[[i]][2]
  
  city_cd_count <- sf::read_sf(
    paste0('CleanedData/CityCouncilDistrict/',
           str_replace_all(tolower(cities[i]), " ", "_"), '_city_council_district.shp')) %>%
    pull(all_of(council_district_id)) %>% unique() %>% length()

  adj <- redist.adjacency(shp = city_block)
  
  map <- redist_map(city_block, pop_tol = 0.05, ndists = city_cd_count, adj = adj)
  
  plans <- redist_smc(map, nsims = 5, compactness = 1) 
  
  # Summarize the plans created
  plans <- plans %>%
    mutate(pop_dev = abs(total_pop / get_target(map) - 1),
           comp = distr_compactness(map, "PolsbyPopper"),
           pct_min = group_frac(map, vap - vap_white, vap),
           pct_dem = group_frac(map, biden, biden + trump))
  
  # Save the new object in the environment
  # Export the new object as shapefile
}

plan_sum <- group_by(plans, draw) %>%
  summarize(max_dev = max(pop_dev),
            avg_comp = mean(comp),
            max_pct_min = max(pct_min),
            dem_distr = sum(pct_dem > 0.5))
