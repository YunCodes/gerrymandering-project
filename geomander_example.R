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

for (i in 1) {
  print(paste0(Sys.time(), ": ", cities[i], " - Loop starts"))
  
  # Load block-level demographic data 
  city_block <- create_block_table(state = cities_states_codes[i], 
                                   county = cities_counties_codes[[i]],
                                   year = 2010)
  #city_block <- sf::read_sf(
  #paste0('CleanedData/CityDemographic/Block/', 
  #str_replace_all(tolower(cities[i]), " ", "_"), '_city_demographic_block.shp'))
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
  Sys.time()
  # Make sure the three data use the same CRS
  # 'city_block' is the benchmark b/c it uses the default CRS of the package
  city_prct <- st_transform(city_prct, st_crs(city_block))
  city_cd <- st_transform(city_cd, st_crs(city_block))
  
  #then clip blocks to match
  city_block <- city_block %>% 
    geo_filter(to = city_cd)
  
  print(paste0(Sys.time(), ": ", cities[i], " - Filtering complete"))
  Sys.time()
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

#Now let's test that it works w/ the algorithm 
adj <- redist.adjacency(shp = block)

map <- redist_map(block, pop_tol = 0.05, ndists = 50, adj = adj)

plans <- redist_smc(map, nsims = 5, compactness = 1) #just 5 sims for speed

redist.plot.plans(plans, draws=c(1,20,40,50), geom=map)
plot(plans, trump, sort=FALSE, size=0.5)
#Runs perfectly. 

#Let's summarize the plans created  ## FINAL DATA
plans <- plans %>%
  mutate(pop_dev = abs(total_pop / get_target(map) - 1),
         comp = distr_compactness(map, "PolsbyPopper"),
         pct_min = group_frac(map, vap - vap_white, vap),
         pct_dem = group_frac(map, biden, biden + trump))


plan_sum = group_by(plans, draw) %>%
  summarize(max_dev = max(pop_dev),
            avg_comp = mean(comp),
            max_pct_min = max(pct_min),
            dem_distr = sum(pct_dem > 0.5))

print(plan_sum)

pal = scales::viridis_pal()(5)[-1]
redist.plot.scatter(plans, pct_min, pct_dem, 
                    color=pal[subset_sampled(plans)$district]) +
  scale_color_manual(values="black")
