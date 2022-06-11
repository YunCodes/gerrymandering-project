
# PART 0: Setup
# PART 1: First approach
# PART 2: Import QGIS-created Intersection-level Data and Compute Spatially-Weighted Estimates for Different Spatial Units
# PART 3: Second approach for cities that didn't work with the first approach


# PART 0: Setup

library(redist)
library(sf)
library(geomander)
library(tigris)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(raster)
library(rgdal)
library(sp)
options(tigris_use_cache = TRUE)

# PART 1: First approach

# y - year index
# i - city index
# j - county index

# 12 Jacksonville: Cannot open "CleanedData/CityElectionReturns/jacksonville_city_election_returns_2010.shp"; The file doesn't seem to exist.
# 26 Detroit: Your geometry data download failed. 

for (y in c(2010)) {
  for (i in 1:length(cities)) {
    for (j in 1:length(cities_counties_codes[[i]])){
      print(paste0(Sys.time(), ": ", cities[i], " - Loop starts"))
      
      # Load block-level demographic data 
      city_block <- create_block_table(state = cities_states_codes[i], 
                                       county = cities_counties_codes[[i]][j],
                                       year = y)
      
      print(paste0(Sys.time(), ": ", cities[i], " - Block data download complete"))
      
      # Load precinct-level partisanship data
      city_prct <- sf::read_sf(
        paste0('CleanedData/CityElectionReturns/', 
               str_replace_all(tolower(cities[i]), " ", "_"), '_city_election_returns_', y, '.shp'))
      
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
      
      if (y == 2010) {
        # Create spatially-weighted estimates
        city_block$dem <-estimate_down(wts = city_block$vap, value = city_prct$g16predcli, group=matchprct)
        city_block$rep <-estimate_down(wts = city_block$vap, value = city_prct$g16prertru, group=matchprct)
      } else if (y == 2020) {
        # Create spatially-weighted estimates
        city_block$dem <-estimate_down(wts = city_block$vap, value = city_prct$g20predbid, group=matchprct)
        city_block$rep <-estimate_down(wts = city_block$vap, value = city_prct$g20prertru, group=matchprct)
      } else {}
      
      print(paste0(Sys.time(), ": ", cities[i], " - Spatial weighting complete"))
      
      assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_block_county_", j), city_block)
      
    }
    
    city_block <- bind_rows(mget(ls(pattern = paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_block_county_[1-5]"))))
    
    # Save the new object in the environment
    assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_block_", y), 
           city_block)
    
    # Export the new object as shapefile
    st_write(
      city_block,
      paste0(
        wd, "CleanedData/geomander/",
        str_replace_all(tolower(cities[i]), " ", "_"),
        "_block_", y),
      driver = "ESRI Shapefile", append = FALSE)
  }
}

blocks_path = "/Volumes/Yun Choi/columbia_university/gerrymandering-project/CleanedData/geomander/block_2010/"

save(file = paste0(blocks_path, "block_2010.rds"), list = ls(pattern = "_geomander_block_2010$")) 

# 1 New York
# 4 Houston
# 8 San Diego
# 9 Dallas
# 11 Austin
# 12 Jacksonville:
# 16 Charlotte
# 17 San Francisco - not contiguous
# 18 Seattle - not contiguous
# 19 Denver - not contiguous
# 21 Nashville
# 24 Boston
# 26 Detroit:
# 29 Baltimore
# 32 Fresno
# 35 Mesa
# 37 Atlanta

# 2:3; 5:7; 10; 13:15; 20; 22:23; 25; 27:28; 30:31; 33:34; 36; 38:

for (y in c(2010)) {
  for (i in 1:length(cities)) {
    # The resultant data from the previous for loop
    city_block <- mget(ls(pattern = paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_block_", y)))[[1]] %>%
      st_make_valid()
    
    if (y == 2010) {
      # Get the name of council district id column
      council_district_id <- cities_precinct_council_district_id_columns_2010[[i]][2]
    } else {
      council_district_id <- cities_precinct_council_district_id_columns_2020[[i]][2]
    }
    
    city_cd_count <- sf::read_sf(
      paste0('CleanedData/CityCouncilDistrict/',
             str_replace_all(tolower(cities[i]), " ", "_"), '_city_council_district.shp')) %>%
      pull(all_of(council_district_id)) %>% unique() %>% length()
    
    try(adj <- redist.adjacency(shp = city_block), silent = TRUE)
    
    g <- suggest_neighbors(adj = adj, city_block)
    
    if (!nrow(g) == 0) {
      for (n in 1:nrow(g)){
        adj <- adj %>% add_edge(g[[1]][n], g[[2]][n] + 1)
      }
      for (n in 1:nrow(g)){
        adj[[g[[1]][n]]] <- c(g[[1]][n], g[[2]][n])
      }
    } else {}
    
    map <- redist_map(city_block, pop_tol = 0.05, ndists = city_cd_count, adj = adj)
    
    plans <- redist_smc(map, nsims = 5, compactness = 1) 
    
    # Summarize the plans created
    plans <- plans %>%
      mutate(pop_dev = abs(total_pop / get_target(map) - 1),
             comp = distr_compactness(map, "PolsbyPopper"),
             pct_min = group_frac(map, vap - vap_white, vap),
             pct_dem = group_frac(map, dem, dem + rep))
    
    # Save the new object in the environment
    assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_plans_", y), plans)
  }
}

ls(pattern = "_geomander_plans_2010$")

plans_path = "/Volumes/Yun Choi/columbia_university/gerrymandering-project/CleanedData/geomander/plans_2010/"

save(file = paste0(plans_path, "plans_2010.rds"), list = ls(pattern = "_geomander_plans_2010$")) 

# Use QGIS-created objects for those who
cities_worked_with_geomander <- ls(pattern = "_geomander_plans_2010$") %>%
  str_remove("_geomander_plans_2010") %>%
  str_replace("_", " ") %>%
  str_to_title()

QGIS_cities_list <- cities[!cities %in% cities_worked_with_geomander]
QGIS_cities_index <- which(!cities %in% cities_worked_with_geomander)


###------------------------PART 2: Import QGIS-created Intersection-level Data and Compute Spatially-Weighted Estimates for Different Spatial Units

for (y in c(2010)) {
  for (i in QGIS_cities_index) {
    
    if (y == 2010) {
      cities_precinct_council_district_id_columns <- cities_precinct_council_district_id_columns_2010
    } else {
      cities_precinct_council_district_id_columns <- cities_precinct_council_district_id_columns_2020
    }
    
    precinct_id <- cities_precinct_council_district_id_columns[[i]][1]
    council_district_id <- cities_precinct_council_district_id_columns[[i]][2]
    
    # Import city_block_precinct_council_district intersection exported from QGIS
    block_precinct_council_district_intersection <- st_read(
      paste0(
        wd,
        "CleanedData/QGISOutput/Block_Precinct_CouncilDistrict/", # path to folder 
        str_replace_all(tolower(cities[i]), " ", "_"), 
        "_block_precinct_council_district_intersection_", y, ".shp"
      )
    ) %>%
      rename(prct_id = all_of(precinct_id), cd_id = all_of(council_district_id))
    
    block_geometry <- mget(ls(pattern = paste0("^", str_replace_all(tolower(cities[i]), " ", "_"),
                                               "_city_block_", y , "$")))[[1]] %>%
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
}

# PART 3: Second approach for cities that didn't work with the first approach

for (y in c(2010)){
  for (i in which(!cities %in% cities_worked_with_geomander)) {
    # The resultant data from the previous for loop
    city_block <- read_sf(
      paste0('CleanedData/CityIntersection/Block/',
             str_replace_all(tolower(cities[i]), " ", "_"), '_intersection_weighted_block', y, '.shp')) %>%
      st_make_valid()
    
    vap_data <- get_decennial(state = cities_states_codes[i],
                              county = cities_counties_codes[[i]],
                              geography = "block",
                              year = 2010,
                              variables = c("vap_white" = "P003002",
                                            "vap_black" = "P003003",
                                            "vap_asian" = "P003005",
                                            "vap" = "P003001"), 
                              output = "wide")
    
    city_block <- city_block %>%
      left_join(vap_data, by = "GEOID")
    
    # Get the name of council district id column
    council_district_id <- cities_precinct_council_district_id_columns_2010[[i]][2]
    
    city_cd_count <- sf::read_sf(
      paste0('CleanedData/CityCouncilDistrict/',
             str_replace_all(tolower(cities[i]), " ", "_"), '_city_council_district.shp')) %>%
      pull(all_of(council_district_id)) %>% unique() %>% length()
    
    try(adj <- redist.adjacency(shp = city_block), silent = TRUE)
    
    g <- suggest_neighbors(adj = adj, city_block)
    
    if (!nrow(g) == 0) {
      for (n in 1:nrow(g)){
        adj <- adj %>% add_edge(g[[1]][n], g[[2]][n] + 1)
      }
      for (n in 1:nrow(g)){
        adj[[g[[1]][n]]] <- c(g[[1]][n], g[[2]][n])
      }
    } else {}
    
    map <- redist_map(city_block, pop_tol = 0.05, total_pop = "sw_ttl_", ndists = city_cd_count, adj = adj, planarize = 4269)
    
    plans <- redist_smc(map, nsims = 5, compactness = 1) 
    
    # Summarize the plans created
    plans <- plans %>%
      mutate(pop_dev = abs(total_pop/get_target(map) - 1),
             comp = distr_compactness(map, "PolsbyPopper"),
             pct_min = group_frac(map, vap - vap_white, vap),
             pct_dem = group_frac(map, sw_g20prd, sw_g20prd + sw_g20prr))
    
    # Save the new object in the environment
    assign(paste0(str_replace_all(tolower(cities[i]), " ", "_"), "_geomander_plans_", y), plans)
  } 
}


#### Clean thus far
# Export the new object as RData
write_csv(
  plans,
  paste0(
    wd, "CleanedData/geomander/",
    str_replace_all(tolower(cities[i]), " ", "_"),
    "_plans"))

plan_sum <- group_by(plans, draw) %>%
  summarize(max_dev = max(pop_dev),
            avg_comp = mean(comp),
            max_pct_min = max(pct_min),
            dem_distr = sum(pct_dem > 0.5))

