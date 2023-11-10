### SCRIPT TO CONVERT MANCHESTER NETWORK LINKS V3.12 TO V3.13 ###
library(tidyverse)
library(dplyr)

region_nm <- as.character("GreaterManchester")
network <- readRDS(file.path("./bigdata/speedTfGM/",region-nm,"/osm_speed.Rds"))

######################
#PART 1: CONVERT DATA TYPES ###
######################
network$indp_sc = as.integer(network$indp_sc)
network$ngp_scr = as.integer(network$ngp_scr)
network$crim_cnt = as.integer(network$crim_cnt)
network$negpoi_hgv_score = as.integer(network$negpoi_hgv_score)
network$junctn = network$junctin
network$maxspeed = network$maxsped

### DEFINE ALLOWED MODES ###
network$modes = recode(network$roadtyp,
                       "Shared Bus Lane" = "bus",
                       "Pedestrian Path - Cycling Forbidden" = "walk,bike",
                       "Path - Cycling Forbidden" = "walk,bike",
                       "Cycleway" = "walk,bike",
                       "Segregated Cycleway" = "walk,bike",
                       "Shared Path" = "walk,bike",
                       "Segregated Shared Path" = "walk,bike",
                       "Living Street" = "walk,bike,car,truck",
                       "Residential Road - Cycling Allowed" = "walk,bike,car,truck",
                       "Minor Road - Cycling Allowed" = "walk,bike,car,truck",
                       "Main Road - Cycling Allowed" = "walk,bike,car,truck",
                       "Main Road Link - Cycling Allowed" = "walk,bike,car,truck",
                       "Trunk Road Link - Cycling Allowed" = "walk,bike,car,truck",
                       "Trunk Road - Cycling Allowed" = "walk,bike,car,truck",
                       "Special Road - Cycling Forbidden" = "car,truck",
                       "motorway_link - Cycling Forbidden" = "car,truck",
                       "motorway - Cycling Forbidden" = "car,truck")

network$modes <- ifelse(network$modalfl == "all",network$modes,gsub(",car,truck","",network$modes))

network$laneCapacity = recode(network$roadtyp,
                              "Shared Bus Lane" = 0,
                              "Pedestrian Path - Cycling Forbidden" = 0,
                              "Path - Cycling Forbidden" = 0,
                              "Cycleway" = 0,
                              "Segregated Cycleway" = 0,
                              "Shared Path" = 0,
                              "Segregated Shared Path" = 0,
                              "Living Street" = 300,
                              "Special Road - Cycling Forbidden" = 600,
                              "Residential Road - Cycling Allowed" = 600,
                              "Minor Road - Cycling Allowed" = 1000,
                              "Main Road - Cycling Allowed" = 1500,
                              "Main Road Link - Cycling Allowed" = 1500,
                              "Trunk Road Link - Cycling Allowed" = 1500,
                              "motorway_link - Cycling Forbidden" = 1500,
                              "Trunk Road - Cycling Allowed" = 2000,
                              "motorway - Cycling Forbidden" = 2000)


network$freespeed = network$maxsped * 0.44704
oneway = startsWith(network$onwysmm,"One Way")

allowsCarOut = grepl("car",network$modes)
allowsCarRtn = allowsCarOut & !oneway

splitWidth = allowsCarRtn | (!allowsCarOut & grepl("walk,bike",network$modes))

width_out = ifelse(splitWidth,network$avg_wdt_mp / 2,network$avg_wdt_mp)
width_rtn = ifelse(splitWidth,network$avg_wdt_mp / 2,0)

network$permlanes = as.integer(case_when(width_out <= 6.5 ~ 1,
                                         width_out <= 9.3 ~ 2,
                                         TRUE ~ 3))

network$capacity = network$laneCapacity * network$permlanes

######################
#PART 2: CLASSIFY LINKS AS URBAN VS. RURAL
######################

# OAs can be found at: https://statistics.ukdataservice.ac.uk/dataset/2011-census-geography-boundaries-output-areas-and-small-areas
# RUC can be found at: https://geoportal.statistics.gov.uk/datasets/rural-urban-classification-2011-of-output-areas-in-england-and-wales-1/about
OAs <- sf::read_sf("./infuse_oa_lyr_2011_clipped.shp")
RUC <- readr::read_csv("./RUC11_OA11_EW/RUC11_OA11_EW.csv")

UrbanOAs <- OAs %>% left_join(RUC, by = c("geo_code" = "OA11CD")) %>% filter(startsWith(RUC11,"Urban"))
test <- sf::st_intersects(network,UrbanOAs)
network$urban <- sapply(test,length) > 0

######################
#PART 3: Add TfGM measured speed
######################
tfgm_speed <- readRDS("02_DataOutput/network/gm/tfgm_speed.Rds") #file found in WP2>Data_WP2>Processed_Data>Greater Manchester>GitHub_inputfiles_network

network <- left_join(network, tfgm_speed, by = "edgeID") %>% select(-spedKPH)
colnames(network)[74] <- "spedKPH"

sf::write_sf(network,file.path("./bigdata/network-clean/",region_nm,"/network_v3.13.gpkg"))
