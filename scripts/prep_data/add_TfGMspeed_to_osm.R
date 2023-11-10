#SET UP
rm(list = ls())

if (!require("sf")) install.packages("sf")
if (!require("rgdal")) install.packages("rgdal")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("qgisprocess")) install.packages("qgisprocess")
install.packages("qgisprocess", dependencies = TRUE)
if (!require("missForest")) install.packages("missForest")
if (!require("doParallel")) install.packages("doParallel")

help <- qgis_algorithms()
qgis_show_help("qgis:joinbylocationsummary")
library(missForest)

setwd("")
#################
# PART 1: read MasterMap road network and join with TfGM speed data
#################
# read input file geodatabase
fgdb <- "GitHub_inputfiles_network/Highways_Network_March19.gdb" #in the Teams folder WP2>Data_WP2>Processed_Data>Greater Manchester>GitHub_inputfiles_network
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
masmap <- st_read(dsn=fgdb,layer="RoadLink_N")
#get GM bounds
gm_bound <- st_read(file.path("GitHub_inputfiles_network/bounds.geojson")) #in the Teams folder WP2>Data_WP2>Processed_Data>Greater Manchester>GitHub_inputfiles_network
#clip to GM bounds
gm_masmap <- st_intersection(masmap, gm_bound) #clip to GM

#read TfGM speed data
speed_85per <- readxl::read_excel(file.path("GitHub_inputfiles_network/TfGM 85th%ile Journey Times.xlsx"), sheet = "Sheet2") #in the Teams folder WP2>Data_WP2>Processed_Data>Greater Manchester>GitHub_inputfiles_network
speed_85per$speedMPH <- speed_85per$speedMPH * 1.609344 #convert m/h to km/h
colnames(speed_85per)[4] <- "speedKPH"
speed_85per <- speed_85per %>% mutate(Link_id = gsub("[a-zA-Z ]", "", Link_id))
speed_85per <- speed_85per %>% group_by(Link_id) %>% mutate(speedKPH=mean(speedKPH)) %>% ungroup()

speed_85per <- speed_85per[!duplicated(speed_85per$Link_id),]
speed_85per$Link_id <- as.character(speed_85per$Link_id)
colnames(speed_85per)[1] <- "identifier"

gm_masmap <- merge(gm_masmap, speed_85per[,c("identifier", "speedKPH")], by = "identifier", all = TRUE)

#################
# PART 2: join to osm network
#################
#read osm
osm <- st_read(file.path("./bigdata/poi/osm_poi.Rds"), drivers = "GPKG")
#sample points along gm_mastermap
sample <- qgis_run_algorithm(
  "native:pointsalonglines",
  INPUT = gm_masmap,
  DISTANCE = 30,
  START_OFFSET = 5,
  END_OFFSET = 5
)

sample_sf <- sf::read_sf(qgis_output(sample, "OUTPUT"))

#snap gm_mastermap points to OSM edges
snap <- qgis_run_algorithm(
  "native:snapgeometries",
  BEHAVIOR = 1, #Prefer closest point, insert extra vertices where required
  INPUT = sample_sf,
  REFERENCE_LAYER = osm,
  TOLERANCE = 20
)

snap_sf <- sf::read_sf(qgis_output(snap, "OUTPUT"))
snapbuffer <- st_buffer(snap_sf, dist = 0.05)

#add speed info to edges
osm_speed <- st_intersection(osm[,"edgeID"], snapbuffer[,"speedKPH"]) %>% st_drop_geometry() #get edgeIDs for gm_mastermap pnt
osm_speed <- osm_speed %>% group_by(edgeID) %>% mutate(speedKPH=mean(speedKPH)) %>% ungroup()
osm_speed <- distinct(osm_speed)
osm <- merge(osm, osm_speed, by = "edgeID", all = TRUE) #attach speedKPH info on the edges

#################
# PART 3: imputing missing data
#################
#run a test to check the missing values in attributes on the network
per_miss <- function(x) {sum(is.na(x))/length(x)*100}
apply(osm, 2, per_miss)

#RESULT: aadt_all_median -- 64.85% of the observations are missing data
#RESULT: speedKPH -- 83.75% of the observations are missing data

#Impute using missForest function for speedKPH using the variables-- 2=maxspeed, 3=averageWidth.imp and 4=speedKPH
osm_imp <- osm %>% select(edgeID, maxspeed, averageWidth.imp, speedKPH) %>% st_drop_geometry() %>% as.data.frame()
doParallel::registerDoParallel(cores = 4)
osm_imp[,2:4] <- missForest(osm_imp, parallelize = 'forests')$ximp[,2:4]

osm <- osm %>% select(-speedKPH) #remove speedKPH with missing values
osm <- merge(osm, osm_imp[,c("edgeID", "speedKPH")], by = "edgeID", all = TRUE) #attach speedKPH after imputation back on the edges

#save output
dir.create(paste0("./bigdata/speedTfGM"))
saveRDS(osm, paste0("./bigdata/speedTfGM/osm_speed.Rds"))
