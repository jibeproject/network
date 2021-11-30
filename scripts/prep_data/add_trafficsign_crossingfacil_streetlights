#set up
rm(list = ls())

if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")
if (!require("qgisprocess")) install.packages("qgisprocess")
install.packages("qgisprocess", dependencies = TRUE)

####################
#PART 1: get road network, traffic signals, crossing facilities and street lights
####################
#read osm network
osm <- st_read(file.path("01_DataInput/Network/GreaterManchester/network_edges_v2.1.gpkg"), drivers = "GPKG") #add this from V-drive as this is not a public dataset

#read traffic signals
trafficsignal <- st_read(file.path("../bigdata/osm-crossings-trafficsign/trafficsignal.Rds"))

#read crossing points
crossingpnts <- st_read(file.path("../bigdata/osm-crossings-trafficsign/crossingpnts.Rds"))
crossingpnts <- crossingpnts[crossingpnts$Status == "Existing", ] #keep only existing

#attributes spatial join
trafficsignal_buf <- st_buffer(trafficsignal, 20) #buffer existing TrafficSignals locations to remove crossing points
crossingpnts_nosign <- st_intersection(crossingpnts, trafficsignal_buf)
crossingpnts_nosign <- crossingpnts[!crossingpnts$geometry %in% crossingpnts_nosign$geometry, ] #keep only points not in TrafficSignals dataset
crossingpnts_nosign_buf <- st_buffer(crossingpnts_nosign, 20)

#read street lights
streetlgh <- st_read(file.path("01_DataInput/GM_streetlights/GM_streetlights.csv"), options=c("X_POSSIBLE_NAMES=easting", "Y_POSSIBLE_NAMES=northing"))  #add this from V-drive as this is not a public dataset
st_crs(streetlgh) <- 27700

####################
#PART 2: addign edgeID to crossing/trafficsignal attribute
####################
#traffic signal
osm_trafficsignal <- osm[,c("edgeID", "highway")]
osm_trafficsignal <- osm_trafficsignal[osm_trafficsignal$highway != "motorway" & osm_trafficsignal$highway != "motorway_link", ]
osm_trafficsignal <- st_intersection(osm_trafficsignal, trafficsignal_buf[,c(3,26)]) %>% st_drop_geometry() %>% select(-highway) #get edgeIDs for trafficsignals
osm_trafficsignal<- osm_trafficsignal[edges_trafficsignal$Type != "Wig Wag", ]
osm_trafficsignal <- osm_trafficsignal %>% group_by(edgeID, Type, Crossing_F) %>% tally() %>% ungroup()
#tally Toucan traffic signals (shared ped/cycl signal controlled crossings)
bike_trafficsignal <- osm_trafficsignal[grepl("Toucan | Toucan with PCAT | Double Toucan | Dual Toucan", edges_trafficsignal$Type) | grepl("TOUCAN", edges_trafficsignal$Crossing_F), ]
bike_trafficsignal <- bike_trafficsignal %>% group_by(edgeID) %>% summarise(signaltype = paste(unique(Type), collapse = "/"), bike_cros_cnt = sum(n))

#crossing facility
osm_crossingpnts_nosign <- osm[,c("edgeID", "highway")]
osm_crossingpnts_nosign <- osm_crossingpnts_nosign[osm_crossingpnts_nosign$highway != "motorway" & osm_crossingpnts_nosign$highway != "motorway_link", ]
osm_crossingpnts_nosign <- st_intersection(osm_crossingpnts_nosign, crossingpnts_nosign_buf[,3]) %>% st_drop_geometry() %>% select(-highway)#get edgeIDs for crossing pnt
osm_crossingpnts_nosign <- osm_crossingpnts_nosign %>% group_by(edgeID, Object_Ref) %>% tally() %>% ungroup()

#tally trafficsignal var 'type' and 'crossing facility' per edge-- multiple facilities per edge are merged
crossings_final_type <- osm_trafficsignal %>% group_by(edgeID) %>% summarise(signaltype = paste(unique(Type), collapse = "/"), signal_cnt = sum(n))
crossings_final_crossing <- osm_trafficsignal %>% group_by(edgeID) %>% summarise(crossingtype = paste(unique(Crossing_F), collapse = "/"))

#join traffic with crossing
crossings_final <- merge(crossings_final_type, crossings_final_crossing, by = "edgeID", all = TRUE)
crossings_final <- merge(crossings_final, osm_crossingpnts_nosign, by = "edgeID", all = TRUE) %>% ungroup()
crossings_final$signal_cnt <- ifelse(is.na(crossings_final$signal_cnt), as.numeric(0), crossings_final$signal_cnt)
crossings_final$n <- ifelse(is.na(crossings_final$n), as.numeric(0), crossings_final$n)

crossings_final$cros_cnt <- rowSums(crossings_final[,c("signal_cnt", "n")])
crossings_final$crossingtype <- ifelse(is.na(crossings_final$crossingtype) | stringr::str_detect(crossings_final$crossingtype, "NA") == TRUE, crossings_final$Object_Ref, crossings_final$crossingtype)
crossings_final <- crossings_final %>% select(-c("Object_Ref", "signal_cnt", "n"))

####################
#PART 3: add weight variable
####################
#Type            | Crossing Facility                                   JIBE

#link with no crossing facilities                                      1 - very hard
#Double Pelican  | PEDESTRIAN                                          3 - fairly easy
#Double Toucan   | TOUCAN or PEDESTRIAN or Pedestrian                  3 - fairly easy
#Double Puffin   | PEDESTRIAN                                          3 - fairly easy
#Dual Toucan     | PEDESTRIAN                                          2 - hard
#Dual Puffin     | PEDESTRIAN                                          2 - hard
#Wig Wag         | NA                                                  REMOVED
#Signal          | 1251 NA + 86 TOUCAN or PEDESTRIAN + 25 PEGASUS      4 - easy
#Toucan          | TOUCAN or Pedestrian or NA                          4 - easy
#Pegasus         | PEDESTRIAN or PEGASUS                               4 - easy
#Pelican         | PEDESTRIAN                                          5 - very easy
#Puffin          | PEDESTRIAN or TOUCAN or NA or PUFFIN                5 - very easy
#Toucan with PCAT| Pedestrian                                          5 - very easy
#Puffin with PCAT| PEDESTRIAN                                          5 - very easy
#Signal (Trunk)  | PEDESTRIAN or TOUCAN                                3 - fairly easy
#Pelican (Trunk) | PEDESTRIAN                                          3 - fairly easy
#Puffin (Trunk)  | PEDESTRIAN                                          3 - fairly easy

#add 'Signal' type for crossings missing information
crossings_final$signaltype <- ifelse(is.na(crossings_final$signaltype), "Signal", crossings_final$signaltype)

#replace nominal with Likert score
crossings_final$signaltype <- ifelse(stringr::str_detect(crossings_final$signaltype, "Double Pelican") == TRUE | stringr::str_detect(crossings_final$signaltype, "Double Toucan") == TRUE |
           stringr::str_detect(crossings_final$signaltype, "Double Puffin") == TRUE | stringr::str_detect(crossings_final$signaltype, "Signal (Trunk)") == TRUE |
           stringr::str_detect(crossings_final$signaltype, "Pelican (Trunk)") == TRUE | stringr::str_detect(crossings_final$signaltype, "Puffin (Trunk)") == TRUE , as.numeric(3), crossings_final$signaltype)
crossings_final$signaltype <- ifelse(stringr::str_detect(crossings_final$signaltype, "Dual Toucan") == TRUE | stringr::str_detect(crossings_final$signaltype, "Double Puffin") == TRUE, as.numeric(2), crossings_final$signaltype)
crossings_final$signaltype <- ifelse(stringr::str_detect(crossings_final$signaltype, "Signal") == TRUE | stringr::str_detect(crossings_final$signaltype, "Toucan") == TRUE |
           stringr::str_detect(crossings_final$signaltype, "Pegasus") == TRUE, as.numeric(4), crossings_final$signaltype)
crossings_final$signaltype <- ifelse(stringr::str_detect(crossings_final$signaltype, "Pelican") == TRUE | stringr::str_detect(crossings_final$signaltype, "Puffin") == TRUE |
           stringr::str_detect(crossings_final$signaltype, "Toucan with PCAT") == TRUE | stringr::str_detect(crossings_final$signaltype, "Puffin with PCAT") == TRUE, as.numeric(5), crossings_final$signaltype)
crossings_final <- crossings_final %>% select(-crossingtype)

#add to the edges
osm <- merge(osm, crossings_final, by = "edgeID", all.x = TRUE) #attach crossing facility info on the edges
osm$signaltype <- as.numeric(osm$signaltype)

#replace NAs for 'signaltype' with 1 (very hard to cross) and 'cros_cnt' with 0
osm$signaltype <- ifelse(is.na(osm$signaltype), as.numeric(1), osm$signaltype)
osm$cros_cnt<- ifelse(is.na(osm$cros_cnt), as.numeric(0), osm$cros_cnt)

#get crossings ratio per edge length
osm <- osm %>% mutate(cross_ratio = cros_cnt/length*signaltype)

####################
#PART 4: add bike crossing attribute
####################

#add bike trafficsignals to edges
osm <- merge(osm, bike_trafficsignal[,c("edgeID", "bike_cros_cnt")], all = TRUE)
edges$bike_cros_cnt<- ifelse(is.na(edges$bike_cros_cnt), as.numeric(0), edges$bike_cros_cnt) #replace NAs with 0

#get crossings ratio per edge length
osm <- oam %>% mutate(bike_cross_ratio = bike_cros_cnt/length*signaltype)

####################
#PART 5: join street lights on the edge
####################
#snap street lights points to OSM edges
snap <- qgis_run_algorithm(
  "native:snapgeometries",
  BEHAVIOR = 3, #Prefer closest point, don't insert new vertices
  INPUT = streetlgh,
  REFERENCE_LAYER = osm,
  TOLERANCE = 15
)

snap_sf <- sf::read_sf(qgis_output(snap, "OUTPUT"))
snapbuffer <- st_buffer(snap_sf, dist = 0.05)
osm_streetlgh <- st_intersection(osm[ , "edgeID"], snapbuffer[ ,"col_ref"]) %>% st_drop_geometry() #get edgeIDs for streetlights pnt
osm_streetlgh <- osm_streetlgh %>% group_by(edgeID) %>% tally() %>% mutate(strlgh_count = n) %>% select(-n) %>% ungroup()

#add to the edges
osm <- merge(osm, osm_streetlgh, by = "edgeID", all = TRUE) #attach crossing facility info on the edges
osm$streetlgh <- ifelse(is.na(osm$streetlgh), as.numeric(0), osm$streetlgh) #replace NAs with 0

####################
#PART 6: join traffic calming on the edge
####################

traffic_calm <- st_read(file.path("01_DataInput/TrafficCalming/20210823_OSDataForCambridge/TrafficCalmingLine_polyline.shp")) #add this from V-drive as this is not a public dataset

sample <- qgis_run_algorithm(
  "native:pointsalonglines",
  INPUT = traffic_calm,
  DISTANCE = 30,
  START_OFFSET = 1,
  END_OFFSET = 1
)

sample_sf <- sf::read_sf(qgis_output(sample, "OUTPUT"))

#snap traffic calming points to OSM edges
snap <- qgis_run_algorithm(
  "native:snapgeometries",
  BEHAVIOR = 1, #Prefer closest point, insert extra vertices where required
  INPUT = sample_sf,
  REFERENCE_LAYER = edges,
  TOLERANCE = 10
)

snap_sf <- sf::read_sf(qgis_output(snap, "OUTPUT"))
snapbuffer <- st_buffer(snap_sf, dist = 0.05)

#add traffic_calm info to edges
osm_traffic_calm <- st_intersection(osm[,"edgeID"], snapbuffer[,"structure"]) %>% st_drop_geometry() #get edgeIDs for traffic_calm pnt
osm_traffic_calm <- distinct(osm_traffic_calm)
osm <- merge(osm, osm_traffic_calm, by = "edgeID", all = TRUE) #attach traffic_calm info on the edges

osm$structure <- ifelse(is.na(osm$structure), as.character("no"), osm$structure)
colnames(osm)[colnames(osm) == "structure"] <- "traf_calm_ftr"

#save for next stages
saveRDS(osm, paste0("../bigdata/network-clean/",region_nm,"/crossings_streetlgts_trafcalm.Rds"))
rm(osm)
