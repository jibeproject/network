#SET UP
rm(list = ls())

if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")
if (!require("qgisprocess")) install.packages("qgisprocess")
install.packages("qgisprocess", dependencies = TRUE)
qgis_show_help("native:snapgeometries")

####################
#PART 1: get road network nodes, traffic signals and crossing facilities
###################
region_nm <- as.character("GreaterManchester")

#get network nodes
nodes <- st_read(file.path(".network-clean/",region_nm,"/network_nodes_z.gpkg"), drivers = "GPKG") #the Z coordinate is added directly in QGIS using the DEM model

#read network
edges <- st_read(file.path("./bigdata/network-clean/",region_nm,"/network_v3.13.gpkg")) #found in local V-drive

#read crossing points
crossingpnts <- st_read(file.path("./bigdata/osm-crossings-trafficsign/CrossingPoints_point.shp")) #found in GitHub in bigdata folder
crossingpnts <- crossingpnts[crossingpnts$Status == "Existing", ] #keep only existing

#read traffic signals
trafficsignal <- readRDS(file.path("./bigdata/osm-crossings-trafficsign/trafficsignal.Rds")) #in GitHub big data folder

#buffer existing TrafficSignals locations to remove crossing points in close proximity-- avoid double counting
trafficsignal_buf <- st_buffer(trafficsignal, 20)
crossingpnts_nosign <- st_intersection(crossingpnts, trafficsignal_buf)

#keep only crossing points not in TrafficSignals dataset
crossingpnts_nosign <- crossingpnts[!crossingpnts$geometry %in% crossingpnts_nosign$geometry, ]
#buffer crossing points excluding crossings already in TrafficSignals
crossingpnts_nosign_buf <- st_buffer(crossingpnts_nosign, 20)

####################
#PART 2: join PEDS traffic signals and crossing points
###################
##############
#TRAFFIC SIGNAL
##############
#join to edges
edges_trafficsignal <- edges[,c("edgeID", "highway")]

#remove motorways (to avoind spatial join error)
edges_trafficsignal <- edges_trafficsignal[edges_trafficsignal$highway != "motorway" & edges_trafficsignal$highway != "motorway_link", ]
edges_trafficsignal <- st_intersection(edges_trafficsignal, trafficsignal_buf[,c(3,26)]) %>% st_drop_geometry() %>% select(-highway) #get edgeIDs for trafficsig based

#exclude emergency Traffic Signals (for Fire Brigades)
edges_trafficsignal<- edges_trafficsignal[edges_trafficsignal$Type != "Wig Wag", ]

#count TrafficSignals of edges
edges_trafficsignal <- edges_trafficsignal %>% group_by(edgeID, Type, Crossing_F) %>% tally() %>% ungroup()

#join to nodes
nodes_trafficsignal <- st_intersection(nodes, edges_trafficsignal)
nodes_trafficsignal <- nodes_trafficsignal[,c("nodeID", "Type")] %>% st_drop_geometry() #get edgeIDs for trafficsig based
colnames(nodes_trafficsignal)[2] <- "peds_signal"

#get unique nodes
nodes_trafficsignal <- unique(nodes_trafficsignal$nodeID)

#find duplicated and remove
nodes_trafficsignal[duplicated(nodes_trafficsignal$nodeID) == TRUE, ]
row_dup <- c("173260.1")
nodes_trafficsignal <- nodes_trafficsignal[!(row.names(nodes_trafficsignal) %in% row_dup),]

##############
#CROSSING POINTS
##############
#join to edges
#crossing
edges_crossingpnts_nosign <- edges[,c("edgeID", "highway")]

#remove motorways (to avoid spatial join error)
edges_crossingpnts_nosign <- edges_crossingpnts_nosign[edges_crossingpnts_nosign$highway != "motorway" & edges_crossingpnts_nosign$highway != "motorway_link", ]

#get edgeIDs for crossing pnt
edges_crossingpnts_nosign <- st_intersection(edges_crossingpnts_nosign, crossingpnts_nosign_buf[,3]) %>% st_drop_geometry() %>% select(-highway)
edges_crossingpnts_nosign <- edges_crossingpnts_nosign %>% group_by(edgeID, Object_Ref) %>% tally() %>% ungroup()

#tally trafficsignal var 'type' and 'crossing facility' per edge-- multiple facilities per edge are merged
crossings_final_type <- edges_trafficsignal %>% group_by(edgeID) %>% summarise(signaltype = paste(unique(Type), collapse = "/"), signal_cnt = sum(n))
crossings_final_crossing <- edges_trafficsignal %>% group_by(edgeID) %>% summarise(crossingtype = paste(unique(Crossing_F), collapse = "/"))

#join to nodes
nodes_crossingpnts <- st_intersection(nodes[,"nodeID"], edges_crossingpnts_nosign[,"Object_Ref"]) %>% st_drop_geometry()
colnames(nodes_crossingpnts)[2] <- "peds_zebra"
#get unique crossings
nodes_crossingpnts <- unique(nodes_crossingpnts)

##############
#JOIN CROSSING POINTS AND TRAFFIC SIGNALS
##############

#join traffic signals with crossing for edges
crossings_final <- merge(crossings_final_type, crossings_final_crossing, by = "edgeID", all = TRUE)
crossings_final <- merge(crossings_final, edges_crossingpnts_nosign, by = "edgeID", all = TRUE) %>% ungroup()
crossings_final$signal_cnt <- ifelse(is.na(crossings_final$signal_cnt), as.numeric(0), crossings_final$signal_cnt)
crossings_final$n <- ifelse(is.na(crossings_final$n), as.numeric(0), crossings_final$n)

#tally crossings
crossings_final$cros_cnt <- rowSums(crossings_final[,c("signal_cnt", "n")])
crossings_final$crossingtype <- ifelse(is.na(crossings_final$crossingtype) | stringr::str_detect(crossings_final$crossingtype, "NA") == TRUE, crossings_final$Object_Ref, crossings_final$crossingtype)
crossings_final <- crossings_final %>% select(-c("Object_Ref", "signal_cnt", "n"))

#remove duplicates
#crossings_final <- crossings_final[unique(crossings_final$edgeID),]
#sense-check remaining duplicates
#crossings_final[duplicated(crossings_final$edgeID),]

#join traffic with crossing for nodes
nodes <- merge(nodes, nodes_trafficsignal, by = "nodeID", all.x = TRUE)
nodes <- merge(nodes, nodes_crossingpnts, by = "nodeID", all = TRUE)

#count crossings and signals on network nodes
nodes <- nodes %>% mutate(ped_cros = coalesce(peds_signal, peds_zebra)) %>% select(-c(peds_signal, peds_zebra))
st_write(nodes, file.path(paste0("./network_nodes_z.gpkg")), "network_nodes_z", driver="GPKG") #write for visual inspection in QGIS

####################
#PART 2.1: join CYCLIST traffic signals an parallel zebra crossings on network nodes
###################

##############
#TOUCAN
##############
#get signal controlled crossings for cyclists (containing TOUCAN)
toucan <- trafficsignal_buf[trafficsignal_buf$Type %like% "Toucan" == TRUE, ]
signal <- trafficsignal_buf[trafficsignal_buf$Type %like% "Signal" == TRUE & trafficsignal_buf$Crossing_F %like% "TOUCAN" == TRUE, ]

#join together
com <- rbind(toucan, signal)

#buffer signals
com_buf <- st_buffer(com, 20)

#spatial join on network nodes
nodes_trafsig <- st_intersection(nodes[,"nodeID"], com_buf[,"Type"]) %>% st_drop_geometry()

#merge network nodes with traffic signals
nodes <- merge(nodes, nodes_trafsig, by = "nodeID", all =TRUE)
colnames(nodes)[4] <- "trafsign"

#write for visual check
st_write(com_buf, file.path(paste0("./com_buf.gpkg")), "com_buf", driver="GPKG")

##############
#PARALLEL (zebra peds+cyc)
##############
#get parallel crossings
crossingparallel <- crossingpnts[crossingpnts$Object_Ref %like% "Parallel crossing point" == TRUE, ]
crossingparallel_buf <- st_buffer(crossingparallel, 20)
nodes_cross <- st_intersection(nodes[,"nodeID"], crossingparallel_buf[,"Object_Ref"]) %>% st_drop_geometry()
nodes <- merge(nodes, nodes_cross, by = "nodeID", all =TRUE)

#count crossings and signals on network nodes
nodes <- nodes %>% mutate(cyc_cros = coalesce(trafsign, Object_Ref))
nodes <- nodes[,c("nodeID", "z_coor", "ped_cros", "cyc_cros")]

##############
#PART 2: Get cycleway crossings from osm tag 'highway:cycleway'
##############
#buffer signals to find edges tagged 'highway:cycleway' intersecting with Signal type
signal_nul <- trafficsignal_buf[trafficsignal_buf$Type %like% "Signal" == TRUE & is.na(trafficsignal_buf$Crossing_F) == TRUE, ]
signal_nul_buf <- st_buffer(signal_nul, 50)

#read in QGIS pre-clipped network segments (using JoinAttributesByLocation 'intersects') tagged 'highway:cycleway' intersecting with Signal buffer
osm_cycleway_cros <- st_read(file.path("./GitHub_inputfiles_network/osm_cycleway_crossings.shp")) #in the Teams folder WP2>Data_WP2>Processed_Data>Greater Manchester>GitHub_inputfiles_network

#find network nodes on the cycleway crossing edges
cyc_sign_cros <- st_intersection(osm_cycleway_cros[, c("edgeID", "highway")], nodes[,c("nodeID")])
cyc_sign_cros <- st_intersection(cyc_sign_cros[,c("nodeID")], signal_nul_buf[,c("Type")])
cyc_sign_cros <- unique(cyc_sign_cros) %>% st_drop_geometry()

#clean duplicated
row <- c("325.1","326.1.1")
cyc_sign_cros <- cyc_sign_cros[!(row.names(cyc_sign_cros) %in% row),]

#merge cycle crossings Signal type to network nodes
nodes <- merge(nodes, cyc_sign_cros, by = "nodeID", all =TRUE)

#count cycle crossings
nodes <- nodes %>% mutate(cyc_cros = coalesce(cyc_cros, Type))
nodes <- nodes[,c("nodeID", "z_coor", "ped_cros", "cyc_cros")]

#sense-check
levels(as.factor(nodes$ped_cros))
levels(as.factor(nodes$cyc_cros))

#write final network nodes file
st_write(nodes, file.path(paste0("./bigdata/network-clean/",region_nm,"/network_nodes_z.gpkg")), "network_nodes_z", driver="GPKG") #write for visual inspection in QGIS
