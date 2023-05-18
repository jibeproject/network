#set up

if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")
if (!require("dbscan")) install.packages("dbscan")
if (!require("vegan")) install.packages("vegan")
if (!require("nngeo")) install.packages("nngeo")

region_nm <- as.character("GreaterManchester")
regions <- regions.todo

####################
#PART 1: get road network and POIs (clip to GM)
####################

#read GM network
osm <- readRDS(file.path("../bigdata/network-clean/",regions[a],"/network_edges.Rds"))

#read all pois
poi_all <- st_read(file.path("01_DataInput/poi/Download_poi_UK_1803987/poi_4138552/poi_4138552.gpkg")) #add this from V-drive as this is not a public dataset
#constrain to Greater Manchester region
gm_bound <- st_read(file.path("../bigdata/boundaries/cityregions/",region_nm,"/bounds.geojson"))
gm_poi <- st_intersection(poi_all, gm_bound)

####################
#PART 2: find POI clusters to filter out remote POI locations (run only for 'High Streets')
####################
#read high street codes
poi_highstreet_codes <- utils::read.csv(file = file.path("../bigdata/highstreet_codes.csv"), header = FALSE)
poi_highstreet_codes[1,1] <- c("10590732")
colnames(poi_highstreet_codes)[1] <- "pointx_class"

#get pois on High Streets
gm_poi_highstreets <- gm_poi[gm_poi$pointx_class %in% poi_highstreet_codes$pointx_class, ]

#get high street pois coordinates
poi_coors <- gm_poi_highstreets %>% st_coordinates()

#choose 'eps' (the epsilon neighborhood of one point aka. search radius) estimated from the spatial clustering of points at hand using the kNN function
#The algorithm classifies the points in three categories: core points; reachable points and noise.Core points and reachable points are part of the same cluster
#if they can be reach at an epsilon radius distance measured at one of the core points.The points from which no more points can be reached at the epsilon
#distance are said to be the reachable points of that cluster that form its border points. Only core points can reach reachable (i.e. non-core) points and not the
#the other way around. Border points that belong to the same set are said to be density connected. Points labeled as noise are the ones that are not density
#reachable from any core point.
#kNNdistplot(poi_coors, k = 20)
#kNNdistplot(poi_coors, k = 5)

#initial test on 'eps' 150 and 'kNN' 15 based on OS 'High Streets criteria'; out of experimental try tested also for  20/200; 30/300; 20/100; 10/100; 5/50; 5/150
#from visual estimate for 'eps' based on var kNN distance curve 'minPts/eps'-- 5/500; 50/500; 10/1100; 15/1000; 20/1000; 5/750; 30/1900;

#get POI clusters
gm_poi_highstreets$geog_cluster <- dbscan(poi_coors, eps = 150, minPts = 10) %>% purrr::pluck('cluster') %>% as.character()
gm_poi_highstreets$geog_cluster <- as.numeric(gm_poi_highstreets$geog_cluster)
clustered_pois <- gm_poi_highstreets[gm_poi_highstreets$geog_cluster > 0, ]
noise <- gm_poi_highstreets[gm_poi_highstreets$geog_cluster == 0, ]

#write for visual check in QGIS
#st_write(clustered_pois, file.path(paste0("02_DataOutput/network/gm/edges/poi_attr")), "clustered_pois_5_150.shp", driver="ESRI Shapefile")
#st_write(noise, file.path(paste0("02_DataOutput/network/gm/edges/poi_attr")), "poi_osm_noise_5_150.shp", driver="ESRI Shapefile")

#spatial join 1-NN (i.e. edgeID of the closest road segment to POI) -- POINT output
join_highstr <- st_join(clustered_pois, osm, join=nngeo::st_nn, maxdist = 50, k = 1) #point output, attaches edgeIDs to points

#create new binary (yes/no) attribute value to the osm link
clustered_pois_var <- join_highstr %>% group_by(edgeID, pointx_class) %>% tally() %>% ungroup()
clustered_pois_var$geom <- NULL
clustered_pois_var <- aggregate(. ~ edgeID, data=clustered_pois_var[,c("edgeID","n")], FUN=sum)

osm <- merge(osm, clustered_pois_var, by.x = "edgeID", all.x = TRUE) %>% st_as_sf()
osm <- osm %>% mutate(highstr = ifelse(!is.na(osm$n), "yes", "no")) %>% select(-n)

####################
#PART 3: Create individual and negative POIs
####################
#PART 3.1:get individual poi weighted
################
#read individual codes
poi_individual_codes <- utils::read.csv(file = file.path("../bigdata/individual_codes.csv"), header = FALSE)
poi_individual_codes[1,1] <- c("06340453")
colnames(poi_individual_codes)[1] <- "pointx_class"

#get individual as sf
gm_poi_individual <- gm_poi[gm_poi$pointx_class %in% poi_individual_codes$pointx_class, ]

#read individual (positive) codes weighted
individual_wgt <- utils::read.csv(file = file.path("../bigdata/individual_wgt.csv"), header = FALSE)
individual_wgt$V2[is.na(individual_wgt$V2)] <- as.numeric("1")
individual_wgt[1,1] <- "06340453"
colnames(individual_wgt)[1] <- "pointx_class"
colnames(individual_wgt)[2] <- "weight"

#merge weights
gm_poi_individual <- merge(gm_poi_individual, individual_wgt, by = "pointx_class")

#spatial join 1-NN (i.e. edgeID of the closest road segment to POI) -- POINT output, attaches edgeIDs to points
ind_poi_osm_join <- st_join(gm_poi_individual[,c("pointx_class", "ref_no", "weight")], osm, join=nngeo::st_nn, maxdist = 50, k = 1) #point output
ind_poi_osm_join$geometry <- NULL
ind_count <- ind_poi_osm_join %>% group_by(edgeID, pointx_class, weight) %>% tally() %>% mutate(indpoi_score = n * weight)
ind_count <- aggregate(. ~ edgeID, data=ind_count[,c("edgeID", "indpoi_score")], FUN=sum)

#asign new count-based attribute to osm network
osm <- merge(osm, ind_count, by = "edgeID", all.x = TRUE)

################
#PART 3.2:get green space access points
################
#read green space access pnts
green_access_pnts <- st_read(file.path("01_DataInput/poi/VectorData/open-greenspace_4206589/GB_AccessPoint.shp")) #add this from V-drive as this is not a public dataset
green_access_pnts <- st_zm(green_access_pnts, drop = TRUE, what = "ZM")
#constrain to Greater Manchester region
gm_green_access_pnts <- st_intersection(green_access_pnts, gm_bound)

#spatial join 1-NN (i.e. edgeID of the closest road segment to POI) -- POINT output, attaches edgeIDs to points
green_osm_join <- st_join(gm_green_access_pnts[,1], osm, join=nngeo::st_nn, maxdist = 50, k = 1) #point output
green_osm_join$geometry <- NULL
green_count <- green_osm_join %>% group_by(edgeID, id) %>% tally()
green_count <- aggregate(. ~ edgeID, data=green_count[,c("edgeID", "n")], FUN=sum)
green_count <- green_count %>% mutate(green_count = n) %>% select(-n)

#asign new count-based attribute to osm network
osm <- merge(osm, green_count, by = "edgeID", all.x = TRUE)

################
#PART 3.3:get negative codes weighted
################
#read negative codes
poi_negative_codes <- utils::read.csv(file = file.path("01_DataInput/poi/negative_codes.csv"), header = FALSE)
poi_negative_codes[1,1] <- c("06340462")
colnames(poi_negative_codes)[1] <- "pointx_class"

#get negative as sf
gm_poi_negative <- gm_poi[gm_poi$pointx_class %in% poi_negative_codes$pointx_class, ]

#read negative codes weighted
negative_wgt <- utils::read.csv(file = file.path("../bigdata/negative_wgt.csv"), header = FALSE)
negative_wgt$V2[is.na(negative_wgt$V2)] <- as.numeric("1")
negative_wgt[1,1] <- "06340462"
colnames(negative_wgt)[1] <- "pointx_class"
colnames(negative_wgt)[2] <- "weight"

#merge weights
gm_poi_negative <- merge(gm_poi_negative, negative_wgt, by = "pointx_class")

#spatial join 1-NN (i.e. edgeID of the closest road segment to POI) -- POINT output
neg_poi_osm_join <- st_join(gm_poi_negative[,c("pointx_class", "ref_no", "weight")], osm, join=nngeo::st_nn, maxdist = 50, k = 1) #point output, attaches edgeIDs to points
neg_poi_osm_join$geometry <- NULL
neg_count <- neg_poi_osm_join %>% group_by(edgeID, pointx_class, weight) %>% tally() %>% mutate(negpoi_score = n * weight)
neg_count <- aggregate(. ~ edgeID, data=neg_count[,c("edgeID", "negpoi_score")], FUN=sum)

#asign new count-based attribute to osm network
osm <- merge(osm, neg_count, by = "edgeID", all.x = TRUE)

saveRDS(osm,paste0("../bigdata/network-clean/",regions[a],"/network_edges.Rds"))
####################
#PART 4: get POI count and diversity (SKIP FOR NOW)
####################
#get count and proportion
clustered_pois <- join_highstr %>% group_by(osm_id, pointx_class) %>% tally() %>% ungroup()
clustered_pois$geom <- NULL
clustered_pois <- clustered_pois  %>% mutate(prop = n/sum(n))

count <- as.data.frame.matrix(xtabs(n ~ osm_id + pointx_class, clustered_pois), responseName = "osm_id") #convert to matrix

#get Shannon's Index
shannon <- vegan::diversity(count, index = "shannon") %>% as.data.frame()
shannon$osm_id <- rownames(shannon)
colnames(shannon)[1] <- "shannon"
clustered_pois <- merge(clustered_pois, shannon, by.x = "osm_id", all.x = TRUE)

#get Simpson's Index-- measure of dominance and as such weights towards the abundance of the most common taxa
simpson <- vegan::diversity(count, index = "simpson") %>% as.data.frame()
simpson$osm_id <- rownames(simpson)
colnames(simpson)[1] <- "simpson"

#join both back to osm
osm_edgeattr <- merge(osm, shannon, by.x = "osm_id", all.x = TRUE) %>% st_as_sf()
osm_edgeattr <- merge(osm_edgeattr, simpson, by.x = "osm_id", all.x = TRUE) %>% st_as_sf()
