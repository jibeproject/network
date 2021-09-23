#set up

if (!require("dplyr")) install.packages("dplyr")
if (!require("sf")) install.packages("sf")
if (!require("dbscan")) install.packages("dbscan")
if (!require("vegan")) install.packages("vegan")

####################
#PART 1: get road network and POIs (clip to GM)
####################

#read GM network
osm <- st_read(file.path("01_DataInput/Network/GreaterManchester/network-addedinfo.geojson"))

#read all pois
poi_all <- st_read(file.path("01_DataInput/poi/Download_poi_UK_1803987/poi_4138552/poi_4138552.gpkg"))
poi_codes <- utils::read.csv(file = file.path("01_DataInput/poi/codes.csv"), header = FALSE)
poi_codes[1,1] <- c("01020012")
colnames(poi_codes)[1] <- "pointx_class"

#constrain to Greater Manchester region
gm_bound <- st_read(file.path("01_DataInput/Cityreg_bounds/GreaterManchester/bounds.geojson"))
gm_poi <- st_intersection(poi_all, gm_bound)

#interesting POIs
poi_edgeattract <- gm_poi[gm_poi$pointx_class %in% poi_codes$pointx_class, ]

####################
#PART 2: find POI clusters to filter out remote POI locations (run only for 'High Streets')
####################

#get poi coordinates
poi_coors <- poi_edgeattract %>% st_coordinates()

#choose 'eps' (the epsilon neighborhood of one point aka. search radius) estimated from the spatial clustering of points at hand using the kNN function
#The algorithm classifies the points in three categories: core points; reachable points and noise.Core points and reachable points are part of the same cluster
#if they can be reach at an epsilon radius distance measured at one of the core points.The points from which no more points can be reached at the epsilon
#distance are said to be the reachable points of that cluster that form its border points. Only core points can reach reachable (i.e. non-core) points and not the
#the other way around. Border points that belong to the same set are said to be density connected. Points labeled as noise are the ones that are not density
#reachable from any core point.
kNNdistplot(poi_coors, k = 20)
kNNdistplot(poi_coors, k = 5)

#initial test on 'eps' 150 and 'kNN' 15 based on OS High Streets criteria; out of experimental try tested also for  20/200; 30/300; 20/100; 10/100; 5/50; 5/150
#from visual estimate for 'eps' based on var kNN distance curve 'minPts/eps'-- 5/500; 50/500; 10/1100; 15/1000; 20/1000; 5/750; 30/1900;

#get POI clusters
poi_edgeattract$geog_cluster <- dbscan(poi_coors, eps = 150, minPts = 5) %>% pluck('cluster') %>% as.character()
poi_edgeattract$geog_cluster <- as.numeric(poi_edgeattract$geog_cluster)
clustered_pois <- poi_edgeattract[poi_edgeattract$geog_cluster > 0, ]
noise <- poi_edgeattract[poi_edgeattract$geog_cluster == 0, ]

#write for visual check in QGIS
st_write(clustered_pois, file.path(paste0("02_DataOutput/network/gm/edges/poi_attr")), "clustered_pois_5_150.shp", driver="ESRI Shapefile")
st_write(noise, file.path(paste0("02_DataOutput/network/gm/edges/poi_attr")), "poi_osm_noise_5_150.shp", driver="ESRI Shapefile")

####################
#PART 3: select individual POIs
####################
####################
#PART 4: assign osm_id to POIs (for more fine-grain results use edgeID instead)
####################
#VAR 1: spatial join 1-NN (i.e. osm_id/edgeID of closest road segment to POI) -- POINT output
join <- st_join(clustered_pois, osm, join=nngeo::st_nn, maxdist = 1000, k = 1) #good output, attaches osm_ids to points
#write for visual check in QGIS
st_write(join, file.path(paste0("02_DataOutput/network/gm/edges/poi_attr")), "poi_osm_join.shp", driver="ESRI Shapefile")

####################
#PART 5: get POI count and diversity
####################
#get count and proportion
clustered_pois <- join %>% group_by(osm_id, pointx_class) %>% tally() %>% ungroup()
clustered_pois$geom <- NULL
clustered_pois <- clustered_pois  %>% mutate(prop = n/sum(n))

count <- as.data.frame.matrix(xtabs(n ~ osm_id + pointx_class, clustered_pois_var2_copy), responseName = "osm_id") #convert to matrix

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

st_write(osm_edgeattr, file.path(paste0("02_DataOutput/network/gm/edges/poi_attr")), "osm_edgeattr_shan_sim.shp", driver="ESRI Shapefile")
