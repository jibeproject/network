#########################
#This attribute is relevant for the MATSim routing script found in
#src/main/java/network/CreateMatsimNetworkRoad.java  (code lines 344-347)

#SET UP
rm(list = ls())

#load libraries
if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")
if (!require("mice")) install.packages("mice")
library(mice)

#################
# PART 1: get road network
#################
#read osm
osm <- st_read(file.path("../bigdata/network-clean/",regions[a],"/network_edges.Rds"))

#remove old AADT imputed values
osm <- osm %>% select(-c('aadt_mp'))

#remove C2(medium heavy >3.5 tons vehicles) and C3(heavy vehicles) AADT classes from the AADT median
osm <- osm %>% dplyr::mutate(aadt_hgv = aadt_md - c2_2019 - c3_2019)
osm <- osm[,c(1:40, 64, 41:63)]#reorganise columns

#add hgv-amplified stress; adjusted for HGV- volume (c2_2019 and c3_2019) by a factor of 6.019 each, based on the study findings in https://injuryprevention.bmj.com/content/27/1/71.abstract 
osm$aadt_hgv <- osm$aadt_hgv + osm$c2_2019*6.019 + osm$c3_2019*6.019

#add hgv-amplified stress to links with same OSM id (where AADT info is missing)
osm <- osm %>% group_by(osm_id) %>% tidyr::fill(aadt_hgv, .direction = "up") %>% ungroup()

#################
# PART 2: impute missing aadt_hgv with MICE
#################
#clip to ONLY GM region (excluding the 10km buffer)
gm_bounds <- st_read(file.path("01_DataInput/SpeedDataTfGM/GM_bounds_unbuf.shp"))
osm_gm <- st_intersection(osm, gm_bounds)

#impute missing values
osm_imp <- osm_gm %>% select(edgeID, quitnss, avg_wdt_mp, maxsped, aadt_hgv) %>% st_drop_geometry() %>% as.data.frame()
osm_imp <- mice(osm_imp) #maxit-- maximum number of iterations, m-- imputed dataset(s), multiple results

#select best imputation output based on sense-Check
imputed <- complete(osm_imp,5)[,c(1,5)]

#add imputed aadt_hgv back to osm network
osm <- merge(osm, imputed, by = "edgeID", all = TRUE) #attach aadt_mp info on the edges
osm$aadt_hgv_im <- round(osm$aadt_hgv_im, digits = 0) #round to integer

rm(osm_imp, gm_bounds, osm_gm) #remove unnecessary objects from work environment

#############################
#PART 3: Assign negative freight-generating/attracting pois to the osm network
#############################
#PART 3.1: Read all needed datasets and files
#############################
#read GM 10km buffered boundary
gm_bound <- st_read(file.path("01_DataInput/Cityreg_bounds/GreaterManchester/bounds.geojson"))

#read all UK pois
poi_all <- st_read(file.path("01_DataInput/poi/Download_poi_UK_1803987/poi_4138552/poi_4138552.gpkg"))

#read negative freight CLASSIFICATION CODES
poi_negativefreight_codes <- utils::read.csv(file = file.path("01_DataInput/poi/negative_freight.csv"), header = FALSE)
poi_negativefreight_codes[1,1] <- "10570794"
colnames(poi_negativefreight_codes)[1] <- "pointx_class"

#read weights
negative_freight_wgt <- utils::read.csv(file = file.path("01_DataInput/poi/negative_freight_wgt.csv"), header = FALSE)
negative_freight_wgt$V2[is.na(negative_freight_wgt$V2)] <- as.numeric("1")
negative_freight_wgt[1,1] <- "10570794"
colnames(negative_freight_wgt)[1] <- "pointx_class"
colnames(negative_freight_wgt)[2] <- "weight"

#read in OS Geomni UKBuilding polygons
geom_build <- st_read(file.path("01_DataInput/poi/Geomni/ukbuildings_4188614/UKBuildings.shp"))

#############################
#PART 3.2: Preform spatial operations
#############################
#constrain to Greater Manchester region
gm_poi <- st_intersection(poi_all, gm_bound)

#constrain building pol to Greater Manchester region
gm_geom_build <- st_intersection(geom_build, gm_bound) %>% st_cast("POLYGON")

#find UKBuildings polygons containing residnetila use
gm_geom_build_res <- gm_geom_build[gm_geom_build$use == "RESIDENTIAL ONLY",]

#filter negative freight from the POI spatial dataset based on their CLASSIFICATION CODE
gm_poi_negativefreight <- gm_poi[gm_poi$pointx_class %in% poi_negativefreight_codes$pointx_class, ]

#remove POIs found in residences (mixed-use)
int_res <- gm_poi_negativefreight[unlist(st_intersects(gm_geom_build_res, gm_poi_negativefreight)), ]$ref_no %>% unique()
gm_geom_negativefreight_resex <- gm_poi_negativefreight[!gm_poi_negativefreight$ref_no %in% int_res, ]

#add weights to negative freight
gm_poi_negativefreight_resex <- merge(gm_geom_negativefreight_resex, negative_freight_wgt, by = "pointx_class")

#attach negative freight to osm edges
neg_hgv_osm_join <- st_join(gm_poi_negativefreight_resex[,c("pointx_class", "ref_no", "weight")], osm, join=nngeo::st_nn, maxdist = 250, k = 1) #point output
neg_hgv_osm_join$geometry <- NULL

#count negative freight on each osm edge
neg_hgv_count_edgeid <- neg_hgv_osm_join %>% group_by(edgeID, pointx_class, weight) %>% tally() %>% mutate(negpoi_hgv_score = n * weight)
neg_hgv_count_edgeid <- aggregate(. ~ edgeID, data=neg_hgv_count_edgeid[,c("edgeID", "negpoi_hgv_score")], FUN=sum)

#remove unnecessary objects from environment
rm(neg_hgv_osm_join, gm_poi_negativefreight_resex, int_res, gm_poi_negativefreight, gm_geom_build_res, gm_geom_build, gm_poi)
rm(geom_build, negative_freight_wgt, poi_negativefreight_codes, poi_all, gm_bound)

#bring negative freight score to osm network
osm <- merge(osm, neg_hgv_count_edgeid, by = "edgeID", all.x = TRUE)
osm <- osm[,c(1:41, 64:65, 42:63)] #rearange columns to match documentation order
