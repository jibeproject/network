#rm(list = ls())

# if (!require("sf")) install.packages("sf")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("qgisprocess")) install.packages("qgisprocess")
# install.packages("qgisprocess", dependencies = TRUE)

##################################
#Attribute values:
##################################

#RouteStatus: 1 (idea/concept) - 2 (potential route) - 3 (in use)
#RouteSurface: 0 (NA) - 1 (unpaved/mountain biking) - 2 (unpaved/muddy when rain) - 3 (all-weather)
#NCNRoute: 1 (not part) - 2 (route part of the National NCN) - 3 (route part of the Regional)
# JIBE
#4 = off road path;
#3 = protected lane/ segregated lanes;
#2 = painted lanes;
#1 = Integrated lanes
#0 = Cycling link but have to walk (Tfgm 9)
#TfGM
# 2 = on-road Advisory Routes*. Routes along roads with directions signs as minimum level of infrastructure. a.k.a. recommended signed routes
# 3 = on-road routes with cycling facilities, e.g. cycle lanes and / or advanced stop lines at signals
# 4 = segregated cycle lanes / Shared Use Footways adjacent to the carriageway
# 5 = traffic-free routes, e.g. converted railway line
# 6 = canal towpaths - cycling permitted
# 7 = canal towpaths – cycling permitted with permit
# 9 = footpaths - cycling prohibited (e.g. footbridges across motorways). Use of these can however prevent long detours
#11 = on-road route – with physical segregation e.g. armadillos

# *Advisory cycle lanes are marked with a broken white line to TSRGD diagram 1004 and should not be entered by other vehicles unless it
#is unavoidable.(DfT, 2020, pp.61-62)

#####################
#PART 1: synchronize TfGM cycling infrastructure to JIBE categories
#####################
# JIBE                                            #TfGM
#4 = off road path;                               5; 6; 7
#3 = protected lane/ segregated lanes;            4; 11
#2 = painted lanes;                               3
#1 = Integrated lanes                             2
#0 = Cycling link but have to walk (Tfgm 9)       9

#read-in networks data
#osm <- st_read(file.path("01_DataInput/Network/GreaterManchester/network-addedinfo.geojson"))

osm <- readRDS(paste0("../bigdata/network-clean/","GreaterManchester","/network_edges.Rds"))
cycle <-  st_read(file.path("../network/01_DataInput/Network/GM_cycle/2018/SHP-format/2018/Cycle_Routes_polyline.shp"))

#keep only existing infra -3; from 3306 total of 75 removed - 1 (idea/concept) - 2 (potential route) --3231 final
#cycle <- cycle[cycle$RouteStatu == 3, ] these tags are outdated (most routes exist)

#sync categories
cycle$RouteType <- cycle$RouteType %>% gsub(9, 0, .) %>% gsub(2, 1, .) %>% gsub(3, 2, .)
cycle$RouteType <- gsub(4, 3, gsub(11, 3, cycle$RouteType))
cycle$RouteType <- gsub(5, 4, gsub(6, 4, gsub(7, 4, cycle$RouteType)))
cycle$RouteType <- as.numeric(cycle$RouteType) # convert character to numeric

#####################
#PART 2: Create sample points on lines
#####################
#Density 1/30 meaning one in 30 m interval, 'n' argument is redundant; overwrites 'density' argument (n = indicate at least one point even if the line is less than 20m)
cycle <- cycle %>% st_cast("LINESTRING")
cycle_points <- st_line_sample (cycle, density = 1/30, type = "regular", sample = NULL)
cycle_points <- st_sf(as.data.frame(cycle), cycle_points) %>% st_cast("POINT") #add attributes of the lines to points; issues with multi-point, so cast to point

#Check in map
#mapview::mapview(cycle_points ["RouteType"])
#st_write(cycle_points, file.path(paste0("02_DataOutput/network/gm/edges")), "cycle_points30m.shp", driver="ESRI Shapefile") #write for visual inspection in QGIS
#muddy routes are included-- consider excluding?

#####################
#PART 2.1: linking networks info (done only for Greater Manchester using TfGM Cycling Network Dataset)
#####################

#snap cycling points to OSM edges
snap <- qgis_run_algorithm(
  "native:snapgeometries",
  BEHAVIOR = 1, #prefer closest points insert extra vertices where needed
  INPUT = cycle_points,
  REFERENCE_LAYER = osm,
  TOLERANCE = 50
)

#convert the temp output to sf object
snap_sf <- sf::read_sf(qgis_output(snap, "OUTPUT"))

snapbuffer <- st_buffer(snap_sf, dist = 0.05)

#mapview::mapview(snapbuffer ["RouteType"])

#add cycling info to edges
joininfo <- qgis_run_algorithm(
  "qgis:joinbylocationsummary",
  DISCARD_NONMATCHING = F,
  PREDICATE = 0, # 0 is equal to intersect
  INPUT = osm,
  JOIN = snapbuffer,
  SUMMARIES = 10, # 10 is for majority, 7 for median
  JOIN_FIELDS =  c('RouteType', 'RouteSurfa')
)

#covert to sf object
osmcycle_sf <- sf::read_sf(qgis_output(joininfo, "OUTPUT"))
#note, what every summarise opetion section in joining that would be a prefix of the new joined column
#Such as, if majority, for "RouteType" the new field would be "RouteType_majority"


#If some part of a single road is missing the joined information we check the OSM ID and see if there is a cycle infrastrcture type available for other parts
# If same OSM ID road segments have missing information we replace the null values with values from other road segments with same OSM ID
osmcycle_sf <- osmcycle_sf %>%
  group_by(osm_id) %>% #group by OSM ID
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(roadtype, "Cycleway") == TRUE, median(RouteType_majority[!is.na(RouteType_majority)]), NA)) %>%
  dplyr::mutate(cycleosm = replace(cycleosm, cycleosm == 0.5, 0)) %>%
  dplyr::mutate(cycleosm = replace(cycleosm, cycleosm == 1.5, 1)) %>%
  dplyr::mutate(cycleosm = replace(cycleosm, cycleosm == 2.5, 2)) %>%
  dplyr::mutate(cycleosm = replace(cycleosm, cycleosm == 3.5, 3)) %>%
  dplyr::mutate(cycleosm = ifelse(roadtype == "motorway - Cycling Forbidden" | roadtype == "motorway_link - Cycling Forbidden", NA, cycleosm))

osmcycle_sf <- osmcycle_sf %>% dplyr::mutate(cycleinfra = ifelse(!is.na(RouteType_majority), RouteType_majority, cycleosm))

#adding OSM cycling infra to complement TfGM Cycling Infra
# JIBE                                          #osm$bicycle   #osm$roadtype             #osm$cycleway.left/OSM$cycleway.right    #osm$highway
#4 = off road path;                                             "Segregated Cycleway"     "track"
#3 = protected lane/ segregated lanes;                                                                                             "cycleway"
#2 = painted lanes;                             "designated"
#1 = Integrated lanes                                                                     "share_busway"
#0 = Cycling link but have to walk (Tfgm 9)

osmcycle_sf <- osmcycle_sf %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(cycleway.left, "lane") == TRUE | stringr::str_detect(cycleway.right, "lane") == TRUE, as.numeric(1), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(cycleway.left, "share_busway") == TRUE | stringr::str_detect(cycleway.right, "share_busway") == TRUE, as.numeric(1), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(bicycle, "designated") == TRUE, as.numeric(2), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(highway, "cycleway") == TRUE, as.numeric(3), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(cycleway.left, "track") == TRUE | stringr::str_detect(cycleway.right, "track") == TRUE, as.numeric(4), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(roadtype, "Segregated Cycleway") == TRUE, as.numeric(4), cycleosm)) %>%
  select(-cycleinfra)

#save for next stages
saveRDS(osmcycle_sf, paste0("../bigdata/network-addedinfo/","GreaterManchester","/network_added_edges.Rds"))

#write for GIS viewing
st_write(osmcycle_sf, paste0("../bigdata/network-addedinfo/","GreaterManchester","/", "network_added_edges.geojson"), overwrite = TRUE)

#remove unnecessary files
rm (osm, cycle, cycle_points, snap, snap_sf, snapbuffer, joininfo, osmcycle_sf)

#####################
#PART 2.2: linking networks info (done for the remaining 10 city regions using OSM 'roadtype' and 'bicycle' attributes)
#####################
osm <- osm %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(cycleway.left, "lane") == TRUE | stringr::str_detect(cycleway.right, "lane") == TRUE, as.numeric(1), NA)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(cycleway.left, "share_busway") == TRUE | stringr::str_detect(cycleway.right, "share_busway") == TRUE, as.numeric(1), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(bicycle, "designated") == TRUE, as.numeric(2), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(highway, "cycleway") == TRUE, as.numeric(3), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(cycleway.left, "track") == TRUE | stringr::str_detect(cycleway.right, "track") == TRUE, as.numeric(4), cycleosm)) %>%
  dplyr::mutate(cycleosm = ifelse(stringr::str_detect(roadtype, "Segregated Cycleway") == TRUE, as.numeric(4), cycleosm))

#save for next stages
saveRDS(osm, paste0("../bigdata/network-addedinfo/",region,"/network_added_edges.Rds"))
rm(osm)
