#SET UP
rm(list = ls())

library(httr)
library(sf)
library(dplyr)

if (!require("qgisprocess")) install.packages("qgisprocess")
install.packages("qgisprocess", dependencies = TRUE)

#"https://api.cyclestreets.net/v2/advocacydata.modalfilters?bbox=-1.3278,53.804,-3.1008,53.2022&key=0275873928ca6633" big boundary

####################
#PART 1: get modal filters from CycleStreets API
####################
#Create CycleStreets API call-- key must be personalized and bbox must be set-- call restriction 2000 features (enough for one city region unless Greater London)
api_call <- "https://api.cyclestreets.net/v2/advocacydata.modalfilters?bbox=-1.3278,53.804,-3.1008,53.2022&key=0275873928ca6633"
httrreq <- httr::GET(api_call)
txt <- httr::content(httrreq, as = "text", encoding = "UTF-8")
obj <- jsonlite::fromJSON(txt)

obj$features$properties
unlist(obj$features$geometry$coordinates[5])[2] #lat
unlist(obj$features$geometry$coordinates[5])[1] #lon

#Extract modal filters from api call
df <- data.frame(osm_id = as.numeric(), modalfil = as.character(), type = as.character(), lat = as.numeric(), lon = as.numeric())
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

for (i in 1:nrow(obj$features$properties)){
  osm_id <- obj$features$properties$osmId[i]
  modalfil <- obj$features$properties$modalfilter[i]
  type <- obj$features$properties$name[1]
  lat <- unlist(obj$features$geometry$coordinates[i])[2] #lat
  lon <- unlist(obj$features$geometry$coordinates[i])[1] #lon
  df_new <- data.frame(osm_id = osm_id, modalfil = modalfil, type = type, lat = lat, lon = lon)
  df <- rbind(df, df_new)
}
mf <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = projcrs) %>% sf::st_transform(27700) #project and transform to OSGB 1936 British Natioanl Grid

####################
#PART 2: read osm dataset and GM boundary for clipping
####################
region_nm <- as.character("GreaterManchester")
#clip modal filters to GM
gm_bound <- st_read(file.path("./GitHub_inputfiles_network/bounds.geojson"))#in the Teams folder WP2>Data_WP2>Processed_Data>Greater Manchester>GitHub_inputfiles_network
mf_gm <- st_intersection(mf, gm_bound)
#get osm network
osm <- readRDS(file.path("./bigdata/speed/",region_nm,"/osm_speed.Rds"))

#snap to osm network
snap <- qgis_run_algorithm(
  "native:snapgeometries",
  BEHAVIOR = 3, #Prefer closest point, don't insert new vertices
  INPUT = mf_gm[,"modalfil"],
  REFERENCE_LAYER = osm,
  TOLERANCE = 5
)
snap_sf <- sf::read_sf(qgis_output(snap, "OUTPUT"))
snapbuffer <- st_buffer(snap_sf, dist = 0.05)

osm_mf <- st_intersection(osm, snapbuffer)
unique(osm_mf$modalfil)
####################
#PART 3: add permited modes
####################

#modal filters type:                                                                                                        JIBE
#bollards/gate      within a street, that prevent through-traffic but allow walking and cycling;                          |   cyc/walk
#gap                within a street that form a short section of cycleway/walkway, similarly preventing through-traffic;  |   cyc/walk
#cycleway filter    no entry except cycles filters;                                                                       |   cyc
#bus gate           which allow bus and cycle access to particular areas;                                                 |   bus/cyc
#streets abutting    (next to or have a common boundary with) main roads only a cycleway/pavement between them;           |   all
#no modal filter                                                                                                          |   all

#replace nominal with Likert score
osm_mf$modalfil <- ifelse(stringr::str_detect(osm_mf$modalfil, "bollard") == TRUE |
                            stringr::str_detect(osm_mf$modalfil, "^gate$") == TRUE |
                            stringr::str_detect(osm_mf$modalfil, "gap") == TRUE,
                            as.character("cyc/walk"), osm_mf$modalfil)
osm_mf$modalfil <- ifelse(stringr::str_detect(osm_mf$modalfil, "cycleway filter") == TRUE, as.character("cyc"), osm_mf$modalfil)
osm_mf$modalfil <- ifelse(stringr::str_detect(osm_mf$modalfil, "bus gate") == TRUE, as.character("bus/cyc"), osm_mf$modalfil)
osm_mf$modalfil <- ifelse(stringr::str_detect(osm_mf$modalfil, "streets abutting") == TRUE, as.character("all"), osm_mf$modalfil)

#count mf on the edges
mf_edgeid <- osm_mf %>% group_by(edgeID, modalfil) %>% tally() %>% ungroup() %>% mutate(mf_cnt=n) %>% select(-n) %>% st_drop_geometry()#count mf on edgeIDs

#get modal filter ratio
osm <- merge(osm, mf_edgeid, by = "edgeID", all = TRUE) #attach modal filter counts info on the edges
osm$modalfil <- ifelse(is.na(osm$modalfil), as.character("all"), osm$modalfil)
osm$mf_cnt <- ifelse(is.na(osm$mf_cnt), as.numeric(0), osm$mf_cnt)
