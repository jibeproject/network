#SET UP
rm(list = ls())

if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")
if (!require("qgisprocess")) install.packages("qgisprocess")
install.packages("qgisprocess", dependencies = TRUE)
qgis_show_help("native:snapgeometries")

####################
#PART 1: get road network and crime data
####################
region_nm <- as.character("GreaterManchester")

#read network
osm <- readRDS(file.path("./bigdata/hgv_negative/",region_nm,"/osm_hgv_negative.Rds"))

crime1 <- st_read(file.path("./bigdata/crimereports/2019-01-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700) # ALL files found on GitHub in the bigdata folder
crime2 <- st_read(file.path("./bigdata/crimereports/2019-02-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700)
crime3 <- st_read(file.path("./bigdata/crimereports/2019-03-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700)
crime4 <- st_read(file.path("./bigdata/crimereports/2019-04-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700)
crime5 <- st_read(file.path("./bigdata/crimereports/2019-05-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700)
crime6 <- st_read(file.path("../bigdata/crimereports/2019-06-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700)

crime7 <- st_read(file.path("./bigdata/crimereports/2018-10-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700)
crime8 <- st_read(file.path("./bigdata/crimereports/2018-11-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700)
crime9 <- st_read(file.path("./bigdata/crimereports/2018-12-greater-manchester-street.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude")) %>% st_set_crs(4326) %>% st_transform(27700)

crime_all <- rbind(crime1[, c("Month", "LSOA.code")], crime2[, c("Month", "LSOA.code")], crime3[, c("Month", "LSOA.code")],
                   crime4[, c("Month", "LSOA.code")], crime5[, c("Month", "LSOA.code")], crime6[, c("Month", "LSOA.code")],
                   crime7[, c("Month", "LSOA.code")], crime8[, c("Month", "LSOA.code")], crime9[, c("Month", "LSOA.code")])

snap <- qgis_run_algorithm(
  "native:snapgeometries",
  BEHAVIOR = 3, #Prefer closest point, don't insert new vertices
  INPUT = crime_all,
  REFERENCE_LAYER = osm,
  TOLERANCE = 15
)
snap_sf <- sf::read_sf(qgis_output(snap, "OUTPUT"))
snapbuffer <- st_buffer(snap_sf, dist = 0.05)

osm_crime <- st_intersection(osm, snapbuffer)
osm_crime <- osm_crime %>% group_by(edgeID) %>% tally() %>% ungroup() %>% mutate(crim_cnt=n) %>% select(-n) %>% st_drop_geometry()#get edgeIDs for crime pnt

osm <- merge(osm, osm_crime, by = "edgeID", all = TRUE) #attach crime counts info on the edges

osm$crim_cnt <- ifelse(is.na(osm$crim_cnt), as.numeric(0), osm$crim_cnt)

#save output
dir.create(paste0("./bigdata/crime"))
saveRDS(osm, paste0("./bigdata/crime/",region_nm,"/osm_crime.Rds"))
rm(osm)
