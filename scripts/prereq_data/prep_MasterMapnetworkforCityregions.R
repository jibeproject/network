# Aim: This script bring the master map street network data, and clean it for joining with OSM

library(stringr)
library(sf)
library(dplyr)

#If use Highway network (2017) data, which is upadted based on ITN network use the following code
#mastermap full UK data from Highway network
#mastermapUKgp <- st_read("../bigdata/MasterMaplinksUk/UKmastermapnodesHighwayWidth.gpkg") #read geopackage main file from the GLM
#saveRDS(mastermapUKgp, "../bigdata/MasterMaplinksUk/mastermap_HW_UK.Rds") #covert to RDS for easy access
#mastermap_UK <- readRDS("../bigdata/MasterMaplinksUk/mastermap_HW_UK.Rds")


#If we use ITN data, the following code is needed to clean it before using
#mastermap full UK data from ITN network
mastermap_UK <- readRDS("../bigdata/MasterMaplinksUk/UKmastermaplinks.Rds")

#a function to split text from number in a same column
numextract <- function(string){
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
#test how the function work
#numextract ("12.1m")

#Cleaning the columns with text and numbers
mastermap_UK$averageWidth <- as.numeric (numextract (mastermap_UK$roadWidthA))


#Get City region Boundaries
bounds <- readRDS("../bigdata/boundaries/cityregions/cityregions_England_10kmbuff.Rds")

#subset the regions to do from the master file
bounds <- bounds[bounds$cityregions %in% regions.todo,]

regions <- regions.todo

#create folders for city regions
for(i in seq(from = 1, to = nrow(bounds))){
  region_nm <- as.character(bounds$cityregions[i])
  exists <- file.exists(paste0("../bigdata/MasterMaplinksUk/",region_nm,"/osm-lines.Rds"))
  bounds$exists[i] <- exists
  if(!exists){
    dir.create(paste0("../bigdata/MasterMaplinksUk/",region_nm))
  }
  rm(exists, region_nm)
}
rm(i)

#sub-set the full UK network to city regions to handle it easily
#Loop to extract the road lines from MasterMap for each region
for (reg in 1:length(regions)) {

  region_nm <- as.character(bounds$cityregions[reg])

  print (region_nm)

  region_shp <- bounds [reg,]
  region_shp$region <- region_nm
  region_shp <- region_shp[,"region"]
  region_shp <- st_transform(region_shp, 27700)
  plot(region_shp)

  lines <- mastermap_UK

  lines <- lines[region_shp,]

  lines <- st_cast(lines, "LINESTRING")

  lines <- st_zm(lines, drop = TRUE, what = "ZM")# deop the z dimension of the main file, it cause issues with shapefile

  saveRDS(lines, paste0("../bigdata/MasterMaplinksUk/",region_nm,"/mastermapUK_lines.Rds"))

  rm (reg, lines, region_shp)

}


rm (mastermap_UK)

#Remove some files if necessary
#do.call(file.remove, list(list.files(path = paste0("../bigdata/MasterMaplinksUk/"),pattern = "mastermapUK_lines.Rds$",full.names = TRUE, recursive = TRUE)))
#st_write(mastermapUK_lines, "mastermapGM_lines.shp")


#For other variables, if needed use this code before running the boundary loop
#mastermapUK$elvGaInDir <- as.numeric (numextract (mastermapUK$elevationGainInDirection))
#mastermapUK$elvGaOppDir <- as.numeric (numextract (mastermapUK$elevationGainOppositeDirection))

#re-code some text values in to numbers to join with other network data
# mastermapUK <- mastermapUK %>%
#   dplyr::mutate(direction = recode(directionality, "inOppositeDirection" = 0, "inDirection" = 1, "bothDirections" = 2)) %>%
#   dplyr::mutate(roadtype = recode (formOfWay,
#                                    "Single Carriageway" = 1,
#                                    "Dual Carriageway" = 2,
#                                    "Enclosed Traffic Area" = 3,
#                                    "Guided Busway" = 4,
#                                    "Layby" = 5,
#                                    "Roundabout" = 6,
#                                    "Shared Use Carriageway" = 7,
#                                    "Slip Road" = 8,
#                                    "Track" = 9,
#                                    "Traffic Island Link" = 10,
#                                    "Traffic Island Link At Junction" = 11))


#formOfWay
#Single Carriageway = 1
#Dual Carriageway = 2
#Enclosed Traffic Area = 3
#Guided Busway = 4
#Layby = 5
#Roundabout = 6
#Shared Use Carriageway = 7
#Slip Road = 8
#Track = 9
#Traffic Island Link = 10
#Traffic Island Link At Junction = 11

