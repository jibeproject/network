# Aim: This script bring the master map street network data, and clean it for joining with OSM

library(stringr)
library(sf)
library(dplyr)


#mastermap full UK data from Highway network
mastermapUKgp <- st_read("../bigdata/MasterMaplinksUk/UKmastermapnodesHighway.gpkg")


mastermapUK <- readRDS("../bigdata/MasterMaplinksUk/UKmastermaplinks.Rds")

#Get Boundaries
bounds <- readRDS("../bigdata/boundaries/cityregions/cityregions_England.Rds")

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

  lines <- mastermapUK

  lines <- lines[region_shp,]

  lines <- st_cast(lines, "LINESTRING")

  saveRDS(lines, paste0("../bigdata/MasterMaplinksUk/",region_nm,"/mastermapUK-lines.Rds"))

}

rm (reg)
rm (lines)


######### If we use ITN network we need to clean it a bit#########


#a function to split text from number in a same column
# numextract <- function(string){
#   str_extract(string, "\\-*\\d+\\.*\\d*")
# }

#test how the function work
#numextract ("12.1m")

#mastermap full UK data from ITN network
#mastermapUK <- readRDS("../bigdata/MasterMaplinksUk/UKmastermaplinks.Rds")


#Cleaning the columns with text and numbers
#mastermapUK$widthavg <- as.numeric (numextract (mastermapUK$roadWidthA))
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





