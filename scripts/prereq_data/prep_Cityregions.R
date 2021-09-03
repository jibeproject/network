# PRep the city regions
library(sf)
library(stringr)
library(dplyr)

bounds <- st_read("../bigdata/boundaries/cityregions/11_UK_Urban_regions.shp")
bounds$cityregions <- as.character(bounds$name)
bounds <- st_transform(bounds, 27700)
bounds <- st_simplify(bounds, dTolerance = 10)

boundsbuff10km <- st_buffer(bounds, 10000)

#check the buffer
#mapview::mapview(bounds["name"]) + mapview::mapview(boundsbuff10km, zcol = "cityregions")


#original bounary
bounds$cityregions <- as.character(bounds$cityregions)
bounds$cityregions <- str_replace_all(bounds$cityregions,"[[:punct:]]","") #check for replacing punctuation
bounds$cityregions <- str_replace_all(bounds$cityregions," ","") #replace space

bounds <- bounds[order(bounds$cityregions),]

bounds <- st_transform(bounds, 4326)

saveRDS(bounds,"../bigdata/boundaries/cityregions/cityregions_England.Rds")

#10 km buffer to take account of the edge effect and extent the network
boundsbuff10km$cityregions <- as.character(boundsbuff10km$cityregions)
boundsbuff10km$cityregions <- str_replace_all(boundsbuff10km$cityregions,"[[:punct:]]","") #check for replacing punctuation
boundsbuff10km$cityregions <- str_replace_all(boundsbuff10km$cityregions," ","") #replace space

boundsbuff10km <- boundsbuff10km[order(boundsbuff10km$cityregions),]

boundsbuff10km <- st_transform(boundsbuff10km, 4326)

saveRDS(boundsbuff10km,"../bigdata/boundaries/cityregions/cityregions_England_10kmbuff.Rds")


#st_write (boundsbuff10km,"cityregions_England10up.shp")


bounds <- bounds[bounds$cityregions %in% regions.todo,]

regions <- regions.todo

#create folders for city regions
for(i in seq(from = 1, to = nrow(bounds))){
  region_nm <- as.character(bounds$cityregions[i])
  exists <- file.exists(paste0("../bigdata/boundaries/cityregions/",region_nm,"/bounds.geojson"))
  bounds$exists[i] <- exists
  if(!exists){
    dir.create(paste0("../bigdata/boundaries/cityregions/",region_nm))
  }
  rm(exists, region_nm)
}
rm(i)

for (reg in 1:length(regions)) {

  region_nm <- as.character(bounds$cityregions[reg])

  print (region_nm)

  region_shp <- bounds [reg,]
  region_shp$region <- region_nm
  region_shp <- region_shp[,"region"]
  region_shp <- st_transform(region_shp, 27700)
  plot(region_shp)

  st_write(region_shp, dsn = paste0("../bigdata/boundaries/cityregions/",region_nm,"/bounds.geojson"))

}
