

library(raster)


{
gpath <- 'E:/Cambridge_Projects/JIBE/Spatial_Data/DSM_DTM_processing/GM_DSMExtended/'

ascfiles <- list.files(path = paste0(gpath),pattern = ".asc$",full.names = TRUE, recursive = TRUE)

raster.list <- lapply(ascfiles, raster)


raster.list$fun <- mean


mos <- do.call(raster::mosaic, raster.list)


raster::writeRaster (mos, paste0(gpath,"Ex_DTM.tif"))

rm (ascfiles, raster.list, mos)

}
