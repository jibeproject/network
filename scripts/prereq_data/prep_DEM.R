

library(raster)


gpath <- 'C:/Users/S M Labib/Desktop/JIBE/Spatial_Data/DSM_DTM_processing/GM_DTMExtended/'

ascfiles <- list.files(path = paste0(gpath),pattern = ".asc$",full.names = TRUE, recursive = TRUE)

raster.list <- lapply(ascfiles, raster)


raster.list$fun <- mean


mos <- do.call(mosaic, raster.list)


writeRaster (mos, paste0(gpath,"Ex_DTM.tif"))
