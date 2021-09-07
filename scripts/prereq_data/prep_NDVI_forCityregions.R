# Process Satellite NDVI data for each city region

library(stringr)
library(sf)
library(dplyr)
library(raster)

bounds <- readRDS("../bigdata/boundaries/cityregions/cityregions_England_10kmbuff.Rds")

#subset the regions to do from the master file
bounds <- bounds[bounds$cityregions %in% regions.todo,]

regions <- regions.todo

#create folders for city regions
for(i in seq(from = 1, to = nrow(bounds))){
  region_nm <- as.character(bounds$cityregions[i])
  exists <- file.exists(paste0("../bigdata/NDVI/",region_nm,"/Sentinel2_NDVI.tif"))
  bounds$exists[i] <- exists
  if(!exists){
    dir.create(paste0("../bigdata/NDVI/",region_nm))
  }
  rm(exists, region_nm)
}
rm(i)


####### Import the NDVI for full UK and crop to the bounding box of the city regions#####

NDVI_UK <- raster('C:/Users/S M Labib/Desktop/JIBE/Spatial_Data/NDVI_UK_2020_April_Oct/NDVI_UK_2020_S2.tif') #the location of the NDVI file

for (reg in 1:length(regions)) {

  region_nm <- as.character(bounds$cityregions[reg])

  print (region_nm)

  region_shp <- bounds [reg,]
  region_shp$region <- region_nm
  region_shp <- region_shp[,"region"]
  region_shp <- st_transform(region_shp, 27700)
  plot(region_shp)

  extentcr <- extent (region_shp)

  region_NDVI <- crop(NDVI_UK, extentcr)


  writeRaster (region_NDVI, paste0("../bigdata/NDVI/",region_nm,"/CR_NDVI.tif"))

  rm (reg, region_shp, extentcr, region_NDVI)

}

rm (NDVI_UK)




######### Google Earth Engine Javascript code to download the NDVI layer for whole UK ########
#Draw the geometry
# var geometry =
#   /* color: #d63000 */
#   /* displayProperties: [
#     {
#       "type": "rectangle"
#     }
#   ] */
#   ee.Geometry.Polygon(
#     [[[-5.456387599483761, 57.157106463391],
#       [-5.456387599483761, 50.33758087671848],
#       [1.5480832257115518, 50.33758087671848],
#       [1.5480832257115518, 57.157106463391]]], null, false),
# imageVisParam = {"opacity":0.01,"gamma":0.1},
# imageVisParam2 = {"opacity":0.01,"gamma":0.1};

# // Selecting the image collection
#
# var collection = ee.ImageCollection('COPERNICUS/S2') // searches all sentinel 2 imagery pixels...
# .filter(ee.Filter.lt("CLOUDY_PIXEL_PERCENTAGE", 5)) // filters based on the metadata for pixels less than 10% cloud
# .filterDate('2020-04-01' ,'2020-10-31') //selecting pixels between the dates
# .filterBounds(geometry); // study area
#
# // print the selected images JSON list including the metadata
# print(collection);
#
# /// To get a nice blended-looking mosaic,
# // try some of the tools for 'reducing' these to one pixel (or bands of pixels in a layer stack).
# // find the median value of all the pixels which meet the criteria.
# var medianpixels = collection.median();
#
# var medianpixelsclipped = medianpixels.clip(geometry).divide(10000); // this cuts up the result so that it fits neatly into your aoi
# // and divides so that values between 0 and 1
#
# //visualisation parameters
# var visParams_ndvi = {min: -0.2, max: 0.8, palette: 'FFFFFF, CE7E45, DF923D, F1B555, FCD163, 99B718, 74A901, 66A000, 529400,' +
#     '3E8601, 207401, 056201, 004C00, 023B01, 012E01, 011D01, 011301'};
#
#
# // Calculate NDVI
# var image_ndvi = medianpixelsclipped.normalizedDifference(['B8','B4']);
#
#
# Map.centerObject(geometry, 9);
#
# // Map results
# Map.addLayer(image_ndvi,visParams_ndvi,'Sentinel-2 NDVI');
#
#
# // Now visualise the mosaic as a false colour image.
# Map.addLayer(medianpixelsclipped, {bands: [ 'B8', 'B4', 'B3'], min: 0, max: 1, gamma: 1.5}, 'Sentinel_2 mosaic');
#
#
#
# // export it to your googledrive as a tiff for use in QGIS
# // Export the image, specifying scale and region.
# Export.image.toDrive({
#   image: image_ndvi,
#   description: 'S2_UK_NDVI2015_April_Oct_2020',
#   scale: 10,
#   maxPixels: 9e12,
#   fileFormat: 'GeoTIFF',
#   region: geometry
# });










