#Aim: Add greenness related variables to OSM

#Additional library
#remotes::install_git("https://github.com/STBrinkmann/GVI") #in not installed
library(GVI)
library(terra)
library(sfheaders)
#library(multidplyr)

#Main code
regions <- regions.todo

#create directory
if(!dir.exists(paste0("../bigdata/osm-greenness"))){
  dir.create(paste0("../bigdata/osm-greenness"))
}


for(a in 1:length(regions)){

  print (a)

  if(file.exists(paste0("../bigdata/Greenness/NDVI/",regions[a],"/CR_NDVI.tif"))){ #Check for input file
    if(file.exists(paste0("../bigdata/osm-greenness/",regions[a],"/osm_greenness.Rds"))){ #check for existing copy of output
      message(paste0("Skipping Greenness modelling for ", regions[a]," as already done"))

    }
    else{
      #create directory
      if(!dir.exists(paste0("../bigdata/osm-greenness/",regions[a]))){
        dir.create(paste0("../bigdata/osm-greenness/",regions[a]))
      }

      message(paste0(Sys.time(), ": Estimating NDVI for each link of OSM for ",regions[a]))

      osm <- readRDS(paste0("../bigdata/osm-slops-DEM/",regions[a],"/osm_slope.Rds"))

      NDVI <- raster (paste0("../bigdata/Greenness/NDVI/",regions[a],"/CR_NDVI.tif"))

      #make the less then 0.1 NDVI values zero to remove water, building and bare land pixels
      NDVI[NDVI < 0.1] <- 0

      #Varying buffer distance for different road based on their width. Here we devide the road width by 2, and then add a 20 m buffer distance to that
      osm$bufferdist <- ((osm$averageWidth/2)) + 20

      #Actual buffer polygon
      osmBuffer <- st_buffer(osm, dist = osm$bufferdist)


      #QGIS zonal statistics is much quicker than base r functions of raster or terra package as it works ob native C++
      #if possible the code should run QGIS based algorithm
      NDVIzonal <- qgis_run_algorithm(
        "native:zonalstatisticsfb",
        INPUT = osmBuffer,
        INPUT_RASTER = NDVI,
        RASTER_BAND = 1,
        STATISTICS = 2, #for mean = 2
        COLUMN_PREFIX = 'NDVI'
      )

      NDVIzonal_sf <- sf::read_sf(qgis_output(NDVIzonal, "OUTPUT"))

      #Extract the mean value of NDVI for each polygon using terra package, same work as QGIS process above just a lot slow!
      #NDVIzonal <- terra::extract (NDVI, osmBuffer, fun=mean, na.rm=TRUE)

      NDVIzonal_df <- NDVIzonal_sf %>% dplyr::select (edgeID, NDVImean) %>% st_drop_geometry() %>% as.data.frame()
      
      rm (NDVI, NDVIzonal_df, NDVIzonal_sf, NDVIzonal)

      osm <- left_join(osm,NDVIzonal_df, by = 'edgeID')

    }

  }


  #For canopy coverage
  if(file.exists(paste0("../bigdata/Greenness/Canopy/",regions[a],"/Canopy.Rds"))){
    #Check for input file

    message(paste0(Sys.time(), ": Estimating Canopy coverage for each link of OSM for ",regions[a]))

    canopy <- readRDS(paste0("../bigdata/Greenness/Canopy/",regions[a],"/Canopy.Rds"))

    canopycentroid <- st_centroid (canopy)

    osmBuffer$buffarea <- st_area(osmBuffer)


    canopy_join_osm <- qgis_run_algorithm(
      "qgis:joinbylocationsummary",
      DISCARD_NONMATCHING = F,
      INPUT = osmBuffer,
      JOIN = canopycentroid,
      SUMMARIES = 5, # 10 is for majority, 7 for median, 5 for sum
      JOIN_FIELDS = 'AREA'#the column to join the attribute
    )

    canopy_join_osm_sf <- sf::read_sf(qgis_output(canopy_join_osm, "OUTPUT"))

    canopy_coverage_osm <- canopy_join_osm_sf %>%
      st_drop_geometry() %>%
      dplyr::select(edgeID, AREA_sum, buffarea) %>%
      rename(canoarea = AREA_sum) %>%
      mutate (cancoverage = (canoarea /buffarea) * 100) %>%
      mutate (cancoverage = ifelse(cancoverage > 100, 100, cancoverage)) %>%
      dplyr::select(edgeID, cancoverage)

    #Slow process using sf and dplyr, takes ages to run!
    # canopy_coverage_osm <- osmBuffer %>%
    #   st_join(canopycentroid) %>%
    #   group_by(edgeID) %>%
    #   summarize(canopysum = sum(AREA),
    #             buffer_area = unique(area),
    #             canopycoverage = (canopysum /buffer_area) * 100)

    osm <- left_join(osm, canopy_coverage_osm, by = 'edgeID')
    
    rm(canopy, canopycentroid, canopy_join_osm, canopy_join_osm_sf, canopy_coverage_osm)

    #saveRDS(osm_Greenness_added, paste0("../bigdata/osm-greenness/",regions[a],"/osm_greenness.Rds"))

    }
    else{

      message(paste0(Sys.time(), ": Canopy data is not available for ",regions[a]))

    }


  #For Eye-level greenness visibility
  if(file.exists(paste0("../bigdata/Greenness/Canopy/",regions[a],"/CR_DSM.tif"))){
    #Check for input file

    message(paste0(Sys.time(), ": Estimating Greenness Visibility for each link of OSM for ",regions[a]))


    #Input greenness and DEM data
    DSM <- rast(paste0("../bigdata/dem/",regions[a],"/CR_DSMextended.tif"))

    DEM <- rast(paste0("../bigdata/dem/",regions[a],"/CR_DEM_F.tif"))

    GreenSpace <- rast(paste0("../bigdata/Greenness/GreenNoGreen/",regions[a],"/GreenNoGreenRes.tif"))

    #osm <- readRDS(paste0("../bigdata/osm-slops-DEM/",regions[a],"/osm_slope.Rds"))

    #pointsonedge <- st_line_sample (osm_Greenness_added,density = 1/20, n = 1, type = "regular", sample = NULL) #each 20m, at least 1 point

    #Take point sample on master map at certain distance and start/end offset
    pointsonedge <- qgis_run_algorithm(
      "native:pointsalonglines",
      DISTANCE = 20, #map unit distance, here it is meters
      END_OFFSET = 5,
      INPUT = osm,#change the input here
      START_OFFSET = 5
    )


    #convert the temporary point samples to sf object
    pointsonedge_sf <- sf::read_sf(qgis_output(pointsonedge, "OUTPUT"))

    #test using sample points
    #pointsonedge_sf_smaple <- sample_n (pointsonedge_sf, 100)

    #point_VGVIs <- vgvi_from_sf(observer = pointsonedge_sf,
                          #dsm_rast = DSM, dtm_rast = DTM, greenspace_rast = Green,
                          #max_distance = 50, observer_height = 1.7,
                          #m = 1, b = 3, mode = "exponential", cores = 3, progress = T)

    pVGVI <- vgvi_from_sf(observer = pointsonedge_sf,
                 dsm_rast = DSM, dtm_rast = DEM, greenspace_rast = GreenSpace,
                 max_distance = 300, observer_height = 1.7,
                 m = 0.5, b = 8, mode = "logit", cores = 3, progress = T)

    #st_write (pVGVI,"pointsVGVI300m.gpkg")
    
    rm (DEM, DSM, Greenspace, pointsonedge, pointsonedge_sf)

    pVGVIbuffer <- st_buffer(pVGVI, dist = 0.5)
    
    rm (pVGVI)

    joinVGVIlines <- qgis_run_algorithm(
      "qgis:joinbylocationsummary",
      DISCARD_NONMATCHING = F,
      INPUT = osm,
      JOIN = pVGVIbuffer,
      SUMMARIES = "mean", # 10 is for majority, 7 for median
      JOIN_FIELDS = 'VGVI' #the column to join the attribute
    )

    joinVGVIlines_sf <- sf::read_sf(qgis_output(joinVGVIlines, "OUTPUT"))
    
    joinVGVIlines_sf_osm <- joinVGVIlines_sf %>%
      st_drop_geometry() %>%
      dplyr::select(edgeID, VGVI_mean)
    
    osm <- left_join(osm, joinVGVIlines_sf_osm, by = 'edgeID')

    saveRDS(osm,paste0("../bigdata/osm-greenness/",regions[a],"/osm_greenness.Rds"))
    
    st_write(osm,paste0("../bigdata/osm-greenness/",regions[a],"/osm_greenness.gpkg"))
    
    rm (joinVGVIlines, joinVGVIlines_sf, joinVGVIlines_sf_osm, pVGVIbuffer)

  }
  else{

    message(paste0(Sys.time(), ": Evey level data is not available for ",regions[a]))

  }



}

# st_write(osm_NDVI_added,paste0("../bigdata/osm-greenness/",regions[a],"/osm_NDVI_added.gpkg"))
#
# canopy <- st_read(paste0("../bigdata/Greenness/Canopy/",regions[a],"/Canopy_Simplify_mergedGM_2010.shp"))
#
# saveRDS(canopy,paste0("../bigdata/Greenness/Canopy/",regions[a],"/Canopy.Rds"))
#
#
#st_write(osm_canopy_added,paste0("../bigdata/osm-greenness/",regions[a],"/osm_canopy_added.gpkg"))


