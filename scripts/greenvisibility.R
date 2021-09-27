
library(terra)
library(GVI)
library(sf)
library(sfheaders)
library(rgdal)
library(tidyverse)
library(multidplyr)
#remotes::install_git("https://github.com/STBrinkmann/GVI")


#bring the network
#network_edge_small <- st_read ("C:/Users/S M Labib/Desktop/JIBE/jibeproject/bigdata/testdata/network-edges-small.shp")
#network_small_edgesf <- st_cast(network_edge_small, "LINESTRING")
#network_small_edgesfid <- network_small_edgesf %>% select(edgeID)

network_edge <- readRDS("C:/Users/S M Labib/Desktop/JIBE/jibeproject/bigdata/network-clean/GreaterManchester/network-edges.Rds")
network_edgesf <- st_as_sf(network_edge)

pointsonedge <- st_line_sample (network_edgesf,density = 1/20, n = 1, type = "regular", sample = NULL) #each 20m, at least 1 point
#pointson <- st_sf(osm_id =as.data.frame(network_small_edgesfid)["osm_id"], pointson)
pointson <- st_sf(as.data.frame(network_edgesf), pointsonedge)
pointson <- st_cast(pointson, "POINT")
pointson <- st_as_sf(pointson)



DSM <- rast("C:/Users/S M Labib/Desktop/JIBE/jibeproject/bigdata/dem/GMDSM_5m.tif")

DTM <- rast("C:/Users/S M Labib/Desktop/JIBE/jibeproject/bigdata/dem/GMDTM_5m.tif")

Green <- rast("C:/Users/S M Labib/Desktop/JIBE/jibeproject/bigdata/dem/GreenNoGreen_5m.tif")

cVGVI <- vgvi_from_sf(observer = pointson,
                          dsm_rast = DSM, dtm_rast = DTM, greenspace_rast = Green,
                          max_distance = 500, observer_height = 1.7,
                          m = 1, b = 3, mode = "exponential", cores = 8, progress = T)

mapview::mapview(pointson["osm_id"])

#cVGVI <- GVIline

GVIbuff <- st_buffer(cVGVI, dist = 1)

networkVGVI <- st_join (network_edgesf, GVIbuff)

#networkVGVI %>%
  #st_drop_geometry() %>%
  #distinct(edgeID) %>%
  #count()

cluster <- new_cluster(6)
cluster

networkVGVIedgescldf <- networkVGVI %>%
  dplyr::group_by(edgeID) %>% partition(cluster)

networkVGVIedges <- networkVGVIedgescldf %>%
  dplyr::summarise(mean(VGVI)) %>%
  collect()


networkVGVIedgesfulldata <- networkVGVIedges %>%
  rename (VGVI = `mean(VGVI)`) %>%
  left_join(network_edgesf, by = "edgeID")

networkVGVIedgesfulldatasf <- st_as_sf(networkVGVIedgesfulldata)

mapview::mapview(networkVGVIedgesfulldatasf["VGVI"])


st_write(networkVGVIedgesfulldatasf, paste0(path, "testdata", "/", "networkVGVIedgesfulldata.shp") , driver="ESRI Shapefile")
