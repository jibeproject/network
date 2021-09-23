#Master Control Script for JIBE network data creation
# Will load in an run all necessary scripts

#This script assumes that you have certain pre-existing dataset in the Bigdata repo
#in the right places that have been pre-formatted for jibeproject

rm(list=ls())

###########################################
#Settings

#Regions are selected using the file  ../network/01_DataInput/RegionsToD.csv
#To do a region just put y in the do column of this csv file

skip <- F #Should the code skip regions that have already been done?
overwrite <- TRUE #Some stages overwrite existing files, for example by adding an extra column of data
                   #Note that not overwriting may cause later stages to fail if they expect earlier stages
                   #results to be in the starting file
ncores <- 7 #Some functions use parallel processing how many clusters should be run?
verbose <- TRUE #Get extra messages and information
all.regions <- F #Ignore the regions to do file and do all regions


##########################################

library(sf)
library(osmdata)
library(stringr)
library(dplyr)
library(parallel)
library(xgboost)
library(igraph)
library(sfnetworks)
library(tidygraph)
library(tidyverse)
library(missForest)
library(tmap)
library(raster)
library(qgisprocess)
qgis_configure()

#some other functions
#source("R/functions.R")
#tmap_mode("view")

#########################################

#Start of code
#Select regions to do using the regions to do file
regions.todo <- read.csv("../network/01_DataInput/RegionsToDo.csv", stringsAsFactors = F)
if(!all.regions){
  regions.todo <- regions.todo[!is.na(regions.todo$do),]
  regions.todo <- regions.todo$region[regions.todo$do == "y"]
}else{
  regions.todo <- regions.todo$region
}
#regions.todo <- "GreaterManchester" # Manually Force a Region
#regions.todo <- regions.todo[regions.todo != "London"] # Force out a region


message("JIBE-network will run for the following regions:")
print(regions.todo)


tot.start <- Sys.time()

#Step 1: Download the Data
source("scripts/prep_data/download-osm-JIBE-regions.R")


#Step 2: Clean the OSM Tags
source("scripts/prep_data/clean-osm-JIBE-regions.R")


#Step 3: Get connected network from Clean OSM data
source("scripts/prep_data/create-connected-network-JIBE-regions.R")


#Step 4: Join mastermap width data to OSM data
source("scripts/prep_data/join-mastermap-width-OSM-JIBE-regions.R")


#Sept 5: Adding greenness information to OSM data
source("scripts/prep_data/add-greenness-to-OSM.R")


#Sept X: Adding AADT Data


#Step XX: If additional cycle infrastructure data available from local authority, add that to OSM network.
#At present only available for Greater Manchester. So this code block should only run for GM.
#For other areas skip this script
source("scripts/prep_data/add_cyclinginfra_JIBE_GreaterManchester.R")



#Display Finishing Message
tot.end <- Sys.time()
message(paste0("Finished, did ",length(regions.todo)," regions in ", round(as.numeric(difftime(tot.end,tot.start,units = "hours")),2) ," hours, at ",Sys.time()))
rm(tot.end,tot.start)
