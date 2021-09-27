#Aim: Add the slope to each edge using Digital terrain data

# install.packages("remotes")
#remotes::install_github("itsleeds/slopes")
library(slopes)

#Main code
regions <- regions.todo

#create directory
if(!dir.exists(paste0("../bigdata/osm-slops-DEM"))){
  dir.create(paste0("../bigdata/osm-slops-DEM"))
}


for(a in 1:length(regions)){

  print (a)

  if(file.exists(paste0("../bigdata/dem/",regions[a],"/CR_DEM.tif"))){ #Check for input file
    if(file.exists(paste0("../bigdata/osm-slops-DEM/",regions[a],"/osm_slope.Rds"))){ #check for existing copy of output
      message(paste0("Skipping slope estimation for ", regions[a]," as already done"))

    }
    else{
      #create directory
      if(!dir.exists(paste0("../bigdata/osm-slops-DEM/",regions[a]))){
        dir.create(paste0("../bigdata/osm-slops-DEM/",regions[a]))
      }

      message(paste0(Sys.time(), ": Estimating slopes to OSM for ",regions[a]))

      osm <- readRDS(paste0("../bigdata/osm-mastermap-width/",regions[a],"/osm_mmap_width.Rds"))

      dtm <- rast (paste0("../bigdata/dem/",regions[a],"/CR_DEM.tif"))

      osm$slope = slope_raster (osm, dem = dtm)

      saveRDS(osm,paste0("../bigdata/osm-slops-DEM/",regions[a],"/osm_slope.Rds"))

    }

  }

}
