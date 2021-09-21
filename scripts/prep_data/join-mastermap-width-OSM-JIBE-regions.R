
#Aim: Join Width and Speed data from MasterMap to OSM network

#create a function to guess the width when missing or NA in master map
osm.join.width.clean <- function(roadtyp, width){

  #Check again if needed
  # width not given guess
  if(roadtyp %in% c("Cycleway", "Segregated Cycleway") && width > 5){
    width <- 3
  }else if(roadtyp %in% c("Living Street") && width > 5) {
    width <- 4
  }else if(roadtyp %in% c("Shared Path", "Segregated Shared Path") && width > 5) {
    width <- 3
  }else if(roadtyp %in% c("Path - Cycling Forbidden") && width > 5) {
    width <- 3
  }else{
    width <- width
  }

  return(width)

}


#Main code
regions <- regions.todo

#create directory
if(!dir.exists(paste0("../bigdata/osm-mastermap-width"))){
  dir.create(paste0("../bigdata/osm-mastermap-width"))
}


for(a in 1:length(regions)){

  print (a)

  if(file.exists(paste0("../bigdata/network-clean/",regions[a],"/network_edges.Rds"))){ #Check for input file
    if(file.exists(paste0("../bigdata/osm-mastermap-width/",regions[a],"/osm_mmap_width.Rds"))){ #check for existing copy of output
      message(paste0("Skipping cleaning OSM tags for ", regions[a]," as already done"))

    }
    else{
      #create directory
      if(!dir.exists(paste0("../bigdata/osm-mastermap-width/",regions[a]))){
        dir.create(paste0("../bigdata/osm-mastermap-width/",regions[a]))
      }

      message(paste0(Sys.time(), ": Joining mastermap width to OSM for ",regions[a]))

      osm <- readRDS(paste0("../bigdata/network-clean/",regions[a],"/network_edges.Rds"))

      mastermap <- readRDS(paste0("../bigdata/MasterMaplinksUk/",regions[a],"/mastermapUK_lines.Rds"))

      mastermap <- mastermap %>%
        filter (!is.na(averageWidth))

      #Take point sample on master map at certain distance and start/end offset
      mastermappoints <- qgis_run_algorithm(
        "native:pointsalonglines",
        DISTANCE = 10,
        END_OFFSET = 5,
        INPUT = mastermap,#change the input here
        START_OFFSET = 5
      )
      #convert the temporary point samples to sf object
      mastermappoints_sf <- sf::read_sf(qgis_output(mastermappoints, "OUTPUT"))

      #snap the points to OSM lines with a tolerance
      snaptoOSMlines <- qgis_run_algorithm(
        "native:snapgeometries",
        BEHAVIOR = 1, #prefer closest points insert extra vertices where needed
        INPUT = mastermappoints_sf,
        REFERENCE_LAYER = osm,
        TOLERANCE = 20
      )

      #snapped points to sf object
      snaptoOSMlines_sf <- sf::read_sf(qgis_output(snaptoOSMlines, "OUTPUT"))

      snapbuffer <- st_buffer(snaptoOSMlines_sf, dist = 0.05)

      joinwidthtoosmlines <- qgis_run_algorithm(
        "qgis:joinbylocationsummary",
        DISCARD_NONMATCHING = F,
        INPUT = osm,
        JOIN = snapbuffer,
        SUMMARIES = 7, # 10 is for majority, 7 for median
        JOIN_FIELDS = 'averageWidth'#the column to join the attribute
      )

      joinwidthtoosmlines_sf <- sf::read_sf(qgis_output(joinwidthtoosmlines, "OUTPUT"))

      # If some OSM links missed during the joining process, transfer the attribute from one row to another
      jointranfer <- joinwidthtoosmlines_sf %>%
        group_by(osm_id) %>%
        mutate(averageWidth_median = mean (averageWidth_median[!is.na(averageWidth_median)])) #this does the transfer

      #Impute the missing width information using other veriables using random forest algorithom
      jointranfermissing.impu <- jointranfer %>%
        select(edgeID, roadtype, maxspeed, sidewalk, onewaysummary, averageWidth_median) %>%
        mutate(sidewalk_num = case_when(
          sidewalk == 'left'  ~ 1,
          sidewalk == 'right'  ~ 1,
          sidewalk == 'both'  ~ 2,
          sidewalk == 'no' ~ 0,
          sidewalk == 'Not Applicable' ~ 0)) %>%
        mutate(onewaysummary_num = case_when(
          onewaysummary == 'Two Way' ~2,
          onewaysummary == 'One Way' ~1,
          onewaysummary == 'One Way - Two Way Cycling' ~1,
          onewaysummary == 'Not Applicable' ~ 0
        )) %>%
        mutate(roadtype_num = case_when(
          roadtype == 'Cycleway' ~1, roadtype == 'Living Street' ~2, roadtype == 'Main Road - Cycling Allowed' ~3, roadtype == 'Main Road Link - Cycling Allowed' ~ 4,
          roadtype == 'Minor Road - Cycling Allowed' ~ 5, roadtype == 'motorway - Cycling Forbidden' ~ 6, roadtype == 'motorway_link - Cycling Forbidden' ~7,
          roadtype == 'Path - Cycling Forbidden' ~ 8, roadtype == 'Residential Road - Cycling Allowed' ~ 9, roadtype == 'Segregated Cycleway' ~ 10,
          roadtype == 'Segregated Shared Path' ~ 11, roadtype == 'Shared Path' ~ 12,roadtype == 'Special Road - Cycling Forbidden' ~ 13,
          roadtype == 'Trunk Road - Cycling Allowed' ~ 14, roadtype == 'Trunk Road Link - Cycling Allowed' ~ 15
        )) %>%
        select(edgeID, maxspeed, sidewalk_num, onewaysummary_num, roadtype_num, averageWidth_median) %>%
        st_drop_geometry() %>%
        as.data.frame()

      #Impute using missForest function for the variables in columns 2 to 6, where 2 is maxspeed, and 6 is averageWidth_median
      jointranfermissing.impu [,2:6] <- missForest(jointranfermissing.impu)$ximp[,2:6]

      jointranfermissing.impu <- jointranfermissing.impu %>%
        select(edgeID, averageWidth_median) %>%
        rename (averageWidth.imp = averageWidth_median)

      jointranfermissing_imp_add <- left_join(jointranfer,jointranfermissing.impu, by = 'edgeID')

      jointranfermissing_imp_add$averageWidth <- mapply(osm.join.width.clean, width = jointranfermissing_imp_add$averageWidth.imp, roadtyp = jointranfermissing_imp_add$roadtype, SIMPLIFY = T, USE.NAMES = F)


      saveRDS(jointranfermissing_imp_add,paste0("../bigdata/osm-mastermap-width/",regions[a],"/osm_mmap_width.Rds"))

      #rm(osm, mastermap, mastermappoints, mastermappoints_sf, snaptoOSMlines, snaptoOSMlines_sf, snapbuffer, joinwidthtoosmlines, joinwidthtoosmlines_sf)

    }
  }
}

#st_write(osm_mmap_width,paste0("../bigdata/osm-mastermap-width/",regions[a],"/osm_mmap_width.gpkg"))

#Back up function, only road based imputation
# osm.join.width.clean <- function(width, roadtyp){
#
#   #Check again if needed
#   if(is.na(width)){
#     # width not given guess
#     if(roadtyp %in% c("Cycleway")){
#       width <- 3
#     }else if(roadtyp %in% c("Living Street")) {
#       width <- 4
#     }else if(roadtyp %in% c("Main Road - Cycling Allowed")){
#       width <- 8.5
#     }else if(roadtyp == "Main Road Link - Cycling Allowed" ){
#       width <- 10
#     }else if(roadtyp == "Minor Road - Cycling Allowed" ){
#       width <- 8
#     }else if(roadtyp %in% c("motorway - Cycling Forbidden")){
#       width <- 16
#     }else if(roadtyp %in% c("motorway_link - Cycling Forbidden")){
#       width <- 12.5
#     }else if(roadtyp %in% c("Path - Cycling Forbidden")){
#       width <- 1
#     }else if(roadtyp %in% c("Residential Road - Cycling Allowed")){
#       width <- 6
#     }else if(roadtyp %in% c("Segregated Cycleway", "Segregated Shared Path")){
#       width <- 6
#     }else if(roadtyp %in% c("Shared Path")){
#       width <- 2
#     }else if(roadtyp %in% c("Special Road - Cycling Forbidden")){
#       width <- 3.5
#     }else if(roadtyp %in% c("Trunk Road - Cycling Allowed")){
#       width <- 11
#     }else if(roadtyp %in% c("Trunk Road Link - Cycling Allowed")){
#       width <- 11
#     }else{
#       width <- NA
#     }
#
#   }
#   return(width)
#
# }


