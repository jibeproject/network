#Aim: Join AADT data
#The AADT data came from John Gulliver's model, which based in year 2013, but later on has been updated using scaling factor for 2019.
#Details of the code and process of updating and scaling factor can be found at:
#"../jibeproject/bigdata/AADT/GreaterManchester/scale_manc_aadt/3_scale_manc_aadt.r"

#Main code
regions <- regions.todo

#create directory
if(!dir.exists(paste0("../bigdata/osm-add-aadt"))){
  dir.create(paste0("../bigdata/osm-add-aadt"))
}


for(a in 1:length(regions)){

  print (a)

  if(file.exists(paste0("../bigdata/AADT/",regions[a],"/aadt_2019.Rds"))){ #Check for input file
    if(file.exists(paste0("../bigdata/osm-add-aadt/",regions[a],"/osm_aadt_added.Rds"))){ #check for existing copy of output
      message(paste0("Skipping joining AADT to ", regions[a]," as already done"))

    }
    else{
      #create directory
      if(!dir.exists(paste0("../bigdata/osm-add-aadt/",regions[a]))){
        dir.create(paste0("../bigdata/osm-add-aadt/",regions[a]))
      }

      message(paste0(Sys.time(), ": Joining AADT to OSM for ",regions[a]))

      osm <- readRDS(paste0("../bigdata/osm-greenness/",regions[a],"/osm_greenness.Rds")) # change based on the previous step

      aadt_network <- readRDS(paste0("../bigdata/AADT/",regions[a],"/aadt_2019.Rds"))

      #Take point sample on master map at certain distance and start/end offset
      aadt_network_points <- qgis_run_algorithm(
        "native:pointsalonglines",
        DISTANCE = 10, #map unit distance, here it is meters
        END_OFFSET = 5,
        INPUT = aadt_network,#change the input here
        START_OFFSET = 5
      )
      #convert the temporary point samples to sf object
      aadt_network_points_sf <- sf::read_sf(qgis_output(aadt_network_points, "OUTPUT"))

      #snap the points to OSM lines with a tolerance
      snapaadtOSMlines <- qgis_run_algorithm(
        "native:snapgeometries",
        BEHAVIOR = 1, #prefer closest points insert extra vertices where needed
        INPUT = aadt_network_points_sf,
        REFERENCE_LAYER = osm,
        TOLERANCE = 20
      )

      #snapped points to sf object
      snapaadtOSMlines_sf <- sf::read_sf(qgis_output(snapaadtOSMlines, "OUTPUT"))

      snapbuffer_aadt <- st_buffer(snapaadtOSMlines_sf, dist = 0.05)

      joinAADTtoosmlines <- qgis_run_algorithm(
        "qgis:joinbylocationsummary",
        DISCARD_NONMATCHING = F,
        INPUT = osm,
        JOIN = snapbuffer_aadt,
        SUMMARIES = "median", # 10 is for majority, 7 for median
        JOIN_FIELDS = c('c1_2019', 'c2_2019', 'c3_2019',  'c4a_2019', 'c4b_2019', 'aadt_all')#the column to join the attribute
      )

      joinAADTtoosmlines_sf <- sf::read_sf(qgis_output(joinAADTtoosmlines, "OUTPUT"))

      # If some OSM links missed during the joining process, transfer the attribute from one row to another
      aadtjointranfer <- joinAADTtoosmlines_sf %>%
        group_by(osm_id) %>%
        mutate(aadt_all_median = mean (aadt_all_median[!is.na(aadt_all_median)])) #this does the transfer

      #Impute the missing width information using other veriables using random forest algorithom
      jointranfermissing.impu <- aadtjointranfer %>%
        select(edgeID, roadtype, maxspeed, sidewalk, onewaysummary, aadt_all_median) %>%
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
        select(edgeID, maxspeed, sidewalk_num, onewaysummary_num, roadtype_num, aadt_all_median) %>%
        st_drop_geometry() %>%
        as.data.frame()

      #Impute using missForest function for the variables in columns 2 to 6, where 2 is maxspeed, and 6 is averageWidth_median
      jointranfermissing.impu [,2:6] <- missForest(jointranfermissing.impu)$ximp[,2:6]

      #drop a lot of variables, and rename the wdith variable to make sure we are clear about the imputation result
      jointranfermissing.impu <- jointranfermissing.impu %>%
        select(edgeID, aadt_all_median) %>%
        rename (averageaadt.imp = aadt_all_median)

      jointranfermissing_imp_add <- left_join(aadtjointranfer,jointranfermissing.impu, by = 'edgeID')


      saveRDS(jointranfermissing_imp_add,paste0("../bigdata/osm-add-aadt/",regions[a],"/osm_aadt_added.Rds"))

      #rm(osm, aadt_network_points, aadt_network_points_sf, snapaadtOSMlines, snapaadtOSMlines_sf, snapbuffer, joinwidthtoosmlines, joinwidthtoosmlines_sf)

    }
  }
}
