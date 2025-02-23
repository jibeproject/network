# Aim: download osm lines and junction points

#Download Function
download.region <- function(a){
  #Get Region Name
  region_nm <- as.character(bounds$cityregions[a])

  #Get Region
  region_shp <- bounds[a,]
  region_shp$region <- region_nm
  region_shp <- region_shp[,"region"]

  #Download data
  q = opq(st_bbox(region_shp)) %>%
    add_osm_feature(key = "highway")
  res = osmdata_sf(q = q)

  #extract lines and points data
  lines <- res$osm_lines
  lines.loops <- res$osm_polygons
  points <- res$osm_points
  rm(res,q)

  #Remove Factors
  lines$osm_id <- as.numeric(as.character(lines$osm_id))
  lines.loops$osm_id <- as.numeric(as.character(lines.loops$osm_id))
  points$osm_id <- as.numeric(as.character(points$osm_id))

  #remove the invalid polygons
  lines.loops <- lines.loops[!is.na(lines.loops$highway),]
  lines.loops <- lines.loops[is.na(lines.loops$area),]

  #Add in any missing columns
  check <- colcheck %in% names(lines)
  check.loops <- colcheck %in% names(lines.loops)

  for(b in 1:length(colcheck)){
    if(check[b]){
      #Do nothing
    }else{
      #Add the column filled with NAs
      lines[,colcheck[b]] <- NA
    }
  }

  if(nrow(lines.loops)>0){
    #Add in any missing columns
    for(i in 1:length(colcheck)){
      if(check.loops[i]){
        #Do nothing
      }else{
        #Add the column filled with NAs
        lines.loops[,colcheck[i]] <- NA
      }
    }
  }else{
    for(i in 1:length(colcheck)){
      if(check.loops[i]){
        #Do nothing
      }else{
        #Add the column filled with NAs
        lines.loops[,colcheck[i]] <- character(0)
      }
    }
  }


  lines <- lines[ ,colcheck]
  lines.loops <- lines.loops[ ,colcheck]
  rm(b,i,check, check.loops)

  #Change Polygons to Lines
  lines.loops <- st_cast (lines.loops, "LINESTRING")
  lines <- st_cast (lines, "LINESTRING")

  # remove invalid geometry
  #lines <- lines[st_is_valid(lines) %in% TRUE,] # %in% TRUE handles NA that occure with empty geometries
  lines.loops <- lines.loops[st_is_valid(lines.loops) %in% TRUE,]
  #points <- points[st_is_valid(points) %in% TRUE,]

  #Bind together
  lines <- rbind(lines,lines.loops)
  rm(lines.loops)

  #Change to British National Grid
  lines <- st_transform(lines, 27700)
  points <- st_transform(points, 27700)
  region_shp <- st_transform(region_shp, 27700)


  #Save out region boundary for reference
  saveRDS(region_shp,paste0("../bigdata/osm-raw/",region_nm,"/bounds.Rds"))

  #Download osm used a square bounding box, now trim to the exact boundary
  #note that lines that that cross the boundary are still included
  lines <- lines[region_shp,]
  points <- points[region_shp,]

  #now cut the lines to the boundary
  #lines <- st_intersection(region_shp,lines) #this has the problem of creating multilinestring

  #Save the lines
  saveRDS(lines, paste0("../bigdata/osm-raw/",region_nm,"/osm-lines.Rds"))

  #Find Junctions, OSM Points are both nodes that make up lines/polygons, and objects e.g. shops
  #remove points that are not nodes on the line
  #node points have no tags
  #col.names <- names(points)[!names(points) %in% c("osm_id","highway", "crossing", "crossing_ref","geometry")] #Get column names other than osm_id, and highway which is just for junction types, and crossing info which can be junction between cycle way and road
  #points.sub <- points
  #points <- points[,c("osm_id","highway")]
  #points.sub <- as.data.frame(points.sub)
  #points.sub$geometry <- NULL
  #points.sub <- points.sub[,col.names]
  #rowsum <- as.integer(rowSums(!is.na(points.sub)))
  #rm(points.sub, col.names)
  #points <- points[rowsum == 0,] #Remove points with any tags

  #now check highway tag to remove things like traffic lights
  #points <- points[is.na(points$highway) | points$highway %in% c("mini_roundabout","motorway_junction"), ]
  #points <- points[,c("osm_id","geometry")]

  #Look for points that intersect lines
  #inter <- st_intersects(points,lines)
  #len <- lengths(inter)
  #points <- points[len >= 2,] #Only keep points that intersec at least 2 lines i.e. a junction

  #Remove any duplicated points
  #points <- points[!duplicated(points$geometry),]


  #Save results
  #saveRDS(points, paste0("../bigdata/osm-raw/",region_nm,"/osm-junction-points.Rds"))

  return(TRUE)
}

# Main Code

#create directory
if(!dir.exists(paste0("../bigdata/osm-raw"))){
  dir.create(paste0("../bigdata/osm-raw"))
}

#Get Boundries
bounds <- readRDS("../bigdata/boundaries/cityregions/cityregions_England_10kmbuff.Rds")

#subset the regions to do from the master file
bounds <- bounds[bounds$cityregions %in% regions.todo,]

#Columns to keep
colcheck <- c("osm_id","name",
              #"FIXME","abutters",
              "access",
              #"access.backward","access.conditional","access.motor_vehicle","addr.city",
              #"addr.housename","addr.housenumber","addr.interpolation","addr.postcode","addr.street","agricultural","alt_name","ambulance","amenity","area","attraction",
              "bicycle","bicycle.oneway","bridge",
              #"bridge.movable","bridge.name","bridge.ref","bridge_ref","bridgemaster","building","bus",
              "bus_lane","busway","busway.left","busway.right",
              #"campus","car","carriageway_ref",
              #"coach","comment","complete","construction","construction.active_traffic_management","conveying","covered","created_by","crossing","crossing_ref","cuisine","cutting",
              #"cyclestreets_id",
              "cycleway","cycleway.left",
              "cycleway.left.width","cycleway.oneside.width",
              "cycleway.otherside",
              "cycleway.otherside.width",
              "cycleway.right",
              "cycleway.right.width",
              "cycleway.both",
              #"date","deadend","description","designated",
              "designation",
              #"destination","destination.lanes","direction","distance","disused","drinkable","drinking_water","ele","embankment","emergency","est_width","except","fenced","fhrs.id","fixme","fixme2",
              "foot","footway",
              #"ford","goods","handrail","hgv",
              "highway",
              #"highways_agency.area","historic","history","home_zone",
              #"horse","hov","hov.lanes","hov.minimum","hvg","image","incline","incline.value","indoor","int_ref","invalid_carriage","is_in","is_in.city",
              "junction",
              #"landuse",
              "lanes","lanes.backward","lanes.bus.forward","lanes.forward","lanes.left",
              "lanes.right",
              "lanes.psv","lanes.psv.backward","lanes.psv.forward",
              #"layer","lcn","lcn_ref","level","lit","loc_name","loc_ref",
              #"man_made","maxcc","maxheight","maxheight.imperial","maxheight.physical","maxlength",
              "maxspeed",
              #"maxspeed.type","maxspeed.variable","maxweight","maxweightrating","maxwidth","memorial","moped","motor_vehicle","motor_vehicle.conditional",
              #"motorbike","motorcar","motorcar.conditional","motorcycle","motorcycle.conditional","motorroad","mtb","mtb.scale",
              #"mtb.scale.uphill","name.botanical","name.left","name.right","narrow","natural","ncn_ref","not.name","note","note2",
              #"note.maxwidth","note.name","note.postal_code","note.postcode","old_name","old_ref",
              "oneway","oneway.bicycle",
              #"oneway.psv","operator","osmarender.renderName","overtaking","path","paved","phone","place_numbers","postal_code",
              #"postcode","private",
              "psv","psv.backward",
              #"pvs",
              #"railway","railway.historic","ramp","ramp.bicycle","ramp.wheelchair",
              #"rcn_ref",
              "ref",
              #"roundabout",
              #"sac_scale",
              "segregated","service","shared","sidewalk",
              #"ski","smoothness","snowmobile","source","source.access","source.address","source.bicycle","source.date","source.designation","source.maxspeed",
              #"source.maxweight","source.name","source.not.name","source.outline","source.position","source.ref","source.track","step_count",
              #"steps",
              "surface",
              #"survey","taxi","temporary.access","temporary.date_off","temporary.date_on","todo","toll",
              #"tourism","tourist_bus","tracktype",
              #"traffic_calming","trail_colour","trail_visibility",
              "tunnel",
              #"turn","turn.lanes","turn.lanes.backward","turn.lanes.forward","type","vehicle","website","wheelchair","width","wikipedia",
              "geometry")


# Step 1 Establish which Regions shoudl be skipped
bounds$exists <- NA

for(i in seq(from = 1, to = nrow(bounds))){
  region_nm <- as.character(bounds$cityregions[i])
  exists <- file.exists(paste0("../bigdata/osm-raw/",region_nm,"/osm-lines.Rds"))
  bounds$exists[i] <- exists
  if(!exists){
    dir.create(paste0("../bigdata/osm-raw/",region_nm))
  }
  rm(exists, region_nm)
}
rm(i)

if(skip){
  message(paste0(Sys.time(),": Skipping download of the following regions"))
  print(as.character(bounds$ttwa11nm[bounds$exists]))
  bounds <- bounds[!bounds$exists,]
}


nCores <- detectCores(logical = FALSE)
ncores <- detectCores() - 1


if(nrow(bounds) > 0){

  ##########################################################
  #Parallel
  start <- Sys.time()
  fun <- function(cl){
    parLapply(cl, seq(from = 1, to = nrow(bounds)), download.region)
  }
  cl <- makeCluster(ncores) #make clusert and set number of cores
  clusterExport(cl=cl, varlist=c("bounds", "colcheck"))
  clusterExport(cl=cl, c('download.region') )
  clusterEvalQ(cl, {library(sf); library(osmdata)})
  res <- fun(cl)
  stopCluster(cl)
  end <- Sys.time()
  message(paste0("Finished downloading ",nrow(bounds)," regions in ",round(difftime(end,start,units = "secs"),2)," seconds, in parallel mode at ",Sys.time()))
  rm(cl,fun, res, start, end)
  ##########################################################

}else{
  message("No regions to do")
}


rm(colcheck, bounds)
