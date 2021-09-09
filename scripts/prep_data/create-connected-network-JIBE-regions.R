
#Aim: To create connected and clean network file
# The script will take the OSM clean file, and use tidygraph to split the large OSM lines at proper intersections
# Clean the nodes and edges that are not connected to the larger network

regions <- regions.todo

#create directory
if(!dir.exists(paste0("../bigdata/network-clean"))){
  dir.create(paste0("../bigdata/network-clean"))
}

for(a in 1:length(regions)){

  print (a)

  if(file.exists(paste0("../bigdata/osm-clean/",regions[a],"/osm-lines.Rds"))){ #Check for input file
    if(file.exists(paste0("../bigdata/network-clean/",regions[a],"/network-edges.Rds"))){ #check for existing copy of output
      message(paste0("Skipping cleaning network for ", regions[a]," as already done"))

    }
    else{
      #create directory
      if(!dir.exists(paste0("../bigdata/network-clean/",regions[a]))){
        dir.create(paste0("../bigdata/network-clean/",regions[a]))
      }

      message(paste0(Sys.time(), ": Cleaning network for ",regions[a]))
      osm <- readRDS(paste0("../bigdata/osm-clean/",regions[a],"/osm-lines.Rds"))

      osm_sf <- st_as_sf(osm)

      osm_sfnetwork <- as_sfnetwork(osm_sf)
      #add pseudo nodes for missing intersections
      osm_pseudo <- convert(osm_sfnetwork, to_spatial_subdivision)



      #################
      # PART 1.1: find biggest connected component
      #################
      #find the biggest connected component-- after https://github.com/ropensci/stplanr/issues/344
      osm_pseudo_sf <- osm_pseudo %>% activate("edges") %>% st_as_sf()
      touching_list <- st_touches(osm_pseudo_sf)
      graph_list <- graph.adjlist(touching_list)
      roads_group <- components(graph_list)
      roads_table <- table(roads_group$membership)
      roads_table_order <- roads_table[order(roads_table, decreasing = TRUE)]
      biggest_group <- names(roads_table_order[1])         #enumeration starts from 1 to n number of unconnected subgraphs; largest to smallest

      #keep ONLY connected component
      osm_connected_edges <- osm_pseudo_sf[roads_group$membership == biggest_group, ]
      osm_connected_nodes <- osm_pseudo %>% activate("nodes") %>% st_filter(osm_connected_edges, .pred = st_intersects) %>% st_as_sf()



      #################
      # PART 1.2: give each feature (nodes and edges) unique linked IDs
      #################

      edges <- osm_connected_edges %>% dplyr::mutate(edgeID = c(1:n()))

      #extract nodes from edge data and give unique IDs
      nodes <- edges %>% st_coordinates() %>% as_tibble() %>%
        dplyr::rename(edgeID = L1) %>%
        dplyr::group_by(edgeID) %>% slice(c(1,n())) %>% ungroup() %>%
        dplyr::mutate(start_end = rep(c("start", "end"), times = n()/2))

      nodes <- nodes %>% dplyr::mutate(xy = paste(.$X, .$Y)) %>% dplyr::mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>% select(-xy)

      #ONLY for directed networks/otherwise duplicate edges and nodes to reverse
      source_nodes <- nodes %>% dplyr::filter(start_end == "start") %>% pull(nodeID)
      target_nodes <- nodes %>% dplyr::filter(start_end == "end") %>% pull(nodeID)
      edges <- edges %>% dplyr::mutate(from = source_nodes, to = target_nodes) #could be skipped???????????

      #remove duplicate nodes removed if need be
      nodes <- nodes %>% distinct(nodeID, .keep_all = TRUE) %>% dplyr::select(-c(edgeID, start_end)) %>% st_as_sf(coords = c('X', 'Y')) %>% st_set_crs(st_crs(edges))


      saveRDS(edges,paste0("../bigdata/network-clean/",regions[a],"/network_edges.Rds"))
      saveRDS(nodes,paste0("../bigdata/network-clean/",regions[a],"/network_nodes.Rds"))

      rm(osm_pseudo_sf, touching_list, graph_list, roads_group, roads_table, roads_table_order, biggest_group)
      rm (osm_connected_edges, osm_connected_nodes, source_nodes, target_nodes)
      rm (edges, nodes, osm, osm_sf, osm_sfnetwork, osm_pseudo)

    }}

  }



