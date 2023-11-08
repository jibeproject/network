#SET UP
rm(list = ls())

if (!require("sf")) install.packages("sf")

#extract nodes from edge data and give unique IDs
nodes <- edges %>% st_coordinates() %>% as_tibble() %>% rename(edgeID = L1) %>% group_by(edgeID) %>% slice(c(1,n())) %>% ungroup() %>% mutate(start_end = rep(c("start", "end"), times = n()/2))
nodes <- nodes %>% mutate(xy = paste(.$X, .$Y)) %>% mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>% select(-xy)

#remove duplicate nodes removed if need be
nodes <- nodes %>% distinct(nodeID, .keep_all = TRUE) %>% select(-c(edgeID, start_end)) %>% st_as_sf(coords = c('X', 'Y')) %>% st_set_crs(st_crs(edges))

#write them back for visual control in QGIS
geojson_write(nodes, file = file.path(paste0("02_DataCreated/network/nodes-",city,".geojson")))
