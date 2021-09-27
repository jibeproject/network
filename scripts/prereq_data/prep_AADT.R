#Aim prep AADT Data

library(sf)
library(dplyr)


aadt_network <- st_read("../bigdata/AADT/GreaterManchester/aadt_2019.shp") #read shapefile
#Add unique ID
aadt_network <- aadt_network %>% mutate(UID = row_number())

#Save as RDS for other usage later
saveRDS(aadt_network, "../bigdata/AADT/GreaterManchester/aadt_2019.Rds")


