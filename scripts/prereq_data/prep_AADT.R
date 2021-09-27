#Aim prep AADT Data

library(sf)
library(dplyr)


aadt_network <- st_read("../bigdata/AADT/GreaterManchester/aadt_2019.shp") #read shapefile
#Add unique ID
aadt_network <- aadt_network %>% mutate(UID = row_number()) %>%
  mutate(aadt_all = c1_2019 + c2_2019 + c3_2019 + c4a_2019 + c4b_2019)

#Save as RDS for other usage later
saveRDS(aadt_network, "../bigdata/AADT/GreaterManchester/aadt_2019.Rds")


