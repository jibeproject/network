#SET UP
rm(list = ls())

if (!require("sf")) install.packages("sf")
if (!require("mice")) install.packages("mice")
library(sf)
library(mice)

#################
# PART 1: imputing missing data with MICE
#################
region_nm <- as.character("GreaterManchester")

#clip to ONLY GM region (excluding the 10km buffer)
osm <- readRDS(file.path("./bigdata/crime/",region_nm,"/osm_crime.Rds"))

gm_bounds <- st_read(file.path("./GitHub_inputfiles_network/GM_bounds_unbuf.shp")) #in the Teams folder WP2>Data_WP2>Processed_Data>Greater Manchester>GitHub_inputfiles_network
osm_gm <- st_intersection(osm, gm_bounds)

#test for missingness
per_miss <- function(x) {sum(is.na(x))/length(x)*100}
apply(osm, 2, per_miss)

#get dataframe for imputing: Imputations are generated according to the default method, which is predictive mean matching
#BEST predition for both high and low extremes
osm_imp <- osm_gm %>% select(edgeID, highway, maxsped, aadt_md, speedKPH) %>% st_drop_geometry() %>% as.data.frame()
osm_imp <- mice(osm_imp)

#view first imputed complete table (output by default set to produce five prediction outputs-- either check for  least outliers of use mean of all five)
tibble::view(as.data.frame(complete(osm_imp,1)))

#view fifth imputed complete table
tibble::view(as.data.frame(complete(osm_imp,3)))

#view fifth imputed complete table
tibble::view(as.data.frame(complete(osm_imp,5)))

imputed <- complete(osm_imp,5)[,c(1,4:5)]

#rename aadt_md to aadt_mp
colnames(imputed)[2] <- 'aadt_mp'

#################
# PART 2: attach speedKPH with imputed values back to osm
#################
osm <- osm %>% select(-speedKPH) #remove speedKPH with missing values
osm <- merge(osm, imputed[,c("edgeID", "speedKPH")], by = "edgeID", all = TRUE) #attach speedKPH after imputation back on the edges

saveRDS(osm, paste0("./bigdata/speedTfGM/",region-nm,"/osm_speed.Rds"))
