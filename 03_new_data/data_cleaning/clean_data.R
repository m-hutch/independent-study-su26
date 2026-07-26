library(dplyr)
library(tidyr)
library(sp)
library(sf)
library(raster)
final_data <- "/Users/maehutch/GitHub_m-hutch/independent-study-su26/03_new_data/final_data/"

# Texas Counties
load("~/GitHub_m-hutch/independent-study-su26/03_new_data/data_cleaning/TX_countyShp_pyramid.rda")
TX_countyShp <- final_shp
TX_countyShp <- select(TX_countyShp, -c(PRISON_MALE, PRISON_FEMALE, MILITARY_MALE, MILITARY_FEMALE))
TX_countyShp <- as(TX_countyShp, "Spatial")
file<-paste0(final_data,"TX_countyShp.Rdata")
save(TX_countyShp, file=file)

#NCTCOG tracts
load("~/GitHub_m-hutch/independent-study-su26/03_new_data/data_cleaning/NCTCOG_tractShp.rda")
DFW_tractShp <- final_shp
DFW_tractShp <- select(DFW_tractShp, -c(TIME2WORK, PCTBADENG))
summary(DFW_tractShp)


tract.factors<-st_read("/Users/maehutch/Library/CloudStorage/Box-Box/TexMix/NCTCOG/tracts_with_factor_vars.gpkg"
, crs=4326)
tract.factors<- select(tract.factors, c(GEOID20, SchoolDistrict, Municipality, CongLevel))
tract.factors<-rename(tract.factors, GEOID=GEOID20)
tract.factors.d <- st_drop_geometry(tract.factors)

joined <- DFW_tractShp %>% left_join(tract.factors.d, by="GEOID")
joined$SchoolDistrict <- as.factor(joined$SchoolDistrict)
joined$Municipality <- as.factor(joined$Municipality)
joined$CongLevel <- as.factor(joined$CongLevel)
DFW_tractShp <- joined
DFW_tractShp <- as(DFW_tractShp, Class = "Spatial")
save(DFW_tractShp, file=paste0(final_data,"DFW_tractShp.Rdata"))


#NCTCOG blockgroups
load("~/GitHub_m-hutch/independent-study-su26/03_new_data/data_cleaning/NCTCOG_bgShp.rda")
DFW_bgShp <- final_shp
DFW_bgShp <- select(DFW_bgShp, -c(TIME2WORK, PCTBADENG))
DFW_bgShp <- as(DFW_bgShp, Class = "Spatial")
save(DFW_bgShp, file=paste0(final_data,"DFW_bgShp.Rdata"))

#TX neighbors
TX_neighShp <- st_read("/Users/maehutch/Library/CloudStorage/Box-Box/TexMix/TX_neighbors.gpkg")
TX_neighShp<- st_zm(TX_neighShp)
TX_neighShp <- as(TX_neighShp, Class = "Spatial")
save(TX_neighShp, file=paste0(final_data,"TX_neighShp.Rdata"))

#TX highways
load("~/GitHub_m-hutch/independent-study-su26/03_new_data/data_cleaning/TX_highwaysShp.rda")
TX_hwy<- spatial_poly_df
plot(TX_hwy)
save(TX_hwy, file=paste0(final_data,"TX_hwy.Rdata"))

#TX water
load("~/GitHub_m-hutch/independent-study-su26/03_new_data/data_cleaning/TX_lakesShp.rda")
load("~/GitHub_m-hutch/independent-study-su26/03_new_data/data_cleaning/TX_riversShp.rda")
TX_lakes<- st_zm(TX_lakes)
TX_lakes <- as(TX_lakes, Class = "Spatial")

my_extent <- extent(c( -97.954,-95.796, 32.431, 33.319))
clipped_sp <- crop(TX_lakes, my_extent)
TX_lakes<-clipped_sp
plot(TX_lakes)
save(TX_lakes, file=paste0(final_data,"TX_lakes.Rdata"))

TX_rivers<- st_zm(TX_rivers)
TX_rivers <- as(TX_rivers, Class = "Spatial")
plot(TX_rivers)
save(TX_rivers, file=paste0(final_data,"TX_rivers.Rdata"))


