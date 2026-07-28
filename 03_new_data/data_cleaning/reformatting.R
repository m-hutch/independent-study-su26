head(DFW_tractShp@data)
library(dplyr)
library(tidyr)
library(sp)
library(sf)
library(raster)

final_data <- "/Users/maehutch/GitHub_m-hutch/independent-study-su26/03_new_data/final_data/"


DFW_water <- st_read("/Users/maehutch/Library/CloudStorage/Box-Box/TexMix/tx_water_simplified.gpkg")
plot(DFW_water)
sp::plot(DFW_water)

DFW_waterShp <- DFW_water
save(DFW_waterShp, file=paste0(final_data,"DFW_waterShp.RData"))

TX_hwyShp <- TX_hwy
save(TX_hwyShp, file=paste0(final_data,"TX_hwyShp.RData"))

Italy_neighShp <- Italy_neighborsShp
save(Italy_neighShp, file=paste0(final_data,"Italy_neighShp.RData"))


# tract
data <- DFW_tractShp@data
names <- data.frame(do.call('rbind', strsplit(as.character(data$NAME),'; ',fixed=TRUE)))
data$NAME <- names$X1
data$COUNTY <- names$X2
DFW_tractShp@data <- data
DFW_tractShp@data$SchoolDistrict<-as.character(DFW_tractShp@data$SchoolDistrict)
DFW_tractShp@data$Municipality<-as.character(DFW_tractShp@data$Municipality)
DFW_tractShp@data$CongLevel<-as.character(DFW_tractShp@data$CongLevel)
DFW_tractShp@data[is.na(DFW_tractShp@data$CongLevel),]<- "Not assessed"
DFW_tractShp@data$CongLevel<-as.factor(DFW_tractShp@data$CongLevel)

dallas<-DFW_tractShp[DFW_tractShp@data$COUNTY=="Dallas County",]
dallas$SchoolDistrict <- as.character(dallas$SchoolDistrict)
factor_vector <- forcats::fct_other(factor(dallas$SchoolDistrict),
                           keep = levels(factor(dallas$SchoolDistrict))[1:12],
                           other_level = "other")
save(DFW_tractShp, file=paste0(final_data,"DFW_tractShp.Rdata"))


data <- DFW_bgShp@data
names <- data.frame(do.call('rbind', strsplit(as.character(data$NAME),'; ',fixed=TRUE)))
data$NAME <- names$X1
data$TRACT <- names$X2
data$COUNTY <- names$X3
DFW_bgShp@data <- data
t.data <- DFW_tractShp@data[,c("SchoolDistrict", "Municipality", "CongLevel")]
t.data['TRACT'] <- DFW_tractShp@data$NAME
new.data2 <- data %>%
  left_join(t.data,by="TRACT")
DFW_bgShp@data<-new.data2
save(DFW_bgShp, file=paste0(final_data,"DFW_bgShp.Rdata"))



non.num <- c("GEOID", "NAME", "SchoolDistrict", "Municipality", "CongLevel", "COUNTY")

for (col in names(DFW_tractShp@data)) {
  if (!col %in% non.num) {
    DFW_tractShp@data[, col] <- as.numeric(as.character(DFW_tractShp@data[, col]))
  }
}
save(DFW_tractShp, file="/Users/maehutch/GitHub_m-hutch/independent-study-su26/02_TexMix/TexMix/data/DFW_tractShp.RData")


library(tidycensus)
library(dplyr)


# Get the PERCAPINC for census tracts in Texas
texas_per_cap_income <- get_acs(
  geography = "tract",
  state = "TX",
  variables = "B19301_001",
  year = 2024,
  survey = "acs5",
  output = "wide"
)

# View the data
glimpse(texas_per_cap_income)

add.var<- texas_per_cap_income%>%
  mutate(PERCAPINC=B19301_001E)  %>% select(GEOID, PERCAPINC)

DFW_tractShp@data <- DFW_tractShp@data %>% left_join(add.var, by='GEOID')
data("DFW_tractShp")

load("~/GitHub_m-hutch/independent-study-su26/02_TexMix/TexMix/data/DFW_lakesShp.RData")
Dallas_lakesShp <- DFW_lakesShp
save(Dallas_lakesShp, file="~/GitHub_m-hutch/independent-study-su26/02_TexMix/TexMix/data/Dallas_lakesShp_lakesShp.RData")

load("~/GitHub_m-hutch/independent-study-su26/02_TexMix/TexMix/data/bndShp.RData")
Dallas_bndShp <- bndShp
save(Dallas_bndShp, file="~/GitHub_m-hutch/independent-study-su26/02_TexMix/TexMix/data/Dallas_bndShp.RData")

load("~/GitHub_m-hutch/independent-study-su26/02_TexMix/TexMix/data/hwyShp.RData")
Dallas_hwyShp <- hwyShp
save(Dallas_hwyShp, file="~/GitHub_m-hutch/independent-study-su26/02_TexMix/TexMix/data/Dallas_hwyShp.RData")
