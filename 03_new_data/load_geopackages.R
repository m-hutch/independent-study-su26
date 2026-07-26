library(sp)

gpkg_pth <- "/Users/maehutch/Library/CloudStorage/Box-Box/TexMix/TX_highways.gpkg"
output_pth <- "/Users/maehutch/GitHub_m-hutch/independent-study-su26/03_new_data/data_cleaning/TX_highwaysShp.rda"

sfc_obj<-st_read(gpkg_pth, crs=4326)
plot(sfc_obj$geom)

polygon_utm <- st_transform(sfc_obj, crs = 32614) # UTM Zone 14 North
polygon_simple_utm <- st_simplify(polygon_utm, dTolerance = 10)
polygon_final_latlon <- st_transform(polygon_simple_utm, crs = 4326) # (WGS 84)

spatial_poly_df <- as(polygon_final_latlon, "Spatial")
save(spatial_poly_df, file=output_pth)

rdata <-
path <- "/Users/maehutch/Library/CloudStorage/Box-Box/TexMix/"
library(sf)
st_write(rdata, path)
