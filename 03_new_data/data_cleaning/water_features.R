library(osmextract)
library(sf)
library(dplyr)

Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")

# Download and convert the PBF/GPKG for a specific region (e.g., Texas)
# The package will automatically extract standard OSM water polygons
water_data <- oe_get(
  place = "US-TX",
  match_by = "iso3166_2",
  layer = "multipolygons",
  quiet = FALSE,
  query = "SELECT osm_id, name, other_tags, geometry FROM multipolygons
           WHERE other_tags LIKE '%\"water\"=>\"riverbank\"%'
              OR other_tags LIKE '%\"water\"=>\"reservoir\"%'"
)

water_data_sf <- st_as_sf(water_data)
sf_use_s2(FALSE)
unified_water <- water_data %>%
  st_make_valid() %>%
  st_buffer(dist = 0) %>%
  st_union()
fast_water_plot <- st_simplify(water_data_sf, dTolerance = 0.001)
sf_use_s2(TRUE)
plot(fast_water_plot)

rivers <- st_read("../TX_major_rivers.gpkg")
lakes <- st_read("../TX_major_reservoirs.gpkg")
plot(rivers$geom)
plot(lakes$geom)
rivers.2 <- rivers[, c("NAME", "geom")]
library(TexMix)
# EPSG 32614 is the standard code for UTM Zone 14 North
polygon_utm <- st_transform(rivers.2, crs = 32614)

# Simplify vertices
# dTolerance is in meters. Increase this value for a more aggressive simplification.
polygon_simple_utm <- st_simplify(polygon_utm, dTolerance = 10)

# Reproject back to Latitude/Longitude (WGS 84)
# EPSG 4326 is the standard code for Lat/Lon
polygon_final_latlon <- st_transform(polygon_simple_utm, crs = 4326)

sf_poly_2d <- st_zm(lakes)
simplified_data <- st_simplify(sf_poly_2d, preserveTopology = TRUE, dTolerance = 0.001)

# Convert to SpatialPolygonsDataFrame
spatial_poly_df <- as(sf_poly_2d, "Spatial")

TX_lakes <- polygon_final_latlon
TX_rivers <- polygon_final_latlon

plot(TX_rivers$geom)

save(TX_rivers, file="TX_riversShp.rda")
