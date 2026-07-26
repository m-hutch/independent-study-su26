#' Texas Lakes and Water Bodies
#'
#' A spatial polygon data frame containing lakes, reservoirs, and major water
#' bodies across Texas.
#' @name TX_lakes
#' @format A SpatialPolygonsDataFrame with 2 variables:
#'
#' \describe{
#'   \item{Shape_Leng}{Numeric. Perimeter length of the water body polygon,
#'     measured in the units of the coordinate reference system (typically
#'     meters for projected coordinate systems or degrees for geographic
#'     coordinate systems). Useful for calculating shoreline length, shape
#'     complexity, or fractal dimension.}
#'   \item{Shape_Area}{Numeric. Area of the water body polygon, measured in
#'     the units of the coordinate reference system squared (typically square
#'     meters for projected systems or square degrees for geographic systems).
#'     Represents the surface area of the lake or water body. To convert to
#'     standard units (square kilometers, acres), divide by the appropriate
#'     conversion factor.}
#' }
#'
#' @details
#' This dataset represents permanent and semi-permanent water bodies in Texas,
#' including natural lakes, reservoirs, and impoundments. Each polygon represents
#' the boundary of a single water body.
#'
#' \code{Shape_Leng} and \code{Shape_Area} are automatically generated geometry
#' attributes, typically calculated by GIS software (e.g., ArcGIS) when creating
#' or exporting spatial data. They are standard for polygon shapefiles and provide
#' a quick reference for the spatial extent of each feature without recalculating
#' perimeter and area.
#'
#' Common analytical uses:
#' \itemize{
#'   \item Calculate perimeter-to-area ratio (shape compactness)
#'   \item Identify largest or smallest lakes by area
#'   \item Estimate shoreline length for environmental studies
#'   \item Analyze water body fragmentation across regions
#'   \item Spatial intersection with other datasets (e.g., counties, census tracts)
#' }
#'
#' @section Unit Conversion:
#' If the coordinate reference system is projected (e.g., UTM, State Plane):
#' \itemize{
#'   \item \strong{Meters to Kilometers}: Divide \code{Shape_Area} by 1,000,000
#'     to convert m² to km²
#'   \item \strong{Meters to Acres}: Divide \code{Shape_Area} by 4,047 to convert
#'     m² to acres
#'   \item \strong{Meters to Square Miles}: Divide \code{Shape_Area} by 2,589,988
#'     to convert m² to square miles
#'   \item \strong{Perimeter in Kilometers}: Divide \code{Shape_Leng} by 1,000
#' }
#'
#' If the coordinate reference system is geographic (latitude/longitude in degrees),
#' unit conversion is more complex; consider reprojecting to a projected
#' coordinate system first (e.g., UTM Zone 14N for central Texas).
#'
#' @note
#' - These are automatically calculated geometry fields and may have minor
#'   numerical precision differences depending on the GIS software and projection
#'   used to generate them.
#' - No named identifier (e.g., lake name) is provided; for named water bodies,
#'   consider linking with external datasets such as USGS Geographic Names
#'   Information System (GNIS) or the National Hydrography Dataset (NHD).
#' - Water body classification (permanent vs. seasonal, natural vs. reservoir)
#'   is not provided; spatial intersection with other datasets may help distinguish types.
#' - For detailed hydrographic analysis, the National Hydrography Dataset (NHD)
#'   is recommended as it includes flow direction, stream order, and other
#'   network attributes.
#'
#' @source Water body boundaries may be derived from USGS, National Hydrography
#' Dataset (NHD), the National Map, or state/local GIS databases. Specific
#' source and vintage TBD.
#'
#' @examples
#' \dontrun{
#' library(sp)
#'
#' # Plot all water bodies
#' plot(TX_lakes, col = "lightblue", border = "darkblue")
#'
#' # Calculate area in acres (assuming units are in meters)
#' TX_lakes@data$area_acres <- TX_lakes@data$Shape_Area / 4047
#'
#' # Calculate area in square kilometers
#' TX_lakes@data$area_km2 <- TX_lakes@data$Shape_Area / 1000000
#'
#' # Find the largest lake by area
#' largest <- TX_lakes[which.max(TX_lakes@data$Shape_Area), ]
#' print(largest@data)
#'
#' # Calculate perimeter-to-area ratio (shape compactness)
#' TX_lakes@data$shape_index <- TX_lakes@data$Shape_Leng /
#'   sqrt(TX_lakes@data$Shape_Area)
#'
#' # Identify lakes larger than 10 square kilometers
#' large_lakes <- TX_lakes[TX_lakes@data$Shape_Area > 10000000, ]
#' cat("Number of lakes > 10 km²:", nrow(large_lakes@data), "\n")
#'
#' # Spatial intersection: lakes in Tarrant County
#' library(rgeos)
#' tarrant <- TX_countyShp[TX_countyShp@data$name == "Tarrant", ]
#' lakes_in_tarrant <- gIntersection(TX_lakes, tarrant, byid = TRUE)
#' plot(lakes_in_tarrant, col = "lightblue")
#' }
#'
#' @keywords datasets spatial water lakes hydrography
"TX_lakes"
