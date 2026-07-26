#' Texas Highway Network
#'
#' A spatial line data frame containing the highway and major road network
#' for Texas, with functional classification.
#' @name TX_hwy
#' @format A SpatialLinesDataFrame with highway/road segments and 1 variable:
#'
#' \describe{
#'   \item{fclass}{Character. Functional classification of the road segment.
#'     Possible values include:
#'     \itemize{
#'       \item "motorway" – Interstate highways and limited-access expressways
#'       \item "trunk" – U.S. highways and state highways (major routes)
#'       \item "primary" – State highways and major regional routes
#'       \item "secondary" – Secondary state highways and important local routes
#'       \item "tertiary" – Minor local roads and rural highways
#'       \item "residential" – Roads serving residential areas
#'       \item "unclassified" – Roads with unspecified classification
#'     }
#'     The exact classification scheme may follow OpenStreetMap standards or
#'     a similar transportation network taxonomy.
#'   }
#' }
#'
#' @details
#' This dataset represents the road and highway network across Texas as
#' connected line segments. Each segment is classified by its functional role
#' in the transportation network, from high-capacity interstate highways to
#' local roads.
#'
#' The functional classification (\code{fclass}) is useful for:
#' \itemize{
#'   \item Network analysis (routing, shortest path, connectivity)
#'   \item Transportation planning and corridor identification
#'   \item Visualization of highway hierarchies and road types
#'   \item Spatial analysis of accessibility and connectivity
#'   \item Linking external transportation data by road type
#' }
#'
#' @section Functional Classification Hierarchy:
#' The functional classes generally follow a hierarchy from highest to lowest
#' capacity and speed:
#' \enumerate{
#'   \item \strong{motorway} – Limited access, highest capacity (Interstates: I-35, I-45, etc.)
#'   \item \strong{trunk} – Major routes, high capacity (U.S. highways: US-77, US-287, etc.)
#'   \item \strong{primary} – Regional routes, moderate-high capacity (state highways)
#'   \item \strong{secondary} – Local routes, moderate capacity
#'   \item \strong{tertiary} – Minor routes, local connectivity
#'   \item \strong{residential} – Residential streets and local access roads
#'   \item \strong{unclassified} – Roads not assigned to a specific category
#' }
#'
#' @note
#' This is a line network dataset. For network analysis (routing, shortest paths),
#' users may need to:
#' \itemize{
#'   \item Ensure line segments are properly connected at nodes
#'   \item Handle one-way vs. two-way traffic direction (if available)
#'   \item Use specialized network analysis packages (e.g., \code{igraph},
#'     \code{stplanr}, or \code{osmnx} for Python users)
#' }
#'
#' The dataset may not include all local roads, especially in rural areas,
#' depending on the source. For comprehensive local routing, consider supplementing
#' with local government road datasets or OpenStreetMap data.
#'
#' @source Highway network data source (specific origin TBD). May be derived
#' from OpenStreetMap, USGS, or state transportation databases.
#'
#' @examples
#' \dontrun{
#' library(sp)
#'
#' # Plot all highways
#' plot(TX_hwy)
#'
#' # Plot only interstates and U.S. highways
#' major_hwy <- TX_hwy[TX_hwy@data$fclass %in% c("motorway", "trunk"), ]
#' plot(major_hwy, col = "red", lwd = 2)
#'
#' # Count road segments by functional class
#' table(TX_hwy@data$fclass)
#'
#' # Find roads near a location (example: buffer analysis)
#' library(rgeos)
#' highway_access <- gBuffer(TX_hwy, width = 1)  # 1-unit buffer
#' plot(highway_access)
#'
#' # Subset to interstates only
#' interstates <- TX_hwy[TX_hwy@data$fclass == "motorway", ]
#' length(interstates)  # Number of interstate segments
#' }
#'
#' @keywords datasets spatial transportation network highway
"TX_hwy"
