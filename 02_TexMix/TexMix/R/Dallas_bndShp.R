#' @title Boundary of Dallas County, TX
#' @description One polygon in the longitude and latitude format (see
#'   \code{proj4string=CRS("+proj=longlat +ellps=WGS84")}).
#' @docType data
#' @name Dallas_bndShp
#' @examples
#' library(sp)
#' tractShp <- DFW_tractShp[DFW_tractShp$COUNTY=="Dallas County",]
#' validTractShp <- tractShp[!is.na(tractShp$NIGHTPOP), ]
#' plot(tractShp, col="white", border="white", axes=TRUE,
#'      main="Dallas Census Tracts with residents")
#' plot(validTractShp, col="ivory2", border="white", add=TRUE)
#' plot(Dallas_lakesShp, col="skyblue", border="skyblue",add=TRUE)
#' plot(Dallas_hwyShp, col="cornsilk3", lwd=3, add=TRUE)
#' plot(Dallas_bndShp, border="black", add=TRUE)
#' box()
#'
NULL
