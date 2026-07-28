#' @title Major Highways in Dallas County, TX
#' @description Basic line layer in the longitude and latitude format (see
#'   \code{proj4string=CRS("+proj=longlat +ellps=WGS84")}).
#' @docType data
#' @name hwyShp
#' @examples
#' library(sp)
#' tractShp <- DFW_tractShp[DFW_tractShp$COUNTY=="Dallas County",]
#' validTractShp <- tractShp[!is.na(tractShp$NIGHTPOP), ]
#' plot(tractShp, col="white", border="white", axes=TRUE,
#'      main="Dallas Census Tracts with residents")
#' plot(validTractShp, col="ivory2", border="white", add=TRUE)
#' plot(lakesShp, col="skyblue", border="skyblue",add=TRUE)
#' plot(hwyShp, col="cornsilk3", lwd=3, add=TRUE)
#' plot(bndShp, border="black", add=TRUE)
#' box()
#'
NULL
