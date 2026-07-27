#' Plot Method for SpatialPolygonsDataFrame
#'
#' S3 method for plotting \code{SpatialPolygonsDataFrame} objects.
#' Provides a convenient interface to the sp package's plotting functionality.
#'
#' @param x A \code{SpatialPolygonsDataFrame} object to plot.
#' @param ... Additional arguments passed to \code{sp::plot}.
#'
#' @details
#' This method dispatches to the sp package's plot functionality for the actual plotting.
#' For detailed information about available plotting arguments, see the
#' documentation for the sp package.
#'
#' @return
#' Invisibly returns \code{NULL}. Called for its side effect of creating a plot.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have a SpatialPolygonsDataFrame object
#'   plot(my_spdf)
#' }
#'
#' @keywords internal
#'
#' @export
plot.SpatialPolygonsDataFrame <- function(x, ...) {
  sp::plot(methods::as(x, "SpatialPolygons"),...)
}
