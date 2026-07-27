.onLoad <- function(libname, pkgname) {
  require("sp")
  setMethod("plot", "SpatialPolygonsDataFrame", function(x, ...) sp::plot(x, ...))
}
