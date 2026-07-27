#' @title Polygon layer of Italy's neighboring countries and regions
#' @description Boundaries of the 19 countries and territories that share a
#'   border with Italy, in longitude/latitude format (WGS84, EPSG:4326).
#'   Intended for use as a backdrop or context layer alongside
#'   \code{\link{ItalyShp}}.
#' @docType data
#' @name Italy_neighShp
#' @source Derived from publicly available administrative boundary data.
#' @format Spatial polygon data-frame with 19 neighboring countries/regions.
#'   The variables are as follows:
#' \describe{
#'   \item{ID}{Internal numeric ID.}
#'   \item{AREA}{Area of the polygon in square kilometers.}
#'   \item{NAME}{Name of the neighboring country or territory.}
#' }
#' @examples
#' data(Italy_neighShp)
#' sp::summary(Italy_neighShp)
NULL
