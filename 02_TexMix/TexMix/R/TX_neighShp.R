#' @title Polygon layer of neighboring geographies around Texas, USA
#' @description Boundaries of the US states and Mexico that share a
#'   border with Texas, in longitude/latitude format (WGS84, EPSG:4326).
#'   Intended for use as a backdrop or context layer alongside
#'   \code{\link{TX_countyShp}}..
#'
#' @docType data
#' @name TX_neighShp
#' @source Derived from publicly available administrative boundary data.
#' @format Spatial polygon data-frame with 15 neighboring geographies.
#'   The variables are as follows:
#' \describe{
#'   \item{name}{Character. Geography name.}
#'   \item{statefp}{Character. State FIPS code.}
#'   \item{geoidfq}{Character. Fully qualified geographic identifier (GEOID)}
#'   \item{abbrv}{Character. Abbreviated name or code.}
#' }
#' @examples
#' data(TX_neighShp)
#' sp::summary(TX_neighShp)
NULL
