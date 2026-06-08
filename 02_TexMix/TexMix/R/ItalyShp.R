#' @title Polygon layer of Italian provinces
#' @description Boundaries and demographic/socioeconomic attributes for the 95
#'   provinces of Italy in longitude/latitude format
#'   (WGS84, EPSG:4326).
#' @docType data
#' @name ItalyShp
#' @source Anselin, L. (1995). Local indicators of spatial association—LISA.
#'   \emph{Geographical Analysis}, 27(2), 93–115.
#' @format Spatial polygon data-frame with 95 provinces. The variables are as
#'   follows:
#' \describe{
#'   \item{ID}{Internal numeric province ID.}
#'   \item{AREA}{Area of the province in square kilometers.}
#'   \item{PROVNAME}{Name of the province.}
#'   \item{REGION}{Region of Italy the province belongs to
#'     (\code{"North"}, \code{"Central"}, or \code{"South"}).}
#'   \item{MALEPOP94}{Male population count, 1994.}
#'   \item{FEMPOP94}{Female population count, 1994.}
#'   \item{TOTPOP94}{Total population count, 1994.}
#'   \item{INFLOW}{Number of in-migrants to the province.}
#'   \item{OUTFLOW}{Number of out-migrants from the province.}
#'   \item{TOTFERTRAT}{Total fertility rate (average number of children per woman).}
#'   \item{FEMMARAGE9}{Mean age of women at first marriage, 1994.}
#'   \item{DIVORCERAT}{Divorce rate (divorces per 1,000 marriages).}
#'   \item{ILLITERRAT}{Illiteracy rate (percent of population that is illiterate).}
#'   \item{TELEPERFAM}{Telephones per family.}
#' }
#' @examples
#' data(ItalyShp)
#' sp::summary(ItalyShp)
NULL
