#' @title Inter-provincial migration flows in Italy
#' @description An origin–destination matrix of internal migration counts among
#'   the 95 provinces of Italy. Each row represents an origin province and each
#'   destination column (\code{DEST01}–\code{DEST95}) contains the number of
#'   migrants who moved from that origin to the corresponding destination
#'   province. Province ordering matches the \code{ID} field in
#'   \code{\link{ItalyShp}}.
#' @docType data
#' @name Italy_migration
#' @source Anselin, L. (1995). Local indicators of spatial association—LISA.
#'   \emph{Geographical Analysis}, 27(2), 93–115.
#' @format A data frame with 95 rows and 96 columns. \code{ORIGIN} contains
#'   the name of the origin province. The remaining 95 columns
#'   (\code{DEST01} through \code{DEST95}) each contain the number of migrants
#'   moving from that origin to the corresponding destination province, where
#'   column indices match the \code{ID} field in \code{\link{ItalyShp}}.
#' @examples
#' data(Italy_migration)
#' head(Italy_migration[, 1:5])
NULL
