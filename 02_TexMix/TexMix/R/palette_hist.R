#' @title Function: creates a histogram of a color palette with set breaks
#'
#' @description \code{palette_hist} generates a histogram of a color palette
#' of an interval scaled variable
#'
#' @details The function \code{palette_hist} maps an \emph{interval scaled
#' variable} by a \emph{sequetial color ramp}.
#'
#' @usage palette_hist(class.intervals, pal=NULL, title = "")
#'
#' @param class.intervals \code{ClassIntervals} object or named list with items
#' \code{class.intervals} and \code{pal}
#' @param pal a color palette from \code{RColorBrewer:brewer.pal}
#' @param title optional title for the histogram
#'
#' @export
#' @return \code{NULL}
#' @author Mae Hutchison <mah230002@@utdallas.edu>
#' @examples
#' ramp <- mapColorRamp(tractShp, 'bad1500D', breaks=9,
#'              map.title="Density of Convenience Stores in Dallas County\nbw=1500 meters",
#'              legend.title="Junk Food", output.breaks=TRUE)
#'
#'palette_hist(ramp)
#'
#'palette_hist(ramp$class.intervals, ramp$pal)
#'
palette_hist <- function(class.intervals, pal=NULL, title = "") {
  # Accept either a named list(class.intervals=, pal=) or separate arguments
  if (is.list(class.intervals) &&
      all(c("class.intervals", "pal") %in% names(class.intervals))) {
    pal              <- class.intervals$pal
    class.intervals  <- class.intervals$class.intervals
  }

  if (is.null(pal)) stop("pal must be provided either as a separate argument or in the list")


  graphics::hist(class.intervals$var,
                 breaks  = class.intervals$brk,
                 col     = pal,
                 border  = grDevices::grey(0.85),
                 main    = if (nchar(title) > 0) title else "Distribution",
                 xlab    = "Value",
                 ylab    = "Count",
                 las     = 1)

  # Overlay break lines
  graphics::abline(v   = class.intervals$brk,
                   col = grDevices::grey(0.4),
                   lty = 2,
                   lwd = 0.8)
}
