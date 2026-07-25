#' @title Function: creates a histogram of a color palette with set breaks
#'
#' @description \code{viewPaletteSpread} generates a histogram of a color palette
#' of an interval scaled variable
#'
#' @details The function \code{viewPaletteSpread} maps an \emph{interval scaled
#' variable} by a \emph{sequetial color ramp}.
#'
#' @usage viewPaletteSpread(class.intervals, pal=NULL, title="",
#'equal.width=TRUE, bins=NULL, density=TRUE, density.bw="nrd",
#'density.kernel="gaussian", density.adjust=1, ColorRamp.breaks=FALSE)
#'
#' @param class.intervals \code{ClassIntervals} object or named list with items
#' \code{class.intervals} and \code{pal}
#' @param pal a color palette from \code{RColorBrewer:brewer.pal}
#' @param title optional title for the histogram
#' @param equal.width Logical, determines if equal width bins should be used
#' (default=\code{TRUE})
#' @param bins numeric number of bins to use (forces \code{equal.width=T})
#' @param density Logical, displays estimated density curve over plot (default=\code{TRUE})
#' @param density.bw bandwidth argument passed to \code{stats::density()} function
#' @param density.kernel kernel argument passed to \code{stats::density()} function
#' @param density.adjust bandwidth adjustment multiplier passed to \code{stats::density()} function
#' @param ColorRamp.breaks Logical, displays exact breakpoints that the color ramp changes
#'
#' @export
#' @return \code{NULL}
#' @author Mae Hutchison <mah230002@@utdallas.edu>
#' @examples
#' ramp <- mapColorRamp(tractShp, 'bad1500D', breaks=9,
#'              map.title="Density of Convenience Stores in Dallas County\nbw=1500 meters",
#'              legend.title="Junk Food", output.breaks=TRUE)
#'
#'viewPaletteSpread(ramp)
#'
#'viewPaletteSpread(ramp$class.intervals, ramp$pal)
#'
viewPaletteSpread <- function(class.intervals, pal = NULL, title = "",
                         equal.width = TRUE, bins = NULL, density=TRUE,
                         density.bw="nrd", density.kernel="gaussian",
                         density.adjust=1,
                         ColorRamp.breaks=FALSE) {

  # Accept either a named list(class.intervals=, pal=) or separate arguments
  if (is.list(class.intervals) &&
      all(c("class.intervals", "pal") %in% names(class.intervals))) {
    pal             <- class.intervals$pal
    class.intervals <- class.intervals$class.intervals
  }
  if (is.null(pal)) stop("pal must be provided either as a separate argument or in the list")

  # Auto-enable equal.width if bins is explicitly supplied
  if (!is.null(bins)) equal.width <- TRUE

  main_title <- if (nchar(title) > 0) title else "Distribution"

  if (!equal.width) {
    graphics::hist(class.intervals$var,
                   breaks = class.intervals$brks,
                   freq   = FALSE,
                   col    = pal,
                   border = grDevices::grey(0.85),
                   main   = main_title,
                   xlab   = "Value",
                   ylab   = "Density",
                   las    = 1)

  } else {
    n_classes <- length(pal)

    # Default: 5 equal-width bins per class interval
    # Override with bins= but warn if not a clean multiple
    if (is.null(bins)) {
      bins <- n_classes * 5L
    } else {
      if (bins %% n_classes != 0L) {
        warning(sprintf(
          "bins (%d) is not a multiple of the number of class intervals (%d). Bar colors may not align cleanly with breaks. ",
          bins, n_classes
        ))
        if(!ColorRamp.breaks){
          warning("Consider setting ColorRamp.breaks=TRUE to see exact color break points")
        }
      }
    }

    # Force exact breakpoints so bins align with class-interval boundaries
    data_range  <- range(class.intervals$var, na.rm = TRUE)
    bin_breaks  <- seq(data_range[1], data_range[2], length.out = bins + 1)

    h <- graphics::hist(class.intervals$var, breaks = bin_breaks, plot = FALSE)

    bar_colors <- pal[
      findInterval(h$mids, class.intervals$brks, rightmost.closed = TRUE)
    ]

    graphics::plot(h,
                   col    = bar_colors,
                   border = grDevices::grey(0.85),
                   main   = main_title,
                   freq   = FALSE,
                   xlab   = "Value",
                   ylab   = "Density",
                   las    = 1)
  }

  if(density){
    dx <- density(class.intervals$var,
                  kernel=density.kernel, bw=density.bw, adjust=density.adjust)
    # Add density
    graphics::lines(dx, lwd = 2, col = "black")
  }

  if(ColorRamp.breaks){
  graphics::abline(v   = class.intervals$brks,
                   col = grDevices::grey(0.4),
                   lty = 2,
                   lwd = 0.8)
  }
}
