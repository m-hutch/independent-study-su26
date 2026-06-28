#' @title Function: Maps a sequential color theme
#'
#' @description \code{mapColorRamp} generates a map with a sequential theme
#' of an interval scaled variable
#'
#' @details The function \code{mapColorRamp} maps an \emph{interval scaled
#' variable} by a \emph{sequetial color ramp}. Quantiles values are coded in
#' a sequential palette. A legend is generated. NA's are permitted.
#'
#' @usage mapColorRamp(shape, column, breaks=8, break.style="quantile",
#'map.title="Sequential Choropleth Map", palette="Oranges", colorblind=FALSE,
#'reverse=FALSE, legend.title=column, legend.digits=3, legend.pos="bottomleft",
#'legend.cex=1, legend.bty='n', legend.col=1, legend.inset=0, add.to.map=FALSE,
#'plot.axes=TRUE, plot.border=grDevices::grey(0.9), na.color=grDevices::grey(0.96),
#'na.label="NA", output.breaks=FALSE)
#'
#' @param shape An existing spatial polygon or spatial polygon data-frame
#' @param column A string name of a column in the data-frame of shape argument,
#' to be mapped in a sequential theme.
#' @param breaks Number of breaks in color ramp, in the range of 3 to 9
#' @param break.style style passed to \code{classInt::classIntervals()} to determine
#' method of calculating breaks
#' @param map.title Character string with map title
#' (default=\code{"Sequential Choropleth Map"})
#' @param palette Character string passed to \code{RColorBrewer::brewer.pal()}
#' @param colorblind Logical indicating whether to use colorblind-friendly palettes
#' @param reverse Logical indicating whether to reverse the color ramp
#' @param legend.title Character string with legend title
#' (default=\code{column})
#' @param legend.digits Integer specifying the number of digits to show in the
#' legend (default = \code{3})
#' @param legend.pos Location of legend in the map frame passed to
#' \code{graphics::legend()} (default=\code{"bottomleft"})
#' @param legend.cex Relative font size of the legend
#' @param legend.bty Character string specifying the box type for the legend
#' @param legend.col Character string specifying the legend border color
#' (default = \code{"black"})
#' @param legend.inset Numeric value specifying the inset of the legend
#' (default = \code{0})
#' @param add.to.map Logical to start a new map frame if \code{FALSE} or overlay
#'  onto an existing map frame if \code{TRUE}
#' @param plot.axes Logical to plot axes (default=\code{TRUE})
#' @param plot.border color of plot border (default=\code{grDevices::grey(0.9)})
#' @param na.color color of NA value shapes (default=\code{grDevices::grey(0.96)})
#' @param na.label Character string to label NA values in the legend
#' (default=\code{"NA"})
#' @param output.breaks Logical to return the break data (default=\code{FALSE})
#'
#' @export
#' @return \code{NULL} or \code{list}
#' @author Michael Tiefelsdorf <tiefelsdorf@@utdallas.edu>
#' @author Mae Hutchison <mah230002@@utdallas.edu>
#' @examples
#' ramp <- mapColorRamp(tractShp, 'bad1500D', breaks=9,
#'              map.title="Density of Convenience Stores in Dallas County\nbw=1500 meters",
#'              legend.title="Junk Food", output.breaks=TRUE)
#'
#'# plot distribution of breaks
#'palette_hist(ramp)
#'
#'# or use plot.classIntervals
#'plot(ramp$class.intervals, pal=ramp$pal)
#'
mapColorRamp <- function(shape, column,
                         breaks=8, break.style="quantile",
                         map.title="Sequential Choropleth Map",
                         palette="Oranges",
                         colorblind=FALSE,
                         reverse=FALSE,
                         legend.title=column,
                         legend.digits=3,
                         legend.pos="bottomleft",
                         legend.cex=1,
                         legend.bty='n',
                         legend.col=1,
                         legend.inset=0,
                         add.to.map=FALSE,
                         plot.axes=TRUE,
                         plot.border=grDevices::grey(0.9),
                         na.color=grDevices::grey(0.96),
                         na.label="NA",
                         output.breaks=FALSE){
  stopifnot(
    "shape must be a SpatialPolygonsDataFrame" =
      methods::is(shape, "SpatialPolygonsDataFrame")
    ,"column must be a character string name of column" = methods::is(column, "character")
    ,"column must be in shape spatial data frame" =
      column %in% names(shape))

  ## get data to plot
  data <- shape[[column]]

  if (length(unique(na.omit(data))) < 2) {
    stop("column '", column
         , "' has fewer than 2 unique non-NA values; cannot compute breaks.")
  }

  ## define breaks and color assignment
  class.intervals <-  suppressWarnings( # supress warning for NAs
    classInt::classIntervals(data, n=breaks, style=break.style)
  )
  breaks.vec <- as.numeric(class.intervals$brks)
  pal <- .resolve_palette(palette, breaks, category='seq',
                          reverse=reverse, colorblind=colorblind)
  map.col <- pal[findInterval(data, breaks.vec, rightmost.closed=TRUE)]
  legend.labs <- .legend_labels(round(breaks.vec,digits=legend.digits))

  if (anyNA(data)){
    map.col[is.na(data)] <- na.color       # Set NA's to light grey
    pal <- c(pal[1:breaks], na.color)      # Augment legend color
    legend.labs <- c(legend.labs, na.label)  # Augment legend name
  }

  # -- map panel --
  sp::plot(shape, col=map.col
           , border=plot.border
           , axes=plot.axes,
           add=add.to.map)
  graphics::legend(legend.pos, title=legend.title
                   ,legend=legend.labs
                   , fill=pal
                   , bty=legend.bty
                   , ncol=legend.col
                   , cex=legend.cex
                   , inset = legend.inset)
  graphics::title(map.title)
  graphics::box()

  if(output.breaks){
    output <- list(class.intervals=class.intervals, pal=pal,
                   legend.labs=legend.labs)
    return(output)
  }
} # end::mapColorRamp
