#' @title Function: Maps a sequential color theme
#'
#' @description \code{mapColorRamp} generates a map with a sequential theme of an interval scaled variable
#'
#' @details The function \code{mapColorRamp} maps an \emph{interval scaled variable} by a
#'   \emph{sequetial color ramp}. Quantiles values are coded in gradually increasing
#'   intensities of oranges. A legend is generated. NA's are permitted.
#'
#' @usage mapColorRamp(var.name, shape, breaks=8, map.title="",
#'                     legend.title=deparse(substitute(var.name)),
#'                     legend.pos="bottomleft", legend.cex=1, add.to.map=FALSE)
#'
#' @param var.name A variable to be mapped in a bipolar theme. If it is in a data-frame,
#'   then the data-frame must be refered to, e.g., \code{df$var}
#' @param shape An existing spatial polygon or spatial polygon data-frame
#' @param breaks Number of qunatiles. It needs to range the range of 3 to 9
#' @param map.title Character string with map title
#' @param legend.title Character string with legend title (default=\code{var.name})
#' @param legend.pos Location of legend in the map frame (default=\code{"bottomleft"})
#' @param legend.cex Relative font size of the legend
#' @param add.to.map Logical to start a new map frame if \code{FALSE} or overlay onto an
#'   existing map frame if \code{TRUE}
#' @export
#' @return \code{NULL}
#' @author Michael Tiefelsdorf <tiefelsdorf@@utdallas.edu>
#' @examples
#' validTractShp <- tractShp[!is.na(tractShp$BUYPOW), ]         # Remove 2 tracts with NA's
#' mapColorQual(validTractShp$CITYPERI, validTractShp,
#'              map.title="Cities and Peripherie in Dallas County",
#'              legend.title="Regions")
#'
#' mapColorRamp(validTractShp$bad1500D, validTractShp, breaks=9,
#'              map.title="Density of Convenience Stores in Dallas County\nbw=1500 meters",
#'              legend.title="Junk Food")
#'
#' hist(tractShp$LRRmedD)
#' mapBiPolar(validTractShp$LRRmedD, validTractShp, break.value=0,
#'            neg.breaks=5, pos.breaks=5,
#'            map.title="LRR: log(f(junk food),f(healthy food))\nbw=medium",
#'            legend.title="log relative risk")
#'

mapColorRamp <- function(var.name, shape, breaks=8,
                         map.title="", legend.title=deparse(substitute(var.name)),
                         legend.pos="bottomleft", legend.cex=1, add.to.map=FALSE) {
  ##
  ## Plot a color ramp variable "var.name"
  ##
  ##
  ## Define leglabs
  ##
  leglabs <- function (vec, under = "under", over = "over", between = "-",
                       reverse = FALSE)
  {
    x <- vec
    lx <- length(x)
    if (lx < 3)
      stop("vector too short")
    if (reverse) {
      x <- rev(x)
      under <- "over"
      over <- "under"
    }
    res <- character(lx - 1)
    res[1] <- paste(under, x[2])
    for (i in 2:(lx - 2)) res[i] <- paste(x[i], between, x[i +
                                                             1])
    res[lx - 1] <- paste(over, x[lx - 1])
    res
  }
  if (breaks <= 2) stop("At least breaks=3 color classes need to be specified")
  if (breaks >= 10) stop("A maximum of breaks=9 color classes can be specified")
  ## define breaks and color assignment
  q.breaks <- classInt::classIntervals(var.name, n=breaks, style="quantile")
  pal <- RColorBrewer::brewer.pal(breaks, "Oranges")
  #pal.YlOrRd <- brewer.pal(n.breaks, "YlOrRd")
  map.col <- pal[findInterval(var.name, q.breaks$brks, rightmost.closed=TRUE)]
  legendLabs <- leglabs(round(q.breaks$brks,digits=3))

  if (anyNA(var.name)){
    map.col[is.na(pal)] <- grDevices::grey(0.96)       # Set NA's to light grey
    pal <- c(pal[1:breaks],grDevices::grey(0.96))      # Augment legend color
    legendLabs <- c(legendLabs, "NA")                 # Augment legend name
  }

  ## generate choropleth map
  sp::plot(shape, col=map.col, border=grDevices::grey(0.9), axes=TRUE, add=add.to.map)
  graphics::legend(legend.pos, title=legend.title,
                   legend=legendLabs, fill=pal, bty="n", ncol=1, cex=legend.cex)
  graphics::title(map.title)
  graphics::box()
} # end::mapColorRamp
