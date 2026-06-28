#' @noRd
.legend_labels <- function(vec, under = "under", over = "over",
                     between = "-", reverse = FALSE) {

  if (length(vec) < 3)
    stop("vec must have at least 3 elements")

  if (reverse) {
    vec   <- rev(vec)
    under <- "over"
    over  <- "under"
  }

  lx  <- length(vec)
  res <- character(lx - 1)

  res[1]      <- paste(under, vec[2])
  res[lx - 1] <- paste(over,  vec[lx - 1])

  if (lx > 3)
    res[2:(lx - 2)] <- paste(vec[2:(lx - 2)], between, vec[3:(lx - 1)])

  res
}

#' @noRd
.resolve_palette <- function(palette, n, reverse = FALSE, category='any',
                             colorblind=FALSE) {
  options<-if(category %in% c('seq', 'div', 'qual')){
    RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == category, ]
  }else{
    RColorBrewer::brewer.pal.info
  }

  if (colorblind){
    options <- options[options$colorblind==TRUE,]
  }

  if (palette %in% rownames(options)){
    cols <- RColorBrewer::brewer.pal(n, palette)
  } else {
    # Fall back to RColorBrewer Orange
    cols <- RColorBrewer::brewer.pal(n, "Oranges")
    warning("Could not resolve palette, defaulting to RColorBrewer Orange")
  }

  if (reverse) cols <- rev(cols)
  cols
}

