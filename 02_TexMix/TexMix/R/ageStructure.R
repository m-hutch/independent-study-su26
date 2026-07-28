#' Plot an Age Structure Diagram (Age-Sex Population Pyramid) from a Spatial Data Frame
#'
#' Extracts male and female age-band population columns from a
#' \code{Spatial*DataFrame} object, optionally re-bins them to a coarser
#' age resolution, and draws a back-to-back population pyramid (males on
#' the left, females on the right).
#'
#' Age-band columns are identified via \code{age_regex} and must encode a
#' start age and either an exclusive end age (e.g. \code{"0_TO_5"} means
#' \code{0 <= age < 5}) or an open-ended top bracket (e.g. \code{"85_PLUS"}).
#' If multiple rows are present in \code{sdf@data}, counts are summed across
#' all rows before plotting.
#'
#' When \code{bin.width} differs from the width implied by the source
#' columns, columns are aggregated into new, wider bins based on each
#' column's start age. Bins cannot be split into narrower ones — see
#' \strong{Errors}. Male and female totals are re-binned independently and
#' then checked for identical resulting bin labels, ensuring the two sides
#' of the pyramid stay aligned.
#'
#' @param sdf A \code{Spatial*DataFrame} object (e.g. \code{SpatialPolygonsDataFrame})
#'   whose \code{@data} slot contains one row per observation (e.g. per
#'   region) and one column per sex/age-band combination.
#' @param bin.width Numeric. Width, in years, of the age bins to display.
#'   Defaults to \code{5}. Must be greater than or equal to the widest
#'   finite-width age column found in \code{sdf}, since existing bins can
#'   only be merged into wider ones, not split into narrower ones.
#' @param sex_prefixes Character vector of length 2 giving the column-name
#'   prefixes used to identify male and female age-band columns, in the
#'   order \code{c(male, female)}. Defaults to \code{c("MALE_", "FEMALE_")}.
#' @param age_regex A regular expression, matched against the end of each
#'   column name, used to identify age-band columns and capture their age
#'   range. Must contain three capture groups: (1) the start age,
#'   (2) the end age following \code{"_TO_"} (exclusive upper bound), and
#'   (3) the literal \code{"PLUS"} marker for an open-ended top bracket.
#'   Defaults to \code{"(\\\\d+)(?:_TO_(\\\\d+)|_(PLUS))?$"}, matching
#'   column suffixes like \code{"0_TO_5"} or \code{"85_PLUS"}.
#' @param main Character. Title for the plot. Defaults to
#'   \code{"Age Structure Diagram"}.
#' @param name Character. A label identifying the observation(s) being
#'   plotted (e.g. a place name). If \code{NA} (the default), it is
#'   inferred automatically: when \code{sdf@data} has exactly one row, a
#'   \code{Name} column is used if present, otherwise the deparsed
#'   \code{sdf} argument is used; when there are multiple rows, the
#'   deparsed \code{sdf} argument is used, suffixed with the observation
#'   count.
#'
#' @return Invisibly returns a list with components:
#'   \describe{
#'     \item{male}{Named numeric vector of male population totals per
#'       (re-binned) age group.}
#'     \item{female}{Named numeric vector of female population totals per
#'       (re-binned) age group.}
#'     \item{ages}{Character vector of age-group labels, in ascending
#'       order, with the open-ended bracket (if any) last.}
#'     \item{start}{Numeric vector of inclusive start ages for each bin.}
#'     \item{end}{Numeric vector of exclusive end ages for each bin
#'       (\code{Inf} for an open-ended top bracket).}
#'   }
#'   As a side effect, a population pyramid is drawn on the current
#'   graphics device.
#'
#' @section Errors:
#' An error is raised if:
#' \itemize{
#'   \item No columns in \code{sdf@data} match \code{age_regex}.
#'   \item Columns matching both \code{sex_prefixes} cannot be found.
#'   \item An age range cannot be parsed from one or more matched column
#'     names.
#'   \item \code{bin.width} is smaller than the widest existing finite-width
#'     age column, which would require splitting an aggregate count into
#'     narrower bins.
#'   \item Male and female columns produce different bin labels after
#'     re-binning (indicating mismatched underlying age breakpoints between
#'     the two sexes).
#' }
#'
#' @examples
#' \dontrun{
#' # Default 5-year bins
#' ageStructure(county_shapefile)
#'
#' # Re-bin into 10-year age groups with a custom title
#' ageStructure(county_shapefile, bin.width = 10,
#'              main = "Population by Age and Sex")
#' }
#'
#' @export
ageStructure <- function(sdf, bin.width = 5, sex_prefixes = c("MALE_", "FEMALE_"),
                         age_regex = "(\\d+)(?:_TO_(\\d+)|_(PLUS))?$",
                         main = "Age Structure Diagram",
                         name = NA) {
  stopifnot("bin.width must be numeric" = is.numeric(bin.width))
  if(is(sdf, "Spatial")){
    # Extract the data frame from the spatial object
    df <- sdf@data
    stopifnot("sdf@data must be a data frame"=is.data.frame(df))
    stopifnot("sdf@data must have at least one row"=length(df)>0)
  }else{
    df<-sdf # allow only data frame to be passed
    stopifnot("sdf or sdf@data must be a data frame"=is.data.frame(df))
    stopifnot("sdf or sdf@data must have at least one row"=length(df)>0)
  }


  mcol <- grDevices::rgb(0.2, 0.4, 0.8, 0.7) # male bar color
  fcol <- grDevices::rgb(0.8, 0.4, 0.6, 0.7) # female bar color

  ## ---- helper: parse start (inclusive) / end (exclusive) age from a column name ----
  parseAgeRange <- function(colname, regex) {
    m <- regmatches(colname, regexec(regex, colname))[[1]]
    if (length(m) == 0 || is.na(m[1])) {
      return(c(start = NA_real_, end = NA_real_))
    }
    start    <- as.numeric(m[2])
    to_val   <- if (length(m) >= 3) m[3] else ""
    plus_val <- if (length(m) >= 4) m[4] else ""
    if (nzchar(to_val)) {
      end <- as.numeric(to_val)    # "_TO_N" already means exclusive upper bound N
    } else if (nzchar(plus_val)) {
      end <- Inf                   # open-ended "PLUS" bucket
    } else {
      end <- start + 1             # single-age bucket, e.g. "100"
    }
    c(start = start, end = end)
  }

  ## ---- helper: rebin a set of (start, end, total) triples onto a new bin.width ----
  rebin <- function(totals, starts, ends, bin.width) {
    finite_widths <- (ends - starts)[is.finite(ends)]
    if (length(finite_widths) > 0 && bin.width < max(finite_widths)) {
      stop(sprintf(
        "bin.width (%s) is smaller than an existing age column width (%s); cannot split existing bins into narrower ones.",
        bin.width, max(finite_widths)
      ))
    }

    is_plus  <- !is.finite(ends)
    plus_idx <- which(is_plus)
    reg_idx  <- which(!is_plus)

    min_start <- min(starts)
    max_finite_end <- if (length(reg_idx)) max(ends[reg_idx]) else min_start
    if (length(plus_idx)) max_finite_end <- max(max_finite_end, max(starts[plus_idx]))

    edges <- seq(min_start, max_finite_end, by = bin.width)
    if (max(edges) < max_finite_end) edges <- c(edges, max(edges) + bin.width)

    n_bins <- length(edges) - 1
    new_totals <- numeric(n_bins)
    new_start  <- edges[-length(edges)]
    new_end    <- edges[-1]

    for (i in seq_len(n_bins)) {
      idx <- reg_idx[starts[reg_idx] >= new_start[i] & starts[reg_idx] < new_end[i]]
      new_totals[i] <- sum(totals[idx], na.rm = TRUE)
    }

    # fold any PLUS columns into the final bin, or add a dedicated open-ended bin
    if (length(plus_idx)) {
      plus_total <- sum(totals[plus_idx], na.rm = TRUE)
      plus_start <- min(starts[plus_idx])
      if (plus_start < new_end[n_bins]) {
        new_totals[n_bins] <- new_totals[n_bins] + plus_total
        new_end[n_bins] <- Inf
      } else {
        new_start  <- c(new_start, plus_start)
        new_end    <- c(new_end, Inf)
        new_totals <- c(new_totals, plus_total)
      }
    }

    labels <- labels <- ifelse(is.finite(new_end),
                               paste0(new_start, " to ", new_end - 1),
                               paste0(new_start, "+"))

    list(totals = new_totals, start = new_start, end = new_end, labels = labels)
  }

  if (is.na(name)) {
    if (is.na(name)) {
      if (nrow(df) == 1) {
        # single observation: look for a "Name" column and use its value
        name_col <- grep("^name$", names(df), ignore.case = TRUE, value = TRUE)
        if (length(name_col) > 0) {
          name <- as.character(df[[name_col[1]]][1])
        } else {
          name <- deparse(substitute(sdf))
        }
      } else {
        # multiple observations: use the variable name plus a count
        name <- paste0(deparse(substitute(sdf)), " (geographies = ", nrow(df), ")")
      }
    }
  }



  # Get column names matching the age regex
  age_cols <- grep(age_regex, names(df), value = TRUE)
  if (length(age_cols) == 0) {
    stop("No columns matching the age regex pattern found.")
  }

  male_prefix <- sex_prefixes[1]
  female_prefix <- sex_prefixes[2]

  female_cols <- grep(paste0("^", female_prefix), age_cols, value = TRUE)
  male_cols   <- grep(paste0("^", male_prefix), age_cols, value = TRUE)
  if (length(male_cols) == 0 || length(female_cols) == 0) {
    stop("Could not find columns matching both male and female prefixes.")
  }

  # Sum across all observations if multiple rows exist
  male_totals   <- colSums(df[, male_cols, drop = FALSE], na.rm = TRUE)
  female_totals <- colSums(df[, female_cols, drop = FALSE], na.rm = TRUE)

  # Parse start/end age for each column
  male_ranges   <- t(sapply(male_cols, parseAgeRange, regex = age_regex))
  female_ranges <- t(sapply(female_cols, parseAgeRange, regex = age_regex))

  if (any(is.na(male_ranges)) || any(is.na(female_ranges))) {
    stop("Could not parse an age range from one or more matched column names.")
  }

  # order both sexes by start age so bars/labels line up correctly
  male_order   <- order(male_ranges[, "start"])
  female_order <- order(female_ranges[, "start"])
  male_cols <- male_cols[male_order]; male_totals <- male_totals[male_order]
  male_ranges <- male_ranges[male_order, , drop = FALSE]
  female_cols <- female_cols[female_order]; female_totals <- female_totals[female_order]
  female_ranges <- female_ranges[female_order, , drop = FALSE]

  # Rebin male/female totals independently onto the requested bin.width
  male_binned   <- rebin(male_totals, male_ranges[, "start"], male_ranges[, "end"], bin.width)
  female_binned <- rebin(female_totals, female_ranges[, "start"], female_ranges[, "end"], bin.width)

  if (!identical(male_binned$labels, female_binned$labels)) {
    stop("Male and female age bins do not align after rebinning; check that source columns share the same age breakpoints.")
  }

  male_totals   <- male_binned$totals
  female_totals <- female_binned$totals
  age_labels    <- male_binned$labels

  # Create the pyramid
  max_val <- max(c(male_totals, female_totals))

  par.setting<-par(no.readonly = TRUE) # save par state
  graphics::par(mar = c(5, 9, 4, 9))
  plot(NULL,
       xlim = c(-max_val, max_val),
       ylim = c(0, length(age_labels)+1.5),
       xlab = "Population",
       ylab = "",
       main = paste(main, "-", name),
       yaxt = "n",
       xaxt = "n",
       type = "n")

  # Custom x-axis with comma-formatted, absolute-value labels
  x_ticks <- graphics::axTicks(1)
  x_labels <- format(abs(x_ticks), big.mark = ",", scientific = FALSE, trim = TRUE)
  graphics::axis(1, at = x_ticks, labels = x_labels)
  graphics::axis(2, at = seq_along(age_labels), labels = age_labels, las = 1)

  label_width <- max(graphics::strwidth(age_labels, units = "inches", cex = graphics::par("cex.axis")))
  title_line <- label_width / graphics::strwidth("M", units = "inches", cex = graphics::par("cex.lab")) + 1.5
  graphics::mtext("Age Group", side = 2, line = title_line, cex = graphics::par("cex.lab"))

  for (i in seq_along(male_totals)) {
    graphics::rect(xleft = -male_totals[i], xright = 0, ybottom = i - 1, ytop = i,
         col = mcol, border = NA)
  }
  for (i in seq_along(female_totals)) {
    graphics::rect(xleft = 0, xright = female_totals[i], ybottom = i - 1, ytop = i,
         col = fcol, border = NA)
  }

  graphics::abline(v = 0, lwd = 2)
  graphics::legend("topleft",
         legend = c("Male", "Female"),
         fill = c(mcol, fcol),
         bty = "n")

  graphics::par(par.setting, no.readonly = TRUE)

  invisible(list(
    male = male_totals, female = female_totals,
    ages = age_labels,
    start = male_binned$start, end = male_binned$end
  ))
}#end::ageStructure
