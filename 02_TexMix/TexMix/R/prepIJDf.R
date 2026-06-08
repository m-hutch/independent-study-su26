#' @title Build an origin-destination data frame
#'
#' @description Converts a wide-format location data frame into a long-format
#'   origin-destination (OD) data frame, expanding every variable into origin,
#'   destination, and log-ratio columns for all \eqn{n^2} OD pairs.
#'
#' @details
#' Each numeric column in \code{df} with all positive values is expanded into
#' three OD columns: origin (\code{.i}), destination (\code{.j}), and log ratio
#' (\code{.ijL}). Numeric columns containing non-positive values are expanded into
#' origin and destination columns only. Factor columns are expanded into origin
#' and destination factor columns only.
#'
#' The column suffixes produced depend on the \code{logTrans} and \code{zTrans}
#' settings:
#' \describe{
#'   \item{\code{.i}, \code{.j}}{Origin and destination values (no transformation).}
#'   \item{\code{.ijL}}{Log ratio of origin to destination (positive variables only).}
#'   \item{\code{.iF}, \code{.jF}}{Origin and destination factor levels.}
#'   \item{\code{.iL}, \code{.jL}}{Log-transformed origin and destination (\code{logTrans=TRUE}).}
#'   \item{\code{.iZ}, \code{.jZ}}{Z-standardized origin and destination (\code{zTrans=TRUE}).}
#'   \item{\code{.iLZ}, \code{.jLZ}}{Log- then z-transformed origin and destination
#'     (\code{logTrans=TRUE} and \code{zTrans=TRUE}).}
#'   \item{\code{.ijLZ}}{Log- and z-transformed log ratio (\code{logTrans=TRUE} and
#'     \code{zTrans=TRUE}).}
#' }
#'
#' The unique pair ID (\code{ID.ij}) uses zero-padded integer concatenation:
#' e.g., \code{"ij"} for \eqn{n < 10}, \code{"iijj"} for \eqn{n < 100}, etc.
#'
#' @param df A data frame with \eqn{n} rows, one per location. Each column is
#'   expanded into OD pair format.
#' @param mij Optional \eqn{n \times n} matrix or data frame of migration flows.
#'   If \code{NULL} (default), the \code{mij} column is set to \code{NA}.
#' @param dij Optional \eqn{n \times n} numeric matrix of distances or costs
#'   between locations. If \code{NULL} (default), the \code{dij} column is set
#'   to \code{NA}.
#' @param logTrans Logical. If \code{TRUE}, numeric variables are
#'   log-transformed. Default is \code{FALSE}.
#' @param zTrans Logical. If \code{TRUE}, numeric variables are z-standardized
#'   after any log transformation. Default is \code{FALSE}.
#'
#' @return A data frame with \eqn{n^2} rows (one per OD pair) containing:
#' \describe{
#'   \item{\code{ID.i}}{Integer index of the origin location.}
#'   \item{\code{ID.j}}{Integer index of the destination location.}
#'   \item{\code{ID.ij}}{Unique numeric pair ID combining origin and destination indices.}
#'   \item{\code{mij}}{Migration flow between origin and destination (or \code{NA}).}
#'   \item{\code{dij}}{Distance or cost between origin and destination (or \code{NA}).}
#'   \item{...}{Expanded OD columns for each variable in \code{df}, using the
#'     suffixes described in Details.}
#' }
#'
#' @export
#' @examples
#' data(tractShp)
#' validTractShp <- tractShp[!is.na(tractShp$BUYPOW), ]
#' # Use a small subset for illustration
#' smallDf <- as.data.frame(validTractShp)[1:5, c("PERCAPINC", "PCTMINOR", "CITYPERI")]
#' od <- prepIJDf(smallDf)
#' head(od)
#'
#' # With log transformation
#' od_log <- prepIJDf(smallDf[, c("PERCAPINC", "PCTMINOR")], logTrans=TRUE)
#' head(od_log)
prepIJDf <- function(df, mij=NULL, dij=NULL, logTrans=FALSE, zTrans=FALSE){
  ##############################################################################
  ## Build a long-format origin-destination (OD) dataframe from a wide-format
  ## input dataframe, expanding every variable into origin (.i), destination (.j)
  ## and log-ratio (.ijL) columns for all n^2 OD pairs.
  ##
  ## Arguments:
  ##   df        - dataframe with n rows, one per location. Each column is
  ##               expanded into OD pair format.
  ##   mij       - optional n x n list of migration flows. If NULL,
  ##               the mij column is set to NA.
  ##   dij       - optional n x n numeric matrix of distances or costs between
  ##               locations. If NULL, the dij column is omitted.
  ##   logTrans  - logical boolean flag indicating if numeric variables are to
  ##               be log-transformed.
  ##   zTrans    - logical boolean flag indicating if numeric variables are to
  ##               be standardized (scale of -1 to 1)
  ## Indices:
  ##           '.i'    for origin
  ##           '.j'    for destination
  ##           '.ij'   for origin/destination ratio
  ##           '.ijF'  factor variable
  ##           '.ijL'  log-transformed variable
  ##           '.ijLZ' log- and z-transformed variable
  ##           '.ijZ'  z-transformed variable
  ##############################################################################
  stopifnot(
    "df must be a dataframe" = is.data.frame(df)
    ,"logTrans must be a logical boolean" = is.logical(logTrans)
    ,"zTrans must be a logical boolean" = is.logical(zTrans)
    ,"mij must be a matrix or data frame of size n x n" =
      is.null(mij) ||
      ((is.matrix(mij) || is.data.frame(mij)) && all(dim(mij) == nrow(df)))
    ,"dij must be a numeric array of size n x n"=
      is.null(dij) ||
      (is.double(dij) && all.equal(dim(dij),rep(nrow(df),2)))
  )

  make_pair_id <- function(i, j, n=max(c(i, j))) {
    ##############################################################################
    ## Helper function to create pair IDs of every combination of vectors i and j
    ## i, j are integer vectors
    ## n: the upper bound of i & j, used to set digit-width so IDs don't collide.
    ##    Default max(i,j) but should be set explicitly when i/j may be a subset
    ##    of their full range.
    ## ex: 'ij' for n < 10, iijj' for n < 100 and 'iiijjj' for n < 1000 etc.
    ##############################################################################
    stopifnot(  "i must be an integer vector"=is.integer(i)
                , "j must be an integer vector"=is.integer(j)
                , "n must be a single positive integer"
                =(is.integer(n) && (n>0) && length(n) == 1)
    )
    digits <- trunc(log10(n) + 1)
    return(i * 10^digits + j)
  }


  expand_column_to_od <- function(col_name, df, transform) {
    ##############################################################################
    ## Helper function to process one column of a dataframe to ij format
    ## col_name is the column name corresponding to column in df to process
    ## df is the data frame with the source data to process
    ## transform is a string 'log', 'standard', 'log.standard', or 'none'
    ##############################################################################
    stopifnot("transform must be str 'log', 'standard', 'log.standard', or 'none'"
              = transform %in% c("log", "standard", "log.standard", "none")
              , "df must be a data frame" = is.data.frame(df)
              , "col_name muse be a column name in df" = col_name %in% names(df)
    )

    col    <- df[[col_name]] # column of interest
    od_vec <- expand.grid(col, col) # origin-destination vector

    if (is.factor(col)) { # for factor columns, only make ij pairs
      col_levels <- levels(df[[col_name]])
      od_var <- data.frame(
        i = factor(od_vec[, 1], levels = col_levels),
        j = factor(od_vec[, 2], levels = col_levels)
      )
      names(od_var) <- c(paste0(col_name, ".iF"), paste0(col_name, ".jF"))
    } else { # for numeric columns, make ij pairs and log ratio,
      # and apply any transformations
      i_col  <- od_vec[, 1]
      j_col  <- od_vec[, 2]
      is_log      <- transform %in% c("log", "log.standard")
      is_standard <- transform %in% c("standard", "log.standard")
      all_positive <- !any(df[[col_name]] <= 0)

      od_var <- if (all_positive) {
        data.frame(
          i   = if (is_log) log(i_col) else i_col,
          j   = if (is_log) log(j_col) else j_col,
          ratio = log(i_col / j_col)
        )
      } else {
        # non-positive numeric variables will only generate ij pairs,
        # ignores any log transformation setting passed to `transform`
        data.frame(i = i_col, j = j_col)
      }

      if (is_standard) {
        od_var <- as.data.frame(scale(od_var))
      }

      suffix <- switch(transform,
                       "log"          = c(".iL",  ".jL",  ".ijL"),
                       "standard"     = c(".iZ",  ".jZ",  ".ijLZ"),
                       "log.standard" = c(".iLZ", ".jLZ", ".ijLZ"),
                       "none"         = c(".i",   ".j",   ".ijL")
      )

      names(od_var) <- paste0(col_name, suffix[seq_len(ncol(od_var))])
    } # end::numeric columns

    return(od_var)
  }


  transform <- if(logTrans&&zTrans){
    "log.standard"
  }else if(logTrans){
    "log"
  }else if(zTrans){
    "standard"
  }else{
    "none"
  }

  ## Setup origin and destination ids and unique record id with the format:
  ## 'ij' for n < 10, iijj' for n < 100 and 'iiijjj' for n < 1000 etc.
  pairs <- expand.grid(i = 1:nrow(df), j = 1:nrow(df))

  od_df <- data.frame(
    ID.i  = pairs$i,
    ID.j  = pairs$j,
    ID.ij = make_pair_id(pairs$i, pairs$j)
  )

  ## Add vectorized migration flows
  od_df$mij <- if (is.null(mij)) {
    NA
  } else {
    as.vector(as.matrix(mij))
  }

  ## Add vectorized distances
  od_df$dij <- if(is.null(dij)){
    NA
  } else {
    as.vector(as.matrix(dij, nrow=nrow(dij)^2))
  }

  ## Cycle over all variables
  od_vars <- lapply(names(df), expand_column_to_od, df = df, transform=transform)

  ## Combine all variables into one data frame
  od_df    <- do.call(data.frame, c(list(od_df), od_vars))

  return(od_df)
} #end::prepIJDf
