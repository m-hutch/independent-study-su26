library(testthat)
library(sp)
library(methods)

# ---------------------------------------------------------------------------
# Helper: build a minimal SpatialPolygonsDataFrame for testing
# ---------------------------------------------------------------------------
make_spdf <- function(n = 10, col_values = NULL, col_name = "value") {
  # Create n simple 1x1 unit squares side by side
  polys <- vector("list", n)
  for (i in seq_len(n)) {
    coords <- matrix(
      c(i-1, 0,
        i,   0,
        i,   1,
        i-1, 1,
        i-1, 0),
      ncol = 2, byrow = TRUE
    )
    polys[[i]] <- Polygons(list(Polygon(coords)), ID = as.character(i))
  }
  sp_polys <- SpatialPolygons(polys)

  if (is.null(col_values)) {
    col_values <- seq(1, n)
  }
  df <- data.frame(x = col_values)
  names(df) <- col_name
  rownames(df) <- as.character(seq_len(n))

  SpatialPolygonsDataFrame(sp_polys, data = df)
}

# ---------------------------------------------------------------------------
# 1. INPUT VALIDATION
# ---------------------------------------------------------------------------

test_that("error when shape is not a SpatialPolygonsDataFrame", {
  expect_error(
    mapColorRamp(data.frame(value = 1:5), "value"),
    "shape must be a SpatialPolygonsDataFrame"
  )
})

test_that("error when column is not a character string", {
  shp <- make_spdf()
  expect_error(
    mapColorRamp(shp, 1L),
    "column must be a character string name of column"
  )
})

test_that("error when column is not present in the shape data frame", {
  shp <- make_spdf(col_name = "value")
  expect_error(
    mapColorRamp(shp, "nonexistent_column"),
    "column must be in shape spatial data frame"
  )
})

# ---------------------------------------------------------------------------
# 2. BASIC EXECUTION (no errors / invisible NULL return)
# ---------------------------------------------------------------------------

test_that("function runs without error on valid inputs", {
  shp <- make_spdf(n = 20, col_values = runif(20, 0, 100))
  expect_silent(
    mapColorRamp(shp, "value", breaks = 5)
  )
})

test_that("function returns NULL invisibly when output.breaks = FALSE", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  result <- mapColorRamp(shp, "value", output.breaks = FALSE)
  expect_null(result)
})

# ---------------------------------------------------------------------------
# 3. OUTPUT WHEN output.breaks = TRUE
# ---------------------------------------------------------------------------

test_that("output.breaks=TRUE returns a list with required elements", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  result <- mapColorRamp(shp, "value", breaks = 5, output.breaks = TRUE)

  expect_type(result, "list")
  expect_named(result, c("class.intervals", "pal", "legend.labs"),
               ignore.order = TRUE)
})

test_that("returned pal has the correct number of colors (no NAs)", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  result <- mapColorRamp(shp, "value", breaks = 5, output.breaks = TRUE)
  # Without NAs: pal length == breaks
  expect_length(result$pal, 5)
})

test_that("returned pal has an extra color for NAs when NAs present", {
  vals <- c(1:18, NA, NA)
  shp <- make_spdf(n = 20, col_values = vals)
  result <- mapColorRamp(shp, "value", breaks = 5, output.breaks = TRUE)
  # With NAs: pal length == breaks + 1
  expect_length(result$pal, 6)
})

test_that("legend.labs length matches pal length", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  result <- mapColorRamp(shp, "value", breaks = 5, output.breaks = TRUE)
  expect_equal(length(result$legend.labs), length(result$pal))
})

test_that("class.intervals is a classIntervals object", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  result <- mapColorRamp(shp, "value", breaks = 5, output.breaks = TRUE)
  expect_s3_class(result$class.intervals, "classIntervals")
})

# ---------------------------------------------------------------------------
# 4. NA HANDLING
# ---------------------------------------------------------------------------

test_that("function handles columns with NA values without error", {
  vals <- c(1:15, NA, NA, NA, NA, NA)
  shp <- make_spdf(n = 20, col_values = vals)
  expect_silent(
    mapColorRamp(shp, "value", breaks = 5)
  )
})

test_that("NA label appears in legend.labs when NAs are present", {
  vals <- c(1:18, NA, NA)
  shp <- make_spdf(n = 20, col_values = vals)
  result <- mapColorRamp(shp, "value", breaks = 5,
                         na.label = "Missing", output.breaks = TRUE)
  expect_true("Missing" %in% result$legend.labs)
})

test_that("NA label does NOT appear when no NAs are present", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  result <- mapColorRamp(shp, "value", breaks = 5,
                         na.label = "Missing", output.breaks = TRUE)
  expect_false("Missing" %in% result$legend.labs)
})

# ---------------------------------------------------------------------------
# 5. BREAKS PARAMETER
# ---------------------------------------------------------------------------

test_that("breaks parameter controls number of classes", {
  shp <- make_spdf(n = 30, col_values = 1:30)
  for (b in c(3, 5, 7, 9)) {
    result <- mapColorRamp(shp, "value", breaks = b, output.breaks = TRUE)
    expect_length(result$pal, b)
  }
})

# ---------------------------------------------------------------------------
# 6. BREAK STYLES
# ---------------------------------------------------------------------------

test_that("break.style 'equal' runs without error", {
  shp <- make_spdf(n = 20, col_values = runif(20))
  expect_silent(
    mapColorRamp(shp, "value", breaks = 5, break.style = "equal")
  )
})

test_that("break.style 'jenks' runs without error", {
  shp <- make_spdf(n = 20, col_values = runif(20))
  expect_silent(
    mapColorRamp(shp, "value", breaks = 5, break.style = "jenks")
  )
})

test_that("break.style 'pretty' runs without error", {
  shp <- make_spdf(n = 20, col_values = runif(20))
  expect_silent(
    mapColorRamp(shp, "value", breaks = 5, break.style = "pretty")
  )
})

# ---------------------------------------------------------------------------
# 7. PALETTE OPTIONS
# ---------------------------------------------------------------------------

test_that("palette 'Blues' runs without error", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  expect_silent(
    mapColorRamp(shp, "value", breaks = 5, palette = "Blues")
  )
})

test_that("reverse=TRUE runs without error and changes palette order", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  r_normal  <- mapColorRamp(shp, "value", breaks = 5,
                            palette = "Oranges", reverse = FALSE,
                            output.breaks = TRUE)
  r_reverse <- mapColorRamp(shp, "value", breaks = 5,
                            palette = "Oranges", reverse = TRUE,
                            output.breaks = TRUE)
  expect_false(identical(r_normal$pal, r_reverse$pal))
})

test_that("colorblind=TRUE runs without error", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  expect_silent(
    mapColorRamp(shp, "value", breaks = 5, colorblind = TRUE)
  )
})

# ---------------------------------------------------------------------------
# 8. LEGEND DIGITS
# ---------------------------------------------------------------------------

test_that("legend.digits controls decimal places in labels", {
  shp <- make_spdf(n = 20, col_values = seq(0.1111, 2.2222, length.out = 20))

  r0 <- mapColorRamp(shp, "value", breaks = 4,
                     legend.digits = 0, output.breaks = TRUE)
  r4 <- mapColorRamp(shp, "value", breaks = 4,
                     legend.digits = 4, output.breaks = TRUE)

  # Higher digit count should produce longer label strings
  max_len_0 <- max(nchar(r0$legend.labs))
  max_len_4 <- max(nchar(r4$legend.labs))
  expect_gte(max_len_4, max_len_0)
})

# ---------------------------------------------------------------------------
# 9. add.to.map FLAG
# ---------------------------------------------------------------------------

test_that("add.to.map=TRUE overlays without error when map already exists", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  # Draw base map first
  sp::plot(shp)
  expect_silent(
    mapColorRamp(shp, "value", breaks = 5, add.to.map = TRUE)
  )
})

# ---------------------------------------------------------------------------
# 10. EDGE CASES
# ---------------------------------------------------------------------------

test_that("all-NA column is handled (with warning from classIntervals)", {
  vals <- rep(NA_real_, 20)
  shp <- make_spdf(n = 20, col_values = vals)
  # classIntervals will warn/error on all-NA; function should propagate that
  # gracefully — we just confirm it does not produce an unexpected hard crash
  # that is unrelated to the NA issue.
  expect_error(
    mapColorRamp(shp, "value", breaks = 5)
  )
})

test_that("constant (zero-variance) column errors with single unique value", {
  shp <- make_spdf(n = 20, col_values = rep(42, 20))
  # classInt::classIntervals() cannot compute breaks on a constant vector;
  # mapColorRamp propagates this error.
  expect_error(
    mapColorRamp(shp, "value", breaks = 5, break.style = "equal")
  )
})

test_that("minimum valid n=3 breaks works", {
  shp <- make_spdf(n = 20, col_values = 1:20)
  result <- mapColorRamp(shp, "value", breaks = 3, output.breaks = TRUE)
  expect_length(result$pal, 3)
})

test_that("maximum valid n=9 breaks works", {
  shp <- make_spdf(n = 30, col_values = 1:30)
  result <- mapColorRamp(shp, "value", breaks = 9, output.breaks = TRUE)
  expect_length(result$pal, 9)
})
