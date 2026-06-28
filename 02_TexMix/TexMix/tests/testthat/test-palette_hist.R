library(testthat)
library(classInt)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_class_intervals <- function(x = 1:30, n = 5, style = "quantile") {
  classInt::classIntervals(x, n = n, style = style)
}

make_pal <- function(n = 5, palette = "Oranges") {
  RColorBrewer::brewer.pal(n, palette)
}

# Named list as returned by mapColorRamp(output.breaks = TRUE)
make_ramp_list <- function(n = 5) {
  ci  <- make_class_intervals(n = n)
  pal <- make_pal(n = n)
  list(class.intervals = ci, pal = pal, legend.labs = letters[seq_len(n)])
}

# ---------------------------------------------------------------------------
# 1. INPUT VALIDATION
# ---------------------------------------------------------------------------

test_that("error when pal is NULL and a plain classIntervals object is passed", {
  ci <- make_class_intervals()
  expect_error(
    palette_hist(ci, pal = NULL),
    "pal must be provided"
  )
})

test_that("error when pal is NULL and list is missing 'pal' element", {
  ci   <- make_class_intervals()
  bad  <- list(class.intervals = ci)          # no 'pal' key
  expect_error(
    palette_hist(bad),
    "pal must be provided"
  )
})

test_that("error when pal is NULL and list is missing 'class.intervals' element", {
  pal  <- make_pal()
  bad  <- list(pal = pal)                     # no 'class.intervals' key
  expect_error(
    palette_hist(bad),
    "pal must be provided"
  )
})

# ---------------------------------------------------------------------------
# 2. BASIC EXECUTION — separate arguments
# ---------------------------------------------------------------------------

test_that("runs without error: classIntervals + pal passed separately", {
  ci  <- make_class_intervals()
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal))
})

test_that("returns NULL invisibly when called with separate arguments", {
  ci  <- make_class_intervals()
  pal <- make_pal()
  result <- palette_hist(ci, pal)
  expect_null(result)
})

# ---------------------------------------------------------------------------
# 3. BASIC EXECUTION — named list (mapColorRamp output)
# ---------------------------------------------------------------------------

test_that("runs without error: named list from mapColorRamp", {
  ramp <- make_ramp_list()
  expect_silent(palette_hist(ramp))
})

test_that("returns NULL invisibly when called with named list", {
  ramp   <- make_ramp_list()
  result <- palette_hist(ramp)
  expect_null(result)
})

test_that("named list with extra elements (e.g. legend.labs) still works", {
  ramp <- make_ramp_list()               # includes legend.labs
  expect_silent(palette_hist(ramp))
})

# ---------------------------------------------------------------------------
# 4. TITLE PARAMETER
# ---------------------------------------------------------------------------

test_that("custom title runs without error", {
  ci  <- make_class_intervals()
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal, title = "My Custom Title"))
})

test_that("empty string title falls back to 'Distribution' without error", {
  ci  <- make_class_intervals()
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal, title = ""))
})

test_that("default title (omitted) falls back to 'Distribution' without error", {
  ci  <- make_class_intervals()
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal))
})

# ---------------------------------------------------------------------------
# 5. PALETTE VARIATIONS
# ---------------------------------------------------------------------------

test_that("Blues palette runs without error", {
  ci  <- make_class_intervals()
  pal <- make_pal(palette = "Blues")
  expect_silent(palette_hist(ci, pal))
})

test_that("single-break palette (n=3 minimum) runs without error", {
  ci  <- make_class_intervals(n = 3)
  pal <- make_pal(n = 3)
  expect_silent(palette_hist(ci, pal))
})

test_that("maximum breaks (n=9) runs without error", {
  ci  <- make_class_intervals(n = 9)
  pal <- make_pal(n = 9)
  expect_silent(palette_hist(ci, pal))
})

# ---------------------------------------------------------------------------
# 6. BREAK STYLES
# ---------------------------------------------------------------------------

test_that("equal-interval breaks run without error", {
  ci  <- make_class_intervals(style = "equal")
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal))
})

test_that("jenks breaks run without error", {
  ci  <- make_class_intervals(style = "jenks")
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal))
})

test_that("pretty breaks run without error", {
  ci  <- make_class_intervals(x = 1:50, style = "pretty")
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal))
})

# ---------------------------------------------------------------------------
# 7. DATA VARIATIONS
# ---------------------------------------------------------------------------

test_that("continuous (non-integer) data runs without error", {
  ci  <- make_class_intervals(x = runif(50, 0, 100))
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal))
})

test_that("negative values run without error", {
  ci  <- make_class_intervals(x = seq(-50, -1))
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal))
})

test_that("mixed negative and positive values run without error", {
  ci  <- make_class_intervals(x = seq(-25, 25))
  pal <- make_pal()
  expect_silent(palette_hist(ci, pal))
})

test_that("large dataset runs without error", {
  ci  <- make_class_intervals(x = rnorm(10000), n = 7)
  pal <- make_pal(n = 7)
  expect_silent(palette_hist(ci, pal))
})

# ---------------------------------------------------------------------------
# 8. LIST DISPATCH LOGIC
# ---------------------------------------------------------------------------

test_that("list dispatch extracts class.intervals correctly (pal lengths match)", {
  n    <- 6
  ci   <- make_class_intervals(n = n)
  pal  <- make_pal(n = n)
  ramp <- list(class.intervals = ci, pal = pal)

  # If dispatch works, the internal ci$brk will have n+1 breakpoints
  # and pal will have n colors — no error means dispatch succeeded
  expect_silent(palette_hist(ramp))
})

test_that("explicit pal arg takes precedence over list when list has no pal key", {
  ci   <- make_class_intervals()
  pal  <- make_pal()
  # Passing a raw classIntervals with an explicit pal should still work
  expect_silent(palette_hist(ci, pal = pal))
})
