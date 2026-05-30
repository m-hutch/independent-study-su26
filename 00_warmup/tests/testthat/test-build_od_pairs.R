library(testthat)

# ==============================================================================
# Shared fixtures
# ==============================================================================

make_small_df <- function() {
  data.frame(
    pop  = c(100L, 200L, 300L),
    cost = c(1.5, 2.5, 3.5),
    type = factor(c("urban", "rural", "urban"))
  )
}

make_df_with_nonpositive <- function() {
  data.frame(
    pop   = c(100L, 200L, 300L),
    score = c(-1.0, 0.0, 3.5)   # non-positive: log transform not applicable
  )
}

make_square_matrix <- function(df) {
  n <- nrow(df)
  matrix(seq_len(n^2), nrow = n)
}

# ==============================================================================
# Output dimensions and structure
# ==============================================================================

test_that("output has n^2 rows", {
  df  <- make_small_df()
  out <- build_od_pairs(df)
  expect_equal(nrow(out), nrow(df)^2)
})

test_that("output contains ID.i and ID.j columns", {
  out <- build_od_pairs(make_small_df())
  expect_true(all(c("ID.i", "ID.j") %in% names(out)))
})

test_that("ID.i and ID.j span the full 1:n range", {
  df  <- make_small_df()
  out <- build_od_pairs(df)
  expect_equal(sort(unique(out$ID.i)), 1:nrow(df))
  expect_equal(sort(unique(out$ID.j)), 1:nrow(df))
})

test_that("all n^2 origin-destination combinations are present", {
  df   <- make_small_df()
  out  <- build_od_pairs(df)
  pairs <- paste(out$ID.i, out$ID.j)
  expect_equal(length(pairs), nrow(df)^2)
  expect_equal(length(unique(pairs)), nrow(df)^2)
})

# ==============================================================================
# Diagonal (i == j) pairs set to NA
# ==============================================================================

test_that("variable columns are NA for diagonal (i == j) pairs", {
  df       <- make_small_df()
  out      <- build_od_pairs(df)
  self     <- out$ID.i == out$ID.j
  var_cols <- !names(out) %in% c("ID.i", "ID.j", "mij", "dij")
  expect_true(all(is.na(out[self, var_cols])))
})

test_that("ID.i and ID.j are NOT NA for diagonal pairs", {
  out  <- build_od_pairs(make_small_df())
  self <- out$ID.i == out$ID.j
  expect_false(any(is.na(out$ID.i[self])))
  expect_false(any(is.na(out$ID.j[self])))
})

test_that("off-diagonal variable columns are not all NA", {
  df       <- make_small_df()
  out      <- build_od_pairs(df)
  off_diag <- out$ID.i != out$ID.j
  expect_false(all(is.na(out$pop.i[off_diag])))
})

# ==============================================================================
# mij argument
# ==============================================================================

test_that("mij column is present and all NA when mij not provided", {
  out <- build_od_pairs(make_small_df())
  expect_true("mij" %in% names(out))
  expect_true(all(is.na(out$mij)))
})

test_that("mij values are flattened correctly when provided", {
  df  <- make_small_df()
  mij <- make_square_matrix(df)
  out <- build_od_pairs(df, mij = mij)
  expect_equal(out$mij, as.vector(mij))
})

test_that("mij must be an n x n matrix", {
  df  <- make_small_df()
  bad <- matrix(1:4, nrow = 2)
  expect_error(build_od_pairs(df, mij = bad))
})

# ==============================================================================
# dij argument
# ==============================================================================

test_that("dij column is present and all NA when dij not provided", {
  out <- build_od_pairs(make_small_df())
  expect_true("dij" %in% names(out))
  expect_true(all(is.na(out$dij)))
})

test_that("dij values are flattened correctly when provided", {
  df  <- make_small_df()
  dij <- make_square_matrix(df) * 0.5
  out <- build_od_pairs(df, dij = dij)
  expect_equal(out$dij, as.vector(dij))
})

test_that("dij must be an n x n matrix", {
  df  <- make_small_df()
  bad <- matrix(1:4, nrow = 2)
  expect_error(build_od_pairs(df, dij = bad))
})

# ==============================================================================
# Factor columns
# ==============================================================================

test_that("factor columns produce .iF and .jF suffixed columns", {
  out <- build_od_pairs(make_small_df())
  expect_true(all(c("type.iF", "type.jF") %in% names(out)))
})

test_that("factor output columns are of factor type", {
  out <- build_od_pairs(make_small_df())
  expect_true(is.factor(out$type.iF))
  expect_true(is.factor(out$type.jF))
})

test_that("factor levels are preserved in output", {
  df  <- make_small_df()
  out <- build_od_pairs(df)
  expect_equal(levels(out$type.iF), levels(df$type))
})

# ==============================================================================
# Numeric columns - no transform
# ==============================================================================

test_that("no transform produces .i and .j columns for positive vars", {
  out <- build_od_pairs(make_small_df(), transform = "none")
  expect_true(all(c("pop.i", "pop.j") %in% names(out)))
})

test_that("no transform produces .ijL log-ratio column for positive vars", {
  out <- build_od_pairs(make_small_df(), transform = "none")
  expect_true("pop.ijL" %in% names(out))
})

test_that("non-positive columns produce only .i and .j columns (no log ratio)", {
  df  <- make_df_with_nonpositive()
  out <- build_od_pairs(df, transform = "none")
  expect_true(all(c("score.i", "score.j") %in% names(out)))
  expect_false("score.ijL" %in% names(out))
})

# ==============================================================================
# Numeric columns - log transform
# ==============================================================================

test_that("log transform produces .iL and .jL columns", {
  out <- build_od_pairs(make_small_df(), transform = "log")
  expect_true(all(c("pop.iL", "pop.jL") %in% names(out)))
})

test_that("log transform values equal log of original values", {
  df       <- make_small_df()
  out_none <- build_od_pairs(df, transform = "none")
  out_log      <- build_od_pairs(df, transform = "log")
  off_diag <- out_none$ID.i != out_none$ID.j
  expect_equal(out_log$pop.iL[off_diag], log(out_none$pop.i[off_diag]), 
               ignore_attr = TRUE)
})

# ==============================================================================
# Numeric columns - standard (z) transform
# ==============================================================================

test_that("standard transform produces .iZ and .jZ columns", {
  out <- build_od_pairs(make_small_df(), transform = "standard")
  expect_true(all(c("pop.iZ", "pop.jZ") %in% names(out)))
})

# ==============================================================================
# Numeric columns - log.standard transform
# ==============================================================================

test_that("log.standard transform produces .iLZ and .jLZ columns", {
  out <- build_od_pairs(make_small_df(), transform = "log.standard")
  expect_true(all(c("pop.iLZ", "pop.jLZ") %in% names(out)))
})

# ==============================================================================
# Input validation
# ==============================================================================

test_that("df must be a data frame", {
  expect_error(build_od_pairs(list(a = 1:3)))
})

test_that("invalid transform string is rejected", {
  expect_error(build_od_pairs(make_small_df(), transform = "bad"))
})

test_that("transform must be one of the four valid strings", {
  df <- make_small_df()
  expect_no_error(build_od_pairs(df, transform = "none"))
  expect_no_error(build_od_pairs(df, transform = "log"))
  expect_no_error(build_od_pairs(df, transform = "standard"))
  expect_no_error(build_od_pairs(df, transform = "log.standard"))
})
