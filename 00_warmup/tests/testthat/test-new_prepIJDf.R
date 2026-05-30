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
    score = c(-1.0, 0.0, 3.5)
  )
}

make_square_matrix <- function(df) {
  n <- nrow(df)
  matrix(seq_len(n^2), nrow = n)
}

# Helper: strip row names for comparison
strip_attrs <- function(df) {
  rownames(df) <- NULL
  df
}

# ==============================================================================
# Output dimensions and structure
# ==============================================================================

test_that("output has n^2 rows", {
  df  <- make_small_df()
  out <- new_prepIJDf(df)
  expect_equal(nrow(out), nrow(df)^2)
})

test_that("output contains ID.i, ID.j, ID.ij columns", {
  out <- new_prepIJDf(make_small_df())
  expect_true(all(c("ID.i", "ID.j", "ID.ij") %in% names(out)))
})

test_that("ID.ij values are unique", {
  out <- new_prepIJDf(make_small_df())
  expect_equal(length(unique(out$ID.ij)), nrow(out))
})

# ==============================================================================
# mij argument
# ==============================================================================

test_that("mij column is NA when not provided", {
  out <- new_prepIJDf(make_small_df())
  expect_true(all(is.na(out$mij)))
})

test_that("mij values are flattened correctly when provided", {
  df  <- make_small_df()
  mij <- make_square_matrix(df)
  out <- new_prepIJDf(df, mij = as.data.frame(mij))
  expect_false(any(is.na(out$mij)))
})

# ==============================================================================
# dij argument
# ==============================================================================

test_that("dij column is NA when not provided", {
  out <- new_prepIJDf(make_small_df())
  expect_true(all(is.na(out$dij)))
})

# ==============================================================================
# Transform mapping from logTrans/zTrans booleans
# ==============================================================================

test_that("logTrans=FALSE, zTrans=FALSE produces .i and .j columns", {
  out <- new_prepIJDf(make_small_df(), logTrans = FALSE, zTrans = FALSE)
  expect_true(all(c("pop.i", "pop.j") %in% names(out)))
})

test_that("logTrans=TRUE, zTrans=FALSE produces .iL and .jL columns", {
  out <- new_prepIJDf(make_small_df(), logTrans = TRUE, zTrans = FALSE)
  expect_true(all(c("pop.iL", "pop.jL") %in% names(out)))
})

test_that("logTrans=FALSE, zTrans=TRUE produces .iZ and .jZ columns", {
  out <- new_prepIJDf(make_small_df(), logTrans = FALSE, zTrans = TRUE)
  expect_true(all(c("pop.iZ", "pop.jZ") %in% names(out)))
})

test_that("logTrans=TRUE, zTrans=TRUE produces .iLZ and .jLZ columns", {
  out <- new_prepIJDf(make_small_df(), logTrans = TRUE, zTrans = TRUE)
  expect_true(all(c("pop.iLZ", "pop.jLZ") %in% names(out)))
})

# ==============================================================================
# Factor columns
# ==============================================================================

test_that("factor columns produce .iF and .jF columns", {
  out <- new_prepIJDf(make_small_df())
  expect_true(all(c("type.iF", "type.jF") %in% names(out)))
  expect_true(is.factor(out$type.iF))
})

# ==============================================================================
# Input validation
# ==============================================================================

test_that("df must be a data frame", {
  expect_error(new_prepIJDf(list(a = 1:3)))
})

test_that("logTrans must be logical", {
  expect_error(new_prepIJDf(make_small_df(), logTrans = "yes"))
})

test_that("zTrans must be logical", {
  expect_error(new_prepIJDf(make_small_df(), zTrans = 1))
})

# ==============================================================================
# Parity with original prepIJDf
# ==============================================================================

test_that("column names match original for no transform", {
  df      <- make_small_df()
  old_out <- prepIJDf(df, logTrans = FALSE, zTrans = FALSE)
  new_out <- new_prepIJDf(df, logTrans = FALSE, zTrans = FALSE)
  expect_equal(names(new_out), names(old_out))
})

test_that("column names match original for log transform", {
  df      <- make_small_df()
  old_out <- prepIJDf(df, logTrans = TRUE, zTrans = FALSE)
  new_out <- new_prepIJDf(df, logTrans = TRUE, zTrans = FALSE)
  expect_equal(names(new_out), names(old_out))
})

test_that("column names match original for z transform", {
  df      <- make_small_df()
  old_out <- prepIJDf(df, logTrans = FALSE, zTrans = TRUE)
  new_out <- new_prepIJDf(df, logTrans = FALSE, zTrans = TRUE)
  expect_equal(names(new_out), names(old_out))
})

test_that("column names match original for log+z transform", {
  df      <- make_small_df()
  old_out <- prepIJDf(df, logTrans = TRUE, zTrans = TRUE)
  new_out <- new_prepIJDf(df, logTrans = TRUE, zTrans = TRUE)
  expect_equal(names(new_out), names(old_out))
})

test_that("numeric values match original for no transform", {
  df      <- make_small_df()
  old_out <- strip_attrs(prepIJDf(df, logTrans = FALSE, zTrans = FALSE))
  new_out <- strip_attrs(new_prepIJDf(df, logTrans = FALSE, zTrans = FALSE))
  numeric_cols <- names(old_out)[sapply(old_out, is.numeric)]
  expect_equal(new_out[numeric_cols], old_out[numeric_cols], tolerance = 1e-10)
})

test_that("numeric values match original for log transform", {
  df      <- make_small_df()
  old_out <- strip_attrs(prepIJDf(df, logTrans = TRUE, zTrans = FALSE))
  new_out <- strip_attrs(new_prepIJDf(df, logTrans = TRUE, zTrans = FALSE))
  numeric_cols <- names(old_out)[sapply(old_out, is.numeric)]
  expect_equal(new_out[numeric_cols], old_out[numeric_cols], tolerance = 1e-10)
})

test_that("numeric values match original for z transform", {
  df      <- make_small_df()
  old_out <- strip_attrs(prepIJDf(df, logTrans = FALSE, zTrans = TRUE))
  new_out <- strip_attrs(new_prepIJDf(df, logTrans = FALSE, zTrans = TRUE))
  numeric_cols <- names(old_out)[sapply(old_out, is.numeric)]
  expect_equal(new_out[numeric_cols], old_out[numeric_cols], tolerance = 1e-10)
})

test_that("numeric values match original for log+z transform", {
  df      <- make_small_df()
  old_out <- strip_attrs(prepIJDf(df, logTrans = TRUE, zTrans = TRUE))
  new_out <- strip_attrs(new_prepIJDf(df, logTrans = TRUE, zTrans = TRUE))
  numeric_cols <- names(old_out)[sapply(old_out, is.numeric)]
  expect_equal(new_out[numeric_cols], old_out[numeric_cols], tolerance = 1e-10)
})

test_that("factor columns match original", {
  df      <- make_small_df()
  old_out <- prepIJDf(df)
  new_out <- new_prepIJDf(df)
  expect_equal(as.character(new_out$type.iF), as.character(old_out$type.iF))
  expect_equal(as.character(new_out$type.jF), as.character(old_out$type.jF))
})

test_that("ID.i and ID.j match original", {
  df      <- make_small_df()
  old_out <- prepIJDf(df)
  new_out <- new_prepIJDf(df)
  expect_equal(new_out$ID.i, old_out$ID.i)
  expect_equal(new_out$ID.j, old_out$ID.j)
})

test_that("non-positive numeric columns match original behaviour", {
  df      <- make_df_with_nonpositive()
  old_out <- prepIJDf(df, logTrans = FALSE, zTrans = FALSE)
  new_out <- new_prepIJDf(df, logTrans = FALSE, zTrans = FALSE)
  expect_equal(names(new_out), names(old_out))
  numeric_cols <- names(old_out)[sapply(old_out, is.numeric)]
  expect_equal(new_out[numeric_cols], old_out[numeric_cols], tolerance = 1e-10)
})

test_that("output has same number of rows as original", {
  df      <- make_small_df()
  old_out <- prepIJDf(df)
  new_out <- new_prepIJDf(df)
  expect_equal(nrow(new_out), nrow(old_out))
})

test_that("output has same number of columns as original", {
  df      <- make_small_df()
  old_out <- prepIJDf(df)
  new_out <- new_prepIJDf(df)
  expect_equal(ncol(new_out), ncol(old_out))
})
