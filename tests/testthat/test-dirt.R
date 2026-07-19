items_df <- function(n = 20) {
  data.frame(
    id = seq_len(n),
    X1_item1 = sample(1:5, n, replace = TRUE),
    X1_item2 = sample(1:5, n, replace = TRUE)
  )
}

test_that("dirt_impossible_values injects the replacement value into X columns", {
  set.seed(1)
  df <- items_df(100)
  out <- dirt_impossible_values(df, prop = 0.5, replacement_value = 99L)
  expect_equal(dim(out), dim(df))
  expect_true(any(out$X1_item1 == 99 | out$X1_item2 == 99))
  # non-X columns are untouched
  expect_equal(out$id, df$id)
})

test_that("dirt_missingness introduces missing values", {
  set.seed(2)
  df <- items_df(100)
  out <- dirt_missingness(df, prop = 0.2, dirtier = FALSE)
  expect_equal(nrow(out), nrow(df))
  expect_true(anyNA(out))
})

test_that("dirt_duplicates appends duplicated rows", {
  df <- items_df(20)
  out <- dirt_duplicates(df, prop = 0.5)
  expect_gt(nrow(out), nrow(df))
  expect_equal(ncol(out), ncol(df))
})

test_that("dirt_duplicates with prop 0 returns the data unchanged", {
  df <- items_df(10)
  expect_equal(nrow(dirt_duplicates(df, prop = 0)), nrow(df))
})

test_that("dirt_numbers formats numeric columns as character with units", {
  df <- data.frame(rt = c(1234, 5678))
  out <- dirt_numbers(df, cols = "rt", unit = " ms")
  expect_type(out$rt, "character")
  expect_true(all(grepl(" ms$", out$rt)))
  expect_true(any(grepl(",", out$rt)))
})

test_that("dirt_dates appends a character date column with mixed formats", {
  df <- data.frame(id = 1:50)
  out <- dirt_dates(df, col = "date", seed = 1)
  expect_true("date" %in% names(out))
  expect_type(out$date, "character")
})

test_that("dirt_untidy appends a combined block/trial column", {
  df <- data.frame(id = 1:5)
  out <- dirt_untidy(df)
  expect_true("block_trial" %in% names(out))
  expect_true(all(grepl("^block\\d+_trial\\d{3}$", out$block_trial)))
})

test_that("dirt_colnames corrupts names and records a name_map", {
  df <- data.frame(id = 1:3, extroversion_sum = 1:3, reaction_time_ms = 1:3)
  out <- dirt_colnames(df, prop = 1, seed = 123)
  expect_equal(nrow(out), nrow(df))
  nm <- attr(out, "name_map")
  expect_s3_class(nm, "data.frame")
  expect_equal(nm$old, names(df))
})

test_that("dirt_header_rows prepends header lines to the CSV body", {
  df <- data.frame(id = 1:3, score = c(10, 20, 15))
  out <- dirt_header_rows(df, headers = c("line one", "line two"))
  expect_type(out, "character")
  expect_equal(out[1:2], c("line one", "line two"))
  # the original column header follows the prepended lines
  expect_match(out[3], "id")
})
