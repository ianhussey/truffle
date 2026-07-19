make_items <- function() {
  data.frame(
    id = 1:4,
    bfi_1 = c(1, 2, 6, 3), # 6 is out of range -> NA
    bfi_2 = c(3, NA, 4, 5),
    bfi_3 = c(5, 4, 2, 1)
  )
}

test_that("snuffle_sum_scores adds the expected derived columns", {
  out <- snuffle_sum_scores(
    make_items(),
    "bfi_",
    min = 1,
    max = 5,
    id_col = "id"
  )
  expect_true(all(
    c(
      "bfi_sumscore",
      "bfi_complete_data",
      "bfi_nonmissing_n",
      "bfi_impossible_n",
      "bfi_impossible_items",
      "bfi_items",
      "bfi_items_reversed"
    ) %in%
      names(out)
  ))
})

test_that("snuffle_sum_scores flags impossible values before cleaning", {
  out <- snuffle_sum_scores(make_items(), "bfi_", min = 1, max = 5)
  # row 3 has bfi_1 = 6, which is impossible
  expect_equal(out$bfi_impossible_n[3], 1L)
  expect_equal(out$bfi_impossible_n[1], 0L)
})

test_that("snuffle_sum_scores returns NA sum scores for incomplete rows unless imputing", {
  out <- snuffle_sum_scores(
    make_items(),
    "bfi_",
    min = 1,
    max = 5,
    impute = FALSE
  )
  # row 2 has a genuine NA, row 3 has an impossible value cleaned to NA
  expect_true(is.na(out$bfi_sumscore[2]))
  expect_true(is.na(out$bfi_sumscore[3]))
  # row 1 is complete
  expect_equal(out$bfi_sumscore[1], 9)
})

test_that("snuffle_sum_scores imputes from the row mean when impute = TRUE", {
  out <- snuffle_sum_scores(
    make_items(),
    "bfi_",
    min = 1,
    max = 5,
    impute = TRUE
  )
  # row 2: observed 2 and 4 -> mean 3 * 3 items = 9
  expect_equal(out$bfi_sumscore[2], 9)
})

test_that("snuffle_sum_scores reverse-scores requested items", {
  out <- snuffle_sum_scores(
    make_items(),
    "bfi_",
    min = 1,
    max = 5,
    reverse = "bfi_3",
    impute = TRUE
  )
  expect_equal(out$bfi_items_reversed[1], "bfi_3")
})

test_that("snuffle_sum_scores aborts on duplicate ids and unmatched prefixes", {
  dupe <- make_items()
  dupe$id <- c(1, 1, 2, 3)
  expect_error(
    snuffle_sum_scores(dupe, "bfi_", min = 1, max = 5, id_col = "id"),
    "Duplicate"
  )
  expect_error(
    snuffle_sum_scores(make_items(), "zzz_", min = 1, max = 5),
    "No columns matched"
  )
})
