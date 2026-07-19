test_that("truffle_sum_scores_by_scale appends one sum column per scale", {
  df <- data.frame(
    id = 1:3,
    X1_item1 = c(1, 2, 3),
    X1_item2 = c(4, 5, 6),
    X2_item1 = c(1, 1, 1),
    X2_item2 = c(2, 2, 2)
  )
  out <- truffle_sum_scores_by_scale(df)
  expect_true(all(c("X1_sum", "X2_sum") %in% names(out)))
  # X2 sum = 1 + 2 = 3 for every row
  expect_equal(out$X2_sum, c(3, 3, 3))
})

test_that("truffle_sum_scores_by_scale prorates over missing items", {
  df <- data.frame(
    X1_item1 = c(2, NA),
    X1_item2 = c(4, 4)
  )
  out <- truffle_sum_scores_by_scale(df, prorate = TRUE)
  # row 1: 2 + 4 = 6; row 2: only one item (4), prorated to 4 * 2 / 1 = 8
  expect_equal(out$X1_sum, c(6, 8))
})

test_that("truffle_sum_scores_by_scale can append non-missing counts", {
  df <- data.frame(X1_item1 = c(1, NA), X1_item2 = c(2, 2))
  out <- truffle_sum_scores_by_scale(df, add_counts = TRUE)
  expect_true("X1_n" %in% names(out))
  expect_equal(out$X1_n, c(2, 1))
})

test_that("truffle_sum_scores_by_scale errors when no columns match", {
  expect_error(
    truffle_sum_scores_by_scale(data.frame(a = 1, b = 2)),
    "No columns match"
  )
})
