test_that("truffle_check returns Cohen's d, a correlation matrix, and a ggplot", {
  d <- truffle_likert(
    n_per_condition = 40,
    n_items = c(4, 4),
    alpha = c(.7, .8),
    approx_d_between_groups = 0.5,
    n_levels = 5,
    seed = 42
  )
  out <- truffle_check(d)
  expect_named(out, c("cohens_d", "r", "item_histograms"))
  expect_length(out$cohens_d, 2)
  expect_true(is.matrix(out$r))
  expect_equal(dim(out$r), c(2, 2))
  expect_s3_class(out$item_histograms, "ggplot")
})

test_that("truffle_check requires a two-level condition column", {
  d <- data.frame(id = 1:4, X1_item1 = 1:4)
  expect_error(truffle_check(d), "condition")

  d$condition <- c("a", "a", "a", "a")
  expect_error(truffle_check(d), "exactly two")
})
