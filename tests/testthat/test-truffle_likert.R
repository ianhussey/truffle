test_that("truffle_likert returns a two-group data frame with expected structure", {
  d <- truffle_likert(
    n_per_condition = 20,
    n_items = c(4, 4),
    alpha = c(.7, .8),
    approx_d_between_groups = 0.5,
    n_levels = 5,
    seed = 42
  )
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 40)
  expect_true(all(c("id", "condition") %in% names(d)))
  expect_setequal(unique(d$condition), c("control", "treatment"))
  # 2 scales x 4 items = 8 item columns
  expect_equal(sum(grepl("^X\\d+_item\\d+$", names(d))), 8)
})

test_that("truffle_likert item responses stay within the Likert range", {
  d <- truffle_likert(
    n_per_condition = 30,
    n_items = 5,
    alpha = .7,
    approx_d_between_groups = 0.4,
    n_levels = 7,
    seed = 1
  )
  items <- d[grepl("^X1_item\\d+$", names(d))]
  vals <- unlist(items)
  expect_true(all(vals >= 1 & vals <= 7))
})

test_that("truffle_likert is reproducible with a seed", {
  args <- list(
    n_per_condition = 10,
    n_items = 4,
    alpha = .7,
    approx_d_between_groups = 0.5,
    n_levels = 5,
    seed = 123
  )
  expect_equal(do.call(truffle_likert, args), do.call(truffle_likert, args))
})

test_that("truffle_likert supports custom condition names", {
  d <- truffle_likert(
    n_per_condition = 10,
    n_items = 4,
    alpha = .7,
    approx_d_between_groups = 0.3,
    n_levels = 5,
    condition_names = c("placebo", "drug"),
    seed = 2
  )
  expect_setequal(unique(d$condition), c("placebo", "drug"))
})

test_that("truffle_likert supports a cross-sectional design", {
  d <- truffle_likert(
    study_design = "crosssectional",
    n_per_condition = 50,
    n_items = c(4, 4),
    alpha = .75,
    r_among_outcomes = 0.3,
    n_levels = 5,
    seed = 9
  )
  expect_equal(nrow(d), 50)
  expect_true("id" %in% names(d))
  expect_false("condition" %in% names(d))
})

test_that("truffle_likert validates study_design and n_items", {
  expect_error(
    truffle_likert(study_design = "nope", n_items = 4),
    "study_design"
  )
  expect_error(
    truffle_likert(n_items = 1, approx_d_between_groups = 0.5),
    regexp = NULL
  )
})
