test_that("round_p_value formats in APA style and drops the leading zero", {
  expect_equal(round_p_value(0.023), ".023")
  expect_equal(round_p_value(0.5), ".500")
})

test_that("round_p_value thresholds small values", {
  expect_equal(round_p_value(0.0004), "< .001")
  expect_equal(round_p_value(0.00004, digits = 4), "< .0001")
})

test_that("round_p_value caps values above 1 and passes through NA", {
  expect_equal(round_p_value(1.5), "1.000")
  expect_true(is.na(round_p_value(NA_real_)))
})

test_that("round_p_value honours a custom decimal separator", {
  expect_equal(round_p_value(0.023, decimal_separator = ","), ",023")
})

test_that("round_p_value is vectorised", {
  out <- round_p_value(c(0.023, 0.0004, 0.5))
  expect_equal(out, c(".023", "< .001", ".500"))
})
