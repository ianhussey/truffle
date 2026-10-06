test_that("round_p_value formats in APA style and drops the leading zero", {
  expect_equal(round_p_value(0.023), ".023")
  expect_equal(round_p_value(0.5), ".500")
})

test_that("round_p_value thresholds small values", {
  expect_equal(round_p_value(0.0004), "< .001")
  expect_equal(round_p_value(0.00004, digits = 4), "< .0001")
})

test_that("round_p_value never reports 1 and passes through NA", {
  expect_equal(round_p_value(c(0.9994, 0.9995, 0.9996, 1)),
               c(".999", "> .999", "> .999", "> .999"))
  expect_equal(round_p_value(1, digits = 2), "> .99")
  expect_equal(round_p_value(1, decimal_separator = ","), "> ,999")
  expect_true(is.na(round_p_value(NA_real_)))
})

test_that("round_p_value errors on values outside [0, 1]", {
  expect_error(round_p_value(1.5), "between 0 and 1")
  expect_error(round_p_value(c(0.5, -0.01)), "between 0 and 1")
  expect_no_error(round_p_value(c(0, 1, NA)))
})

test_that("round_p_value honours a custom decimal separator", {
  expect_equal(round_p_value(0.023, decimal_separator = ","), ",023")
})

test_that("round_p_value is vectorised", {
  out <- round_p_value(c(0.023, 0.0004, 0.5))
  expect_equal(out, c(".023", "< .001", ".500"))
})

test_that("round_p_value rounds half up despite floating-point representation", {
  expect_equal(
    round_p_value(c(0.1235, 0.0445, 0.0045, 0.0125, 0.0499), alpha = NULL),
    c(".124", ".045", ".005", ".013", ".050")
  )
  expect_equal(round_p_value(0.01235, digits = 4), ".0124")
})

test_that("round_p_value adds digits rather than rounding across alpha", {
  expect_equal(round_p_value(0.0499), ".0499")
  expect_equal(round_p_value(0.04996), ".04996")
  expect_equal(round_p_value(0.0495), ".0495")
  expect_equal(round_p_value(c(0.05, 0.0501, 0.0504)), c(".050", ".050", ".050"))
  expect_equal(round_p_value(0.049), ".049")
  expect_equal(round_p_value(0.0099, alpha = .01), ".0099")
  expect_equal(round_p_value(0.0499, decimal_separator = ","), ",0499")
  expect_equal(round_p_value(c(0.0499, NA, 0.2)), c(".0499", NA, ".200"))
})
