test_that("truffle_demographics appends age and gender", {
  df <- data.frame(id = 1:20)
  out <- truffle_demographics(df, gender_probs = c(male = .4, female = .6))
  expect_true(all(c("id", "age", "gender") %in% names(out)))
  expect_equal(nrow(out), 20)
  expect_true(all(out$gender %in% c("male", "female")))
})

test_that("truffle_demographics places age and gender right after id", {
  df <- data.frame(id = 1:5, score = 1:5)
  out <- truffle_demographics(df)
  expect_equal(names(out)[1:3], c("id", "age", "gender"))
})

test_that("truffle_demographics keeps ages within the requested bounds", {
  df <- data.frame(id = 1:100)
  out <- truffle_demographics(df, age_min = 20, age_max = 30)
  expect_true(all(out$age >= 20 & out$age <= 30))
})

test_that("truffle_demographics requires a named gender_probs vector", {
  df <- data.frame(id = 1:5)
  expect_error(truffle_demographics(df, gender_probs = c(0.5, 0.5)), "named")
})

test_that("truffle_demographics moves age and gender to the front when id is absent", {
  df <- data.frame(score = 1:5)
  out <- truffle_demographics(df)
  expect_equal(names(out)[1:2], c("age", "gender"))
})
