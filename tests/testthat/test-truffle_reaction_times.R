test_that("truffle_reaction_times returns n positive, whole-number RTs", {
  rt <- truffle_reaction_times(n = 50, seed = 1)
  expect_length(rt, 50)
  expect_true(all(rt > 0))
  expect_equal(rt, round(rt))
})

test_that("truffle_reaction_times is reproducible with a seed", {
  expect_identical(
    truffle_reaction_times(n = 10, seed = 42),
    truffle_reaction_times(n = 10, seed = 42)
  )
})

test_that("truffle_reaction_times respects the nondecision-time shift", {
  rt <- truffle_reaction_times(n = 100, shift = 300, seed = 7)
  expect_true(all(rt >= 300))
})

test_that("truffle_reaction_times rejects non-positive n", {
  expect_error(truffle_reaction_times(n = 0))
})
