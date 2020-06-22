context("helpers")

test_that("format_range", {
  expect_equal(format_range(c(0, 10)), "\\eqn{[0, 10]}{[0, 10]}")
  expect_equal(format_range(c(-Inf, Inf)), "\\eqn{(-\\infty, \\infty)}{(-Inf, Inf)}")
})

test_that("format_types", {
  expect_equal(format_types(letters[1:3]), "a, b, c")
})
test_that("check_subsetpattern", {
  expect_true(check_subsetpattern("a", letters))
  expect_equal(
    check_subsetpattern(1, letters[1:3]),
    "Must be a subset of {a, b, c}, but is {1}")
})
