context("helpers")

test_that("format_range", {
  expect_equal(format_range(c(0,10)), "\\eqn{[0, 10]}{[0, 10]}")
  expect_equal(format_range(c(-Inf,Inf)), "\\eqn{(-\\infty, \\infty)}{(-Inf, Inf)}")
})

test_that("format_types", {
  expect_equal(format_types(letters[1:3]), "a, b, c")
})
