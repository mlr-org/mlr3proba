test_that("surv matrix asserts", {
  x = matrix(data = c(1,0.6,0.4,0.8,0.8,0.7), nrow = 2, ncol = 3, byrow = TRUE)
  # no time points specified
  expect_error(assert_surv_matrix(x), "column names must be increasing")

  # time points specified, but not numeric
  colnames(x) = letters[1:3]
  expect_error(suppressWarnings(assert_surv_matrix(x)), "Contains missing values")

  # time points specified and numeric
  colnames(x) = c(12, 34, 42)
  expect_silent(assert_surv_matrix(x))

  # S(t) >= S(t+1)
  x[2,3] = 0.81
  expect_error(assert_surv_matrix(x), "Survival probabilities must be")
})
