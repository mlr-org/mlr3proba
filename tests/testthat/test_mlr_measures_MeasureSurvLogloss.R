context("Logloss")

test_that("logloss", {
  expect_silent({
    l = MeasureSurvLogloss$new(eps = 0.001)
  })
  expect_error({
    l$eps = "a"
  })
  expect_silent({
    l$eps = 0.0001
  })
  expect_equal(l$eps, 0.0001)
})

test_that("intlogloss", {
  expect_silent({
    l = MeasureSurvIntLogloss$new(eps = 0.001)
  })
  expect_error({
    l$eps = "a"
  })
  expect_silent({
    l$eps = 0.0001
  })
  expect_equal(l$eps, 0.0001)
})

test_that("intloglossse", {
  expect_silent({
    l = msr("surv.intlogloss", se = TRUE)
  })
  expect_error({
    l$eps = "a"
  })
  expect_silent({
    l$eps = 0.0001
  })
  expect_equal(l$eps, 0.0001)
})
