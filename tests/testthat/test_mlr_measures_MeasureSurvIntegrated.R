context("MeasureSurvIntegrated")

test_that("constructor", {
  expect_error(MeasureSurvIntegrated$new(), "abstract")
  expect_error(MeasureSurvGraf$new(integrated = FALSE), "non-integrated")
  expect_silent(MeasureSurvGraf$new(integrated = FALSE, times = 60))
})

test_that("fields", {
  m = MeasureSurvGraf$new(times = 32, integrated = TRUE)
  expect_equal(m$times, 32)
  expect_false(m$integrated)
  expect_silent({
    m$times = 34
  })
  expect_silent({
    m$integrated = TRUE
  })
  expect_silent({
    m$times = c(34, 60)
  })
  expect_error({
    m$integrated = "D"
  }, "logical flag")
  expect_error({
    m$integrated = FALSE
  }, "non-integrated score")
  expect_silent({
    m$times = 34
  })
  expect_silent({
    m$integrated = FALSE
  })
  expect_error({
    m$times = c(34, 60)
  }, "non-integrated score")
  expect_silent({
    m$times = 34
  })
})
