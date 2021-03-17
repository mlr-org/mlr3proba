context("MeasureSurvIntegrated")

test_that("constructor", {
  expect_error(MeasureSurvIntegrated$new(proper = TRUE), "abstract")
  expect_error(MeasureSurvGraf$new(integrated = FALSE, proper = TRUE), "non-integrated")
  expect_silent(MeasureSurvGraf$new(integrated = FALSE, proper = TRUE, times = 60))
})

test_that("fields", {
  m = MeasureSurvGraf$new(times = 32, integrated = TRUE, proper = TRUE)
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

test_that('training data for weights', {
  m = MeasureSurvGraf$new(proper = TRUE)
  t = tsk("rats")
  l = lrn("surv.kaplan")
  s1 = l$train(t, 1:50)$predict(t, 51:100)$score(m)
  s2 = l$train(t, 1:50)$predict(t, 51:100)$score(m, task = t, train_set = 1:50)
  expect_false(identical(s1, s2))
})

test_that('proper option', {
  set.seed(1)
  m1 = MeasureSurvGraf$new(proper = TRUE)
  m2 = suppressWarnings(MeasureSurvGraf$new(proper = FALSE))
  t = tgen('simsurv')$generate(20)
  l = lrn('surv.kaplan')
  s1 = l$train(t)$predict(t)$score(m1)
  s2 = l$train(t)$predict(t)$score(m2)
  expect_false(identical(s1, s2))
})
