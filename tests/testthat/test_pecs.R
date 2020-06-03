context("pecs")

set.seed(1)
task = tgen("simsurv")$generate(5)

test_that("pecs.list", {
  skip_on_cran()
  learns = lrns(c("surv.kaplan", "surv.coxph"))
  expect_error(pecs(learns), "trained survival learners")
  suppressWarnings({
    lapply(learns, function(x) x$train(task))
  })
  expect_error(pecs(learns, meas = "sdsd"))
  expect_silent(pecs(learns, task = task, meas = "logloss", n = 10))
  expect_error(pecs(learns, times = 1))
  expect_error(pecs(learns, times = -10:-5, task = task), "Not enough")
})

test_that("pecs.PredictionSurv", {
  skip_on_cran()
  suppressWarnings({
    p = lrn("surv.coxph")$train(task)$predict(task)
  })
  expect_silent(pecs(p))
})
