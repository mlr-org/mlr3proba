set.seed(1L)
task = tsk("rats")$filter(sample(300, 5L))

test_that("pecs.list", {
  skip_on_cran()
  learns = lrns(c("surv.kaplan", "surv.coxph"))
  expect_error(pecs(learns))
  suppressWarnings({
    lapply(learns, function(x) x$train(task))
  })
  expect_error(pecs(learns, meas = "sdsd"))
  expect_silent(pecs(learns, task = task, meas = "logloss", n = 10L))
  expect_error(pecs(learns, times = 1L))
  expect_error(pecs(learns, times = -10:-5, task = task), "Not enough")
})

test_that("pecs.PredictionSurv", {
  skip_on_cran()
  suppressWarnings({
    p = lrn("surv.coxph")$train(task)$predict(task)
  })
  expect_silent(pecs(p))
})
