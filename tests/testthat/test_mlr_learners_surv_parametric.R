context("surv.parametric")

test_that("autotest", {
  learner = mlr_learners$get("surv.parametric")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)

  task = tsk("rats")
  expect_silent(expect_prediction_surv(lrn("surv.parametric", type = "aft")$train(tsk("rats"))$predict(tsk("rats"))))
  expect_silent(expect_prediction_surv(lrn("surv.parametric", type = "ph")$train(tsk("rats"))$predict(tsk("rats"))))
  expect_silent(expect_prediction_surv(lrn("surv.parametric", type = "po")$train(tsk("rats"))$predict(tsk("rats"))))
})

data(lung, package = "survival")
lung2 = lung
lung2$status = lung2$status-1
lung2 = lung2[-14,c("time","status","ph.ecog")]
task = TaskSurv$new("param", backend = lung2, time = "time", event = "status")

test_that("manualtest - aft",{
  learner = lrn("surv.parametric", dist = "weibull", type = "aft")
  expect_silent(learner$train(task))
  p = learner$predict(task)
  expect_prediction_surv(p)
  expect_equal(p$lp[1:20], predict(learner$model$fit, type = "lp")[1:20])
  expect_equal(p$distr[1]$survival(predict(learner$model$fit, type = "quantile", p = c(0.2,0.8))[1,]), c(0.8, 0.2))
  expect_equal(p$distr[10]$cdf(predict(learner$model$fit, type = "quantile", p = seq.int(0,1,0.1))[10,]),
               seq.int(0,1,0.1))

  learner = lrn("surv.parametric", dist = "lognormal", type = "aft")$train(task)
  p = learner$predict(task)
  expect_equal(p$distr[15]$cdf(predict(learner$model$fit, type = "quantile", p = seq.int(0,1,0.1))[15,]), seq.int(0,1,0.1))

  learner = lrn("surv.parametric", dist = "loglogistic", type = "aft")$train(task)
  p = learner$predict(task)
  expect_equal(p$distr[15]$cdf(predict(learner$model$fit, type = "quantile", p = seq.int(0,1,0.1))[15,]), seq.int(0,1,0.1))
})


test_that("missing",{
  learner = lrn("surv.parametric")
  learner$train(task)
  expect_error(learner$predict(tsk("lung")))
})
