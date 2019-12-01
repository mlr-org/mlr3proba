context("surv.flexible")

test_that("autotest", {
  set.seed(1)
  learner = lrn("surv.flexible", inits = c(1,2))
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)

  set.seed(1)
  learner = lrn("surv.flexible", inits = c(1,2))$train(tsk("rats"))$predict(tsk("rats"))
  expect_silent(learner$distr$pdf(40))
  expect_silent(learner$distr$cdf(40))
  expect_silent(learner$distr$rand(1))
  expect_silent(learner$distr$quantile(0.2))
})
