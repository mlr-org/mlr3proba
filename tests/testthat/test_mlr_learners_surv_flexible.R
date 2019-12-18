context("surv.flexible")

task = TaskGeneratorSimsurv$new()$generate(20)
test_that("autotest", {
  set.seed(1)
  learner = lrn("surv.flexible", inits = c(1,2))
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)

  set.seed(1)
  learner = lrn("surv.flexible", inits = c(1,2))$train(task)$predict(task)
  expect_silent(learner$distr[1]$pdf(40))
  expect_silent(learner$distr[2]$cdf(40))
  expect_silent(learner$distr[3]$rand(1))
  expect_silent(learner$distr[4]$quantile(0.2))
})

test_that("manualtest",{
  set.seed(1)
  learn = lrn("surv.flexible", k = 3, scale = "normal", inits = c(1,2))
  learn$train(task)
  p = learn$predict(task)
  # Comparison to flexsurv
  expect_equal(round(p$lp[1:20],5),
        round(summary(learn$model,
                      task$data(cols=task$feature_names),se=F,ci=F,type="link",tidy=T)[1:20,2],5))
  expect_equal(round(p$distr[2]$survival(1:23),5),
        round(summary(learn$model,
                      task$data(cols=task$feature_names),se=F,ci=F,type="survival",tidy=T,t=1:23)[24:46,2],5))
})

test_that("missing",{
  learner = lrn("surv.flexible", k = 3, scale = "normal", inits = c(1,2))
  learner$train(task)
  expect_error(learner$predict(tsk("lung")))
})
