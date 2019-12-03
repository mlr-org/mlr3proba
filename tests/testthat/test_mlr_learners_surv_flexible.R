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

test_that("manualtest",{
  set.seed(1)
  task = TaskGeneratorSimsurv$new()$generate(50)
  learn = lrn("surv.flexible", k = 3, scale = "normal")
  learn$train(task)
  p = learn$predict(task)
  # Comparison to flexsurv
  expect_equal(round(p$lp[1:20],5),
        round(summary(learn$model,
                      task$data(cols=task$feature_names),se=F,ci=F,type="link",tidy=T)[1:20,2],5))
  expect_equal(round(p$distr[1]$survival(1:23),5),
        round(summary(learn$model,
                      task$data(cols=task$feature_names),se=F,ci=F,type="survival",tidy=T,t=1:23)[1:23,2],5))
  expect_equal(round(p$distr[2]$survival(1:23),5),
        round(summary(learn$model,
                      task$data(cols=task$feature_names),se=F,ci=F,type="survival",tidy=T,t=1:23)[24:46,2],5))
})
