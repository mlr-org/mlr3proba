test_that("basic properties", {
  expect_pipeop(PipeOpSurvAvg$new())
  expect_pipeop(PipeOpSurvAvg$new(param_vals = list()))
})

task = tgen("simsurv")$generate(5)
p1 = lrn("surv.kaplan")$train(task)$predict(task)
p2 = lrn("surv.kaplan")$train(task)$predict(task)


test_that("equal weights", {
  poc = mlr3pipelines::po("survavg")
  expect_silent({p = poc$predict(list(p1, p2))$output})
  expect_prediction_surv(p)

  expect_equal(p$crank, (p1$crank + p2$crank)/2)
  expect_equal(as.numeric(p$distr$cdf(1)),
               as.numeric((p1$distr$cdf(1) + p2$distr$cdf(1))/2))
  expect_equal(p$lp, rep(NA_real_, 5))
  expect_equal(p$response, rep(NA_real_, 5))
})

test_that("unequal weights", {
  poc = mlr3pipelines::po("survavg", param_vals = list(weights = c(0.2, 0.8)))
  expect_silent({p = poc$predict(list(p1, p2))$output})
  expect_prediction_surv(p)

  expect_equal(p$crank, (p1$crank*0.2 + p2$crank*0.8))
  expect_equal(as.numeric(p$distr$cdf(1)),
               as.numeric((p1$distr$cdf(1)*0.2 + p2$distr$cdf(1)*0.8)))
})

test_that("lp", {
  poc = mlr3pipelines::po("survavg")
  expect_silent({p = poc$predict(list(p1, p1))$output})
  expect_equal(p$lp, (p1$lp + p1$lp)/2)
})

test_that("response", {
  poc = mlr3pipelines::po("survavg")
  p3 = mlr3pipelines::ppl("crankcompositor", lrn("surv.kaplan"),
                         response = TRUE, graph_learner = TRUE)$train(task)$predict(task)
  expect_silent({p = poc$predict(list(p3, p3))$output})
  expect_equal(p$response, (p3$response + p3$response)/2)
})

test_that("surv_averager", {
  poc = mlr3pipelines::po("survavg", weights = c(0.2, 0.8))
  p = poc$predict(list(p1, p2))[[1]]

  p2 = mlr3pipelines::ppl("survaverager", list(lrn("surv.kaplan"), lrn("surv.kaplan", id = "k2")),
                                      list(weights = c(0.2, 0.8)),
                          graph_learner = TRUE)$
                          train(task)$predict(task)

  expect_equal(p$crank, p2$crank)
  expect_equal(p$distr$cdf(1:3), p2$distr$cdf(1:3))
})

