test_that("basic properties", {
  expect_pipeop(PipeOpSurvAvg$new())
  expect_pipeop(PipeOpSurvAvg$new(param_vals = list()))
})

task = tsk("lung")$filter(1:50)
p1 = lrn("surv.kaplan")$train(task)$predict(task)
p2 = lrn("surv.coxph")$train(task)$predict(task)

test_that("equal weights", {
  poc = po("survavg")
  poc$train(list(NULL))
  expect_silent({
    p = poc$predict(list(p1, p2))[[1L]]
  })
  expect_prediction_surv(p)

  expect_equal(p$crank, unname((p1$crank + p2$crank)) / 2)
  expect_equal(as.numeric(p$distr$cdf(100)),
    as.numeric((p1$distr$cdf(100) + p2$distr$cdf(100)) / 2))
  expect_null(p$lp) # Cox's lp's were deleted
  expect_null(p$response)
})

test_that("unequal weights", {
  poc = po("survavg", param_vals = list(weights = c(0.2, 0.8)))
  poc$train(list(NULL))
  expect_silent({
    p = poc$predict(list(p1, p2))$output
  })
  expect_prediction_surv(p)

  expect_equal(p$crank, unname(p1$crank * 0.2 + p2$crank * 0.8))
  expect_equal(as.numeric(p$distr$cdf(100)),
    as.numeric((p1$distr$cdf(100) * 0.2 + p2$distr$cdf(100) * 0.8)))
})

test_that("lp", {
  poc = po("survavg")
  poc$train(list(NULL))
  expect_silent({
    p = poc$predict(list(p2, p2))[[1L]]
  })
  expect_equal(p$lp, unname((p2$lp + p2$lp) / 2))
})

test_that("response", {
  poc = po("survavg")
  poc$train(list(NULL))

  por = ppl("responsecompositor", learner = lrn("surv.coxph"), graph_learner = TRUE)
  p3 = por$train(task)$predict(task)
  expect_silent({
    p = poc$predict(list(p3, p3))[[1L]]
  })
  expect_equal(p$response, unname((p3$response + p3$response) / 2))
})

test_that("pipeline surv_averager", {
  poc = po("survavg", weights = c(0.2, 0.8))
  poc$train(list(NULL))
  p = poc$predict(list(p1, p2))[[1L]]

  grlrn = ppl("survaverager", learners = list(lrn("surv.kaplan"), lrn("surv.coxph")),
              list(weights = c(0.2, 0.8)), graph_learner = TRUE)
  p2 = grlrn$train(task)$predict(task)

  expect_equal(p$crank, p2$crank)
  expect_equal(p$distr$cdf(1:3), p2$distr$cdf(1:3))
})
