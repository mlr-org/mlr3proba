test_that("PipeOpDistrCompositor - basic properties", {
  expect_pipeop(PipeOpDistrCompositor$new())

  # check that during construction, initial values are not overwritten
  values  = PipeOpDistrCompositor$new()$param_set$values
  values2 = PipeOpDistrCompositor$new(param_vals = list(overwrite = FALSE))$param_set$values
  expect_equal(values, values2)
})

set.seed(42)
task = tsk("rats")$filter(sample(300, 110))
cox_pred = lrn("surv.coxph")$train(task)$predict(task)

test_that("PipeOpDistrCompositor - overwrite = FALSE", {
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.kaplan", id = "k2"), overwrite = FALSE)
  expect_silent(gr$train(task))
  expect_equal(
    gr$predict(task)[[1]]$data$distr,
    lrn("surv.kaplan", id = "k2")$train(task)$predict(task)$data$distr
  )

  # breslow
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.coxph"),
    estimator = "breslow", overwrite = FALSE)
  expect_silent(gr$train(task))
  expect_equal(gr$predict(task)[[1]]$data$distr, cox_pred$data$distr)
})

test_that("PipeOpDistrCompositor - overwrite = TRUE", {
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.kaplan", id = "k2"), overwrite = TRUE, form = "ph")
  expect_silent(gr$train(task))
  p = gr$predict(task)[[1]]
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)

  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.kaplan", id = "k2"), overwrite = TRUE, form = "po",
    graph_learner = TRUE)
  expect_prediction_surv(gr$train(task)$predict(task))

  # breslow
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.coxph"), estimator = "breslow",
    overwrite = TRUE, graph_learner = TRUE)
  expect_silent(gr$train(task))
  surv_mat1 = gr$predict(task)$data$distr
  surv_mat2 = cox_pred$data$distr
  expect_false(all(surv_mat1 == surv_mat2)) # distr predictions changed (a bit)
})

test_that("no params", {
  base = lrn("surv.kaplan")$train(task)$predict(task)
  pred = lrn("surv.kaplan", id = "k2")$train(task)$predict(task)
  pod = mlr3pipelines::po("distrcompose", param_vals = list())
  expect_silent(pod$predict(list(base = base, pred = pred)))
})

test_that("breslow PipeOp works", {
  # learner is needed
  expect_error(po("breslowcompose"), "is missing")

  # learner needs to be of survival type
  expect_error(po("breslowcompose", learner = lrn("classif.featureless")),
               "must have task type")
  # learner needs to have lp predictions
  expect_error(po("breslowcompose", learner = lrn("surv.kaplan")),
    "must provide lp")

  # learner with lp predictions
  learner = lrn("surv.coxph")
  b1 = po("breslowcompose", learner = learner, breslow.overwrite = TRUE)
  b2 = po("breslowcompose", learner = learner)

  expect_pipeop(b1)
  expect_pipeop(b2)
  expect_equal(b1$id, learner$id)
  expect_equal(b2$id, learner$id)
  expect_true(b1$param_set$values$breslow.overwrite)
  expect_false(b2$param_set$values$breslow.overwrite)
  expect_learner(b1$learner)
  expect_error({b1$learner = lrn("surv.kaplan")}) # read-only

  expect_silent(b1$train(list(task)))
  expect_silent(b2$train(list(task)))
  p1 = b1$predict(list(task))[[1L]]
  p2 = b2$predict(list(task))[[1L]]

  expect_equal(p1$lp, p2$lp)
  surv_mat1 = p1$data$distr
  surv_mat2 = p2$data$distr
  expect_false(all(surv_mat1 == surv_mat2)) # distr predictions changed (a bit)
  expect_true(all(surv_mat2 == cox_pred$data$distr)) # distr was not overwritten
})
