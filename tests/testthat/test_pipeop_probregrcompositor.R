test_that("PipeOpProbregrCompositor - basic properties", {
  expect_pipeop(PipeOpProbregrCompositor$new())
  expect_pipeop(PipeOpProbregrCompositor$new(param_vals = list()))
})

task = tgen("friedman1")$generate(10)

# test_that("PipeOpProbregrCompositor - assertions", {
#   base = lrn("surv.svm")$train(task)$predict(task)
#   pred = lrn("surv.coxph")$train(task)$predict(task)
#   pod = po("distrcompose", param_vals = list(form = "aft", overwrite = TRUE))
#   expect_error(pod$predict(list(base = base, pred = pred)), "Assertion on 'distr'")
#   # expect_silent(pod$predict(list(base = lrn("surv.kaplan")$train(task)$predict(task),
#   #                               pred = lrn("surv.nelson")$train(task)$predict(task))))
# })

test_that("PipeOpProbregrCompositor", {
  gr = probregr_compose(lrn("regr.featureless", predict_type = "se"))
  p = gr$train(task)$predict(task)
  expect_true(distr6::testDistribution(p$distr))
})

test_that("no se", {
  gr = probregr_compose(lrn("regr.featureless"))
  expect_error(gr$train(task)$predict(task))
})


test_that("probregr_compose", {
  gr = probregr_compose(
    learner = lrn("regr.featureless", predict_type = "se"),
    dist = "Logistic")$train(task)$predict(task)
  expect_true(distr6::testDistribution(gr$distr))
})
