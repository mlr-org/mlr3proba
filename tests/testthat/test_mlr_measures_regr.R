test_that("mlr_measures", {
  task = tsk("mtcars")
  l = ppl("probregr", learner = lrn("regr.featureless"), graph_learner = TRUE)
  p = l$train(task)$predict(task)
  m = msr("regr.logloss")
  expect_measure(m)
  expect_numeric(p$score(m))
})
