library(mlr3pipelines)
l = as_learner(ppl("probregr", learner = lrn("regr.featureless")))
task = tsk("mtcars")
p = l$train(task)$predict(task)

expect_measure(msr("regr.logloss"))
expect_numeric(p$score(msr("regr.logloss")))
