# context("logloss")
#
# test_that("mlr_measures_logloss", {
#   task = TaskDensity$new("a",data.table::data.table(target = rep(1:10,1000)),"target")
#   lrn = lrn("density.kde", predict_type = "prob")
#   lrn$param_set$values$kernel = "UnifKern"
#   m = msr("classif.logloss")
#   measures = list(m)
#
#   p = lrn$train(task)$predict(task, 1:10)
#   perf = p$score(measures)
#   expect_equal(unname(perf), 0.5)
# })
