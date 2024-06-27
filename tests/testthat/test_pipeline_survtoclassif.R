skip_if_not_installed("mlr3learners")

task = tsk("rats")$filter(sample(300, 50))

test_that("survtoclassif", {
  requireNamespace("mlr3learners")

  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new())
  expect_class(pipe, "Graph")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p[[1]])

  cox = lrn("surv.coxph")
  cox$train(task) |> suppressWarnings()
  p2 = cox$predict(task)

  expect_equal(p[[1]]$truth, p2$truth)
  expect_equal(p[[1]]$score(), p2$score(), tolerance = 0.1)

  # Test with cut
  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new(), cut = c(10, 30, 50))
  expect_class(pipe, "Graph")
  pipe$train(task) |> suppressWarnings()
  p = pipe$predict(task)
  expect_prediction_surv(p[[1]])

  # Test with max_time
  t = task$data()[status == 1, min(time)]
  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new(), max_time = t)
  expect_class(pipe, "Graph")
  expect_error(pipe$train(task))

  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new(), max_time = t + 1)
  pipe$train(task) |> suppressWarnings()
  p = pipe$predict(task)
  expect_prediction_surv(p[[1]])
})
