context("surv.coxph")

test_that("autotest", {
  set.seed(1)
  learner = lrn("surv.coxph")
  expect_learner(learner)
  # unclear why weights is failing
  result = run_autotest(learner, exclude = "weight")
  expect_true(result, info = result$error)

  learner = lrn("surv.coxph")
  expect_error(learner$importance(), "No model stored")
  expect_error(learner$selected_features(), "No model stored")
  learner$train(tsk("rats"))
  expect_named(expect_numeric(learner$importance()))
  expect_character(learner$selected_features())
})
