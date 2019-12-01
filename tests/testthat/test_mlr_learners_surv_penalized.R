context("surv.penalized")

test_that("autotest", {
  learner = mlr_learners$get("surv.penalized")
  contr.none = penalized::contr.none
  contr.diff = penalized::contr.diff
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
