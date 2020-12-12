test_that("autotest", {
  learner = mlr_learners$get("surv.kaplan")
  expect_learner(learner)
  # sanity requires c > 0.5, kaplan always = 0.5
  result = run_autotest(learner, check_replicable = FALSE, N = 10, exclude = "sanity")
  expect_true(result, info = result$error)
})
