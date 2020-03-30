context("surv.cvcoxboost")

test_that("autotest", {
  learner = lrn("surv.cvcoxboost", maxstepno = 1, K = 2)
  expect_learner(learner)
  result = run_autotest(learner, N = 10)
  expect_true(result, info = result$error)

  learner = lrn("surv.cvcoxboost", penalty = "optimCoxBoostPenalty",
                maxstepno = 1, minstepno = 0, iter.max = 1, K = 2)
  expect_learner(learner)
  result = run_autotest(learner, N = 10)
  expect_true(result, info = result$error)
})
