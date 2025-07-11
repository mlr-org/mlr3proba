test_that("autotest", {
  with_seed(42L, {
    learner = lrn("cmprsk.aalen")
    expect_learner(learner)
    result = run_autotest(learner, N = 42, check_replicable = FALSE)
    expect_true(result, info = result$error)
  })
})
