test_that("fallback = default_fallback() works", {
  # survival learners
  learner = lrn("surv.kaplan")
  fallback = default_fallback(learner)
  expect_class(fallback, "LearnerSurvKaplan")
  expect_equal(fallback$predict_type, "crank")

  learner = lrn("surv.rpart")
  fallback = default_fallback(learner)
  expect_class(fallback, "LearnerSurvKaplan")
  expect_equal(fallback$predict_type, "crank")

  learner = lrn("surv.coxph")
  fallback = default_fallback(learner)
  expect_class(fallback, "LearnerSurvKaplan")
  expect_equal(fallback$predict_type, "crank")

  # density learners
  learner = lrn("dens.kde")
  fallback = default_fallback(learner)
  expect_class(fallback, "LearnerDensHistogram")
  expect_equal(fallback$predict_type, "pdf")

  # competing risk learners
  learner = lrn("cmprsk.aalen")
  fallback = default_fallback(learner)
  expect_class(fallback, "LearnerCompRisksAalenJohansen")
  expect_equal(fallback$predict_type, "cif")
})
