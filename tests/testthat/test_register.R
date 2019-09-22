context("populate dictionaries")

test_that("re-populate dictionaries", {
  rm("surv.coxph", envir = mlr_learners$items)
  expect_disjunct("surv.coxph", mlr_learners$keys())
  register_mlr3()
  expect_subset("surv.coxph", mlr_learners$keys())
})
