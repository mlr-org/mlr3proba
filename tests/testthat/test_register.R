context("populate dictionaries")

test_that("re-populate learners", {
  rm("surv.coxph", envir = mlr_learners$items)
  expect_disjunct("surv.coxph", mlr_learners$keys())
  register_mlr3()
  expect_subset("surv.coxph", mlr_learners$keys())
})

test_that("re-populate pipelines", {
  rm("distrcompose", envir = mlr_pipeops$items)
  expect_disjunct("distrcompose", mlr_pipeops$keys())
  register_mlr3pipelines()
  expect_subset("distrcompose", mlr_pipeops$keys())
})

test_that("re-populate reflections", {
  mlr_reflections$task_types = mlr_reflections$task_types[!unlist(mlr_reflections$task_types[, 1]) %in% "surv",]
  expect_false("surv" %in% mlr_reflections$task_types[,1])
  register_mlr3()
  expect_true("surv" %in% mlr_reflections$task_types[3,1])
})
