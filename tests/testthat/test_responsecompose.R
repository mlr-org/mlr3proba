test_that("basic properties", {
  expect_pipeop(PipeOpResponseCompositor$new())
  expect_equal(PipeOpResponseCompositor$new()$param_set$values$method, "rmst")
  expect_false(PipeOpResponseCompositor$new()$param_set$values$add_crank)
  expect_false(PipeOpResponseCompositor$new()$param_set$values$overwrite)

  # check that during construction, initial values are not overwritten
  values = PipeOpResponseCompositor$new()$param_set$values
  values2 = PipeOpResponseCompositor$new(param_vals = list(method = "rmst"))$param_set$values
  expect_equal(values, values2)

  # parameter checks
  expect_error(PipeOpResponseCompositor$new(param_vals = list(method = "wrong")))
  expect_error(PipeOpResponseCompositor$new(param_vals = list(add_crank = "not_a_bool")))
})

set.seed(42)
task = tgen("coxed", T = 99)$generate(20L)
pcox = lrn("surv.coxph")$train(task)$predict(task)
pcox$data$response = rexp(20) # hack: add survival time predictions to cox model!

test_that("overwrite", {
  # no overwrite
  por = mlr3pipelines::po("responsecompose")
  p1 = por$predict(list(pcox))[[1L]]
  expect_identical(p1$response, pcox$response)

  # overwrite response
  por = mlr3pipelines::po("responsecompose", overwrite = TRUE)
  p2 = por$predict(list(pcox))[[1L]]
  expect_false(all(p2$response == pcox$response))

  # even if prediction doesn't have response, pipeop will add them even if no overwrite
  por = mlr3pipelines::po("responsecompose")
  pkm = lrn("surv.kaplan")$train(task)$predict(task)
  expect_null(pkm$response)
  p3 = por$predict(list(pkm))[[1L]]
  expect_false(is.null(p3$response))
  expect_identical(pkm$crank, p3$crank)
})

test_that("different methods, different responses", {
  por = mlr3pipelines::po("responsecompose", overwrite = TRUE, method = "rmst")
  p1 = por$predict(list(pcox))[[1L]]
  por$param_set$set_values(method = "median")
  p2 = por$predict(list(pcox))[[1L]]
  expect_false(all(p1$response == p2$response))
})

test_that("different cutoffs, different rmst", {
  por1 = mlr3pipelines::po("responsecompose", overwrite = TRUE, method = "rmst")
  por2 = mlr3pipelines::po("responsecompose", overwrite = TRUE, method = "rmst",
                           cutoff_time = 100) # t_max = 99 in the generated data
  por3 = mlr3pipelines::po("responsecompose", overwrite = TRUE, method = "rmst",
                           cutoff_time = 65)
  por4 = mlr3pipelines::po("responsecompose", overwrite = TRUE, method = "rmst",
                           cutoff_time = 25)
  p1 = por1$predict(list(pcox))[[1L]]
  p2 = por2$predict(list(pcox))[[1L]]
  p3 = por3$predict(list(pcox))[[1L]]
  p4 = por4$predict(list(pcox))[[1L]]
  expect_identical(p1$response, p2$response)
  expect_false(all(p2$response == p3$response))
  expect_false(all(p2$response == p4$response))
  expect_false(all(p3$response == p4$response))
  expect_gte(max(p2$response), max(p3$response))
  expect_gte(max(p3$response), max(p4$response))
})

test_that("crank is added", {
  por = mlr3pipelines::po("responsecompose", overwrite = FALSE, add_crank = TRUE)
  p1 = por$predict(list(pcox))[[1L]]
  # same crank or response
  expect_identical(p1$response, pcox$response)

  por = mlr3pipelines::po("responsecompose", overwrite = TRUE, add_crank = TRUE)
  p2 = por$predict(list(pcox))[[1L]]
  # response changed
  expect_false(all(pcox$response == p2$response))
  # crank is -response
  expect_identical(p2$response, -p2$crank)
})
