test_that(".cbhaz_breslow works", {
  set.seed(42L)
  lp = rnorm(5L)
  times = seq_len(5L)

  # train data has only censored observations
  cbh = .cbhaz_breslow(times, status = c(0, 0, 0, 0, 0), lp)
  expect_numeric(cbh, len = 5L, lower = 0, upper = 0) # all zero
  expect_named(cbh, as.character(times))

  # one unique event
  cbh1 = .cbhaz_breslow(times, status = c(0, 0, 0, 0, 1), lp)
  expect_true(all(cbh1[1:4] == 0))
  expect_gt(cbh1[5L], 0)
  expect_named(cbh1, as.character(times))

  cbh2 = .cbhaz_breslow(times, status = c(0, 0, 0, 0, 1), lp, eval_times = c(4.8, 5.1))
  expect_true(cbh2[1L] == 0)
  expect_gt(cbh2[2L], 0)
  expect_named(cbh2, c("4.8", "5.1"))

  cbh3 = .cbhaz_breslow(times, status = c(0, 0, 1, 0, 0), lp)
  expect_true(all(cbh3[1:2] == 0))
  expect_true(all(cbh3[3:5] > 0))
  expect_named(cbh3, as.character(times))

  cbh4 = .cbhaz_breslow(times, status = c(1, 0, 0, 0, 0), lp)
  expect_true(all(cbh4 > 0)) # all > 0
  cbh5 = .cbhaz_breslow(times, status = c(1, 0, 0, 0, 0), lp, eval_times = c(0.8, 1))
  expect_equal(unname(cbh5[1L]), 0)
  expect_gt(cbh5[2L], 0)

  # many events
  cbh6 = .cbhaz_breslow(times, status = c(0, 0, 0, 1, 1), lp)
  expect_equal(unname(cbh6[1:3]), c(0, 0, 0))
  expect_true(all(cbh6[4:5] > 0))

  cbh7 = .cbhaz_breslow(times, status = c(1, 0, 1, 1, 1), lp)
  expect_equal(unname(cbh7[1L]), unname(cbh7[2]))

  cbh8 = .cbhaz_breslow(times, status = c(1, 1, 1, 1, 1), lp)
  expect_true(all(diff(cbh8) > 0))

  # tied event times
  cbh9 = .cbhaz_breslow(times = c(1, 2, 2, 3, 3), status = c(1, 1, 1, 1, 1), lp)
  expect_numeric(cbh9, len = 3L)

  cbh10 = .cbhaz_breslow(times = c(1, 1, 1, 1, 1), status = c(0, 1, 1, 0, 1), lp)
  expect_numeric(cbh10, len = 1L)

  cbh11 = .cbhaz_breslow(times = c(1, 1, 1, 1, 1), status = c(0, 1, 1, 0, 1), lp,
    eval_times = c(0.9, 1.1))
  expect_numeric(cbh11, len = 2L)
  expect_equal(unname(cbh11[1L]), 0)
  expect_gt(cbh11[2L], 0)

  # Inf lp predictions => cumulative baseline hazards are all zero
  cbh12 = .cbhaz_breslow(times = times, status = c(0, 1, 1, 0, 1), lp = rep(Inf, 5))
  expect_numeric(cbh12, len = 5L, lower = 0, upper = 0)
  cbh13 = .cbhaz_breslow(times = times, status = rep(1, 5), lp = c(-Inf, -Inf, Inf, Inf, -Inf))
  expect_numeric(cbh13, len = 5L, lower = 0, upper = Inf, sorted = TRUE)
})

test_that("breslow() works", {
  set.seed(42L)
  t = tsk("rats")$filter(sample(50))
  p = partition(t, ratio = 0.8)
  l = lrn("surv.coxph")
  suppressWarnings(l$train(t, p$train))
  ptrain = l$predict(t, p$train)
  ptest = l$predict(t, p$test)

  # normal test
  surv = breslow(times = t$times(p$train), status = t$status(p$train),
    lp_train = ptrain$lp, lp_test = ptest$lp)
  expect_matrix(surv, any.missing = FALSE, nrows = length(p$test),
    ncols = length(unique(t$times(p$train))), col.names = "unique")
  # compare with Breslow estimation from survival
  surv_mat = t(ptest$distr$survival(t$unique_times(p$train)))
  expect_equal(unname(surv), unname(surv_mat), tolerance = 0.004) # very close!

  # eval_times test
  eval_times = c(1, 50, 100, 150)
  surv2 = breslow(times = t$times(p$train), status = t$status(p$train),
    lp_train = ptrain$lp, lp_test = ptest$lp, eval_times = eval_times)
  expect_matrix(surv2, any.missing = FALSE, nrows = length(p$test),
    ncols = length(eval_times), col.names = "unique")

  # 1 test observation + 1 eval_point
  surv3 = breslow(times = t$times(p$train), status = t$status(p$train),
    lp_train = ptrain$lp, lp_test = ptest$lp[3L], eval_times = eval_times[3L])
  expect_matrix(surv3, any.missing = FALSE, nrows = 1L, ncols = 1L,
    col.names = "unique")

  # test cumhaz
  surv4 = breslow(times = t$times(p$train), status = t$status(p$train),
    lp_train = ptrain$lp, lp_test = ptest$lp, eval_times = eval_times, type = "cumhaz")
  expect_matrix(surv4, any.missing = FALSE, nrows = length(p$test),
    ncols = length(eval_times), col.names = "unique")

  # basic parameter checks
  expect_error(breslow(times = NULL))
  expect_error(breslow(times = c(1, 2), status = 0, lp_train = 1))
  expect_error(breslow(times = c(1, 2), status = c(0, 1), lp_train = 1))
  expect_error(breslow(times = c(1, 2), status = c(0, 1), lp_train = c(1, 2)))
  expect_error(breslow(times = c(1, 2), status = c(0, 1), lp_train = c(1, 2)))
  expect_error(breslow(times = c(1, 2), status = c(0, 1), lp_train = c(1, 2),
    lp_test = c(1, 2), type = "hazard")) # has to be "cumhaz"

  # Inf lp predictions: cumulative baseline hazard
  times = seq_len(5)
  lp = c(-Inf, -Inf, Inf, Inf, -Inf)
  st = rep(1, 5)
  cbhaz = .cbhaz_breslow(times = times, status = st, lp = lp)
  expect_numeric(cbhaz, len = 5L, lower = 0, upper = Inf, sorted = TRUE)

  # Inf lp predictions: cumulative hazard and survival matrix
  lp_test = c(-Inf, -1, 0, 1, Inf)
  cumhaz = breslow(times, status = st, lp_train = lp, lp_test = lp_test, type = "cumhaz")
  expect_matrix(cumhaz, nrows = length(lp_test), ncols = length(cbhaz))
  expect_equal(sum(is.nan(cumhaz)), 0)

  surv = breslow(times, status = st, lp_train = lp, lp_test = lp_test)
  expect_matrix(surv, nrows = length(lp_test), ncols = length(cbhaz))
  expect_true(all(surv >= 0, surv <= 1))
})

test_that("breslowcompose PipeOp works", {
  # learner is needed
  expect_error(po("breslowcompose"), "is missing")

  # learner needs to be of survival type
  expect_error(po("breslowcompose", learner = lrn("classif.featureless")),
               "must have task type")
  # learner needs to have lp predictions
  expect_error(po("breslowcompose", learner = lrn("surv.kaplan")),
               "must provide lp")

  # learner with lp predictions
  learner = lrn("surv.coxph")
  b1 = po("breslowcompose", learner = learner, breslow.overwrite = TRUE)
  b2 = po("breslowcompose", learner = learner)

  expect_pipeop(b1)
  expect_pipeop(b2)
  expect_equal(b1$id, learner$id)
  expect_equal(b2$id, learner$id)
  expect_true(b1$param_set$values$breslow.overwrite)
  expect_false(b2$param_set$values$breslow.overwrite)
  expect_learner(b1$learner)
  expect_error({b1$learner = lrn("surv.kaplan")}) # read-only

  task = tsk("lung")
  cox_pred = lrn("surv.coxph")$train(task)$predict(task)

  expect_silent(b1$train(list(task)))
  expect_silent(b2$train(list(task)))
  p1 = b1$predict(list(task))[[1L]]
  p2 = b2$predict(list(task))[[1L]]

  expect_equal(p1$lp, p2$lp)
  surv_mat1 = p1$data$distr
  surv_mat2 = p2$data$distr
  expect_false(all(surv_mat1 == surv_mat2)) # distr predictions changed (a bit)
  expect_true(all(surv_mat2 == cox_pred$data$distr)) # distr was not overwritten
})
