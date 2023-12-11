test_that(".cbhaz_breslow works", {
  set.seed(42)
  lp = rnorm(5)
  times = seq_len(5)

  # train data has only censored observations
  cbh = .cbhaz_breslow(times, status = c(0,0,0,0,0), lp)
  expect_numeric(cbh, len = 5, lower = 0, upper = 0) # all zero
  expect_named(cbh, as.character(times))

  # one unique event
  cbh1 = .cbhaz_breslow(times, status = c(0,0,0,0,1), lp)
  expect_true(all(cbh1[1:4] == 0))
  expect_true(cbh1[5] > 0)
  expect_named(cbh1, as.character(times))

  cbh2 = .cbhaz_breslow(times, status = c(0,0,0,0,1), lp, eval_times = c(4.8, 5.1))
  expect_true(cbh2[1] == 0)
  expect_true(cbh2[2] > 0)
  expect_named(cbh2, c("4.8", "5.1"))

  cbh3 = .cbhaz_breslow(times, status = c(0,0,1,0,0), lp)
  expect_true(all(cbh3[1:2] == 0))
  expect_true(all(cbh3[3:5] > 0))
  expect_named(cbh3, as.character(times))

  cbh4 = .cbhaz_breslow(times, status = c(1,0,0,0,0), lp)
  expect_true(all(cbh4 > 0)) # all > 0
  cbh5 = .cbhaz_breslow(times, status = c(1,0,0,0,0), lp, eval_times = c(0.8,1))
  expect_equal(unname(cbh5[1]), 0)
  expect_true(cbh5[2] > 0)

  # many events
  cbh6 = .cbhaz_breslow(times, status = c(0,0,0,1,1), lp)
  expect_equal(unname(cbh6[1:3]), c(0,0,0))
  expect_true(all(cbh6[4:5] > 0))

  cbh7 = .cbhaz_breslow(times, status = c(1,0,1,1,1), lp)
  expect_equal(unname(cbh7[1]), unname(cbh7[2]))

  cbh8 = .cbhaz_breslow(times, status = c(1,1,1,1,1), lp)
  expect_true(all(diff(cbh8) > 0))

  # tied event times
  cbh9 = .cbhaz_breslow(times = c(1,2,2,3,3), status = c(1,1,1,1,1), lp)
  expect_numeric(cbh9, len = 3)

  cbh10 = .cbhaz_breslow(times = c(1,1,1,1,1), status = c(0,1,1,0,1), lp)
  expect_numeric(cbh10, len = 1)

  cbh11 = .cbhaz_breslow(times = c(1,1,1,1,1), status = c(0,1,1,0,1), lp, eval_times = c(0.9, 1.1))
  expect_numeric(cbh11, len = 2)
  expect_equal(unname(cbh11[1]), 0)
  expect_true(cbh11[2] > 0)
})

test_that("breslow works", {
  set.seed(42)
  t = tsk("rats")$filter(sample(50))
  p = partition(t, ratio = 0.8)
  l = lrn("surv.coxph")
  suppressWarnings(l$train(t, p$train))
  ptrain = l$predict(t, p$train)
  ptest  = l$predict(t, p$test)

  # normal test
  surv = breslow(times = t$times(p$train), status = t$status(p$train),
    lp_train = ptrain$lp, lp_test = ptest$lp)
  expect_matrix(surv, any.missing = FALSE, nrows = length(p$test),
    ncols = length(unique(t$times(p$train))), col.names = "unique")
  # compare with Breslow estimation from survival
  surv_mat = t(ptest$distr$survival(t$unique_times(p$train)))
  expect_equal(unname(surv), unname(surv_mat), tolerance = 0.004) # very close!

  # eval_times test
  eval_times = c(1,50,100,150)
  surv2 = breslow(times = t$times(p$train), status = t$status(p$train),
    lp_train = ptrain$lp, lp_test = ptest$lp, eval_times = eval_times)
  expect_matrix(surv2, any.missing = FALSE, nrows = length(p$test),
    ncols = length(eval_times), col.names = "unique")

  # 1 test observation + 1 eval_point
  surv3 = breslow(times = t$times(p$train), status = t$status(p$train),
    lp_train = ptrain$lp, lp_test = ptest$lp[3], eval_times = eval_times[3])
  expect_matrix(surv3, any.missing = FALSE, nrows = 1, ncols = 1,
    col.names = "unique")

  # test cumhaz
  surv4 = breslow(times = t$times(p$train), status = t$status(p$train),
    lp_train = ptrain$lp, lp_test = ptest$lp, eval_times = eval_times, type = "cumhaz")
  expect_matrix(surv4, any.missing = FALSE, nrows = length(p$test),
    ncols = length(eval_times), col.names = "unique")

  # basic parameter checks
  expect_error(breslow(times = NULL))
  expect_error(breslow(times = c(1,2), status = 0, lp_train = 1))
  expect_error(breslow(times = c(1,2), status = c(0,1), lp_train = 1))
  expect_error(breslow(times = c(1,2), status = c(0,1), lp_train = c(1,2)))
  expect_error(breslow(times = c(1,2), status = c(0,1), lp_train = c(1,2)))
  expect_error(breslow(times = c(1,2), status = c(0,1), lp_train = c(1,2),
    lp_test = c(1,2), type = "hazard")) # has to be "cumhaz"
})
