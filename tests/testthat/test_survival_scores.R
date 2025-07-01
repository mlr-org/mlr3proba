cens = matrix(c(1, 0.9, 2, 0.8, 3, 0.6), ncol = 2, byrow = TRUE) # G(t)
eps = 1e-3

test_that("input checks", {
  score = matrix(rep(1,3), nrow = 1) # 1 obs, 3 time points
  unique_times = c(1, 2)
  truth = Surv(time = 1, event = 1)
  expect_error(c_apply_ipcw_weights(score, unique_times, truth, cens, eps), "Length of")

  unique_times = c(1, 2, 3)
  truth = Surv(time = c(1, 2), event = c(1, 0))
  expect_error(c_apply_ipcw_weights(score, unique_times, truth, cens, eps), "Number of rows")
})

test_that("weights applied correctly", {
  score = matrix(rep(1,6), nrow = 2) # 1 obs, 3 time points
  unique_times = c(1, 2, 3) # tau times
  # 1st obs => ti = 2 (event)
  # 2nd obs => ti = 1.5 (censored)
  truth = Surv(time = c(2, 1.5), event = c(1, 0))

  ipcw_score = c_apply_ipcw_weights(score, unique_times, truth, cens, eps)
  expected_score = matrix(
    # 1st obs: first uses G(1), as ti < tau, last two use G(ti) as ti >= tau
    c(1/0.9, 1/0.8, 1/0.8,
    # 2nd obs: first uses G(1), as ti < tau, last two are 0 as obs is censored and ti >= tau
      1/0.9, 0, 0), nrow = 2, byrow = TRUE
  )
  expect_equal(ipcw_score, expected_score)
})

test_that("uses epsilon when G(t) = 0", {
  score = matrix(1, nrow = 1, ncol = 1)
  truth = Surv(time = 2, event = 1) # event
  unique_times = c(2)
  cens0 = matrix(c(2, 0.0), ncol = 2, byrow = TRUE) # G(2) = 0

  ipcw_score = c_apply_ipcw_weights(score, unique_times, truth, cens0, eps)
  expect_equal(ipcw_score, matrix(1/eps))
})

test_that("uses left-constant interpolation for G(t)", {
  score = matrix(rep(1, 4), nrow = 2, ncol = 2)
  truth = Surv(time = c(2.5, 2.5), event = c(1, 0)) # t_i = 2.5 (1 event, 1 censored)
  unique_times = c(2.4, 2.6) # tau

  ipcw_score = c_apply_ipcw_weights(score, unique_times, truth, cens, eps)

  # G(2) = 0.8
  # ti = 2.5 > 2.4 (tau) => use G(tau) = G(2.4) falls in [2, 3) => no matter the outcome
  # ti = 2.5 < 2.6 (tau) => use G(ti) = G(2.5) falls in [2, 3) => if event, else 0
  expected_score = matrix(
    # 1st obs (event)
    c(1/0.8, 1/0.8,
    # 2nd obs (censored)
      1/0.8, 0), nrow = 2, byrow = TRUE
  )
  expect_equal(ipcw_score, expected_score)
})

test_that("G(t) = 1 for t < min(cens_times)", {
  score = matrix(4, nrow = 1, ncol = 1)
  truth = Surv(time = 0.5, event = 1) # t_i = 0.5
  unique_times = c(0.5)
  cens = matrix(c(1, 0.9), ncol = 2, byrow = TRUE)  # only starts at t = 1

  ipcw_score = c_apply_ipcw_weights(score, unique_times, truth, cens, eps)

  expect_equal(ipcw_score, matrix(4)) # 4 / 1.0
})
