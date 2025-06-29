test_that(".interp_surv works", {
  surv_data = list(surv = c(0.98, 0.9, 0.5, 0.1), time = c(1, 2, 3, 4))

  ## === CONSTANT INTERPOLATION === ##
  # Exact time point in data
  expect_equal(.interp_surv(surv_data, eval_times = 3, method = "constant"), 0.5)

  # Between known time points: constant interpolation should take the left value
  expect_equal(.interp_surv(surv_data, eval_times = 2.5, method = "constant"), 0.9)

  # Left extrapolation: returns 1
  expect_equal(.interp_surv(surv_data, eval_times = 0.5, method = "constant"), 1)

  # Right extrapolation: last known value
  expect_equal(.interp_surv(surv_data, eval_times = 10, method = "constant"), 0.1)

  # Duplicated query times
  expect_equal(.interp_surv(surv_data, eval_times = c(1, 3.5, 3.5, 1), method = "constant"),
               c(0.98, 0.5, 0.5, 0.98))

  ## === LINEAR INTERPOLATION === ##
  # Exact match
  expect_equal(.interp_surv(surv_data, eval_times = 2), 0.9)

  # Interpolation within range
  expect_equal(.interp_surv(surv_data, eval_times = 2.5), 0.7) # halfway between 0.9 and 0.5

  # Left extrapolation: linear towards S(0) = 1
  expect_equal(.interp_surv(surv_data, eval_times = 0.5), 0.99)
  expect_equal(.interp_surv(surv_data, eval_times = 0), 1)

  # Right extrapolation: slope-based, clipped at 0
  expect_equal(.interp_surv(surv_data, eval_times = 4.1), 0.06)
  expect_equal(.interp_surv(surv_data, eval_times = 5), 0)

  # Multiple unordered time points
  expect_equal(.interp_surv(surv_data, eval_times = c(2, 4, 4, 2)), c(0.9, 0.1, 0.1, 0.9))

  ## === EDGE CASES === ##
  # All S(t) values identical => constant survival
  constant_surv = list(surv = rep(0.8, 4), time = c(1, 2, 3, 4))
  expect_equal(.interp_surv(constant_surv, eval_times = c(1, 0.5, 3, 6, 10)), rep(0.8, 5))

  # Only one survival time point => constant survival
  one_point = list(surv = 0.9, time = 2)
  expect_equal(.interp_surv(one_point, eval_times = c(0, 2, 10)), rep(0.9, 3))

  # Non-monotonic time input (eval_times only)
  expect_equal(.interp_surv(surv_data, eval_times = c(4, 2, 1)), c(0.1, 0.9, 0.98))

  # Duplicated S(t) values are removed in linear interpolation
  surv_data = list(surv = c(1.0, 0.9, 0.9, 0.7), time = c(1, 2, 3, 4))
  eval_times = c(1, 2, 2.5, 3, 3.5, 4)
  res_const = .interp_surv(surv_data, eval_times, method = "constant")
  expect_equal(res_const, c(1.0, 0.9, 0.9, 0.9, 0.9, 0.7))
  res_linear = .interp_surv(surv_data, eval_times, method = "linear")
  expect_equal(res_linear, c(1.0, 0.9, 0.85, 0.8, 0.75, 0.7))
})

test_that(".interp_pdf works", {
  # Survival curve with decreasing steps
  surv_data = list(surv = c(1, 0.8, 0.6, 0.3, 0.1), time = c(1, 2, 3, 4, 5))

  # Regular eval_times within the defined support
  eval_times = c(1, 2, 3, 4, 5)
  pdf_vals = .interp_pdf(surv_data, eval_times)
  expect_true(all(pdf_vals > 0))

  # Duplicated and unordered times should still return valid results
  eval_times2 = c(5, 3, 3, 1, 1)
  pdf_vals2 = .interp_pdf(surv_data, eval_times2)
  expect_equal(length(pdf_vals2), length(eval_times2))
  expect_true(all(pdf_vals2 > 0))

  # Evaluate extrapolation on both sides
  eval_times3 = c(0, 6)  # outside range
  pdf_vals3 = .interp_pdf(surv_data, eval_times3)
  expect_equal(pdf_vals3[1], 0) # as S(1) = 1
  expect_equal(pdf_vals3[2], 0.1) # as S(5) = 0.1, S(6) = 0, time diff = 1

  # Edge case: flat survival function => zero PDF
  flat_surv = list(surv = rep(0.9, 5), time = 1:5)
  pdf_flat = .interp_pdf(flat_surv, eval_times = 1:5)
  expect_equal(pdf_flat, rep(0, 5))

  # One time point only — should return 0 everywhere
  one_point = list(surv = 0.9, time = 2)
  pdf_one = .interp_pdf(one_point, eval_times = c(1, 2, 3))
  expect_equal(pdf_one, c(0, 0, 0))

  # Constant drop in survival (linear survival) => constant PDF
  linear_surv = list(surv = seq(1, 0.5, length.out = 5), time = 1:5)
  pdf_linear = .interp_pdf(linear_surv, eval_times = 1:5)
  expect_true(all(pdf_linear == 0.125))

  # Duplicated S(t) values are removed
  surv_data = list(surv = c(1.0, 0.9, 0.9, 0.7), time = c(1, 2, 3, 4))
  eval_times = c(1, 2, 2.5, 3, 3.5, 4)
  pdf_vals = .interp_pdf(surv_data, eval_times)
  expect_true(all(pdf_vals > 0))
})
