test_that("Task duplicates rows", {
  task = tsk("lung")
  expect_task_surv(task)
  expect_set_equal(extract_vars(task$formula())$rhs, ".")
})

test_that("right censoring", {
  expect_silent({
    task = TaskSurv$new("right", backend = survival::rats, time = "time", event = "status")
  })
  expect_equal(task$censtype, "right")
  expect_numeric(task$times())
  expect_integer(task$status())
  expect_equal(task$formula(), as.formula(Surv(time, status, type = "right") ~ .))
})

test_that("left censoring", {
  expect_silent({
    task = TaskSurv$new("left", backend = survival::rats, time = "time", event = "status",
      type = "left")
  })
  expect_equal(task$censtype, "left")
  expect_numeric(task$times())
  expect_integer(task$status())
  expect_equal(task$formula(), as.formula(Surv(time, status, type = "left") ~ .))
})

test_that("interval censoring", {
  # interval censoring data should have one subject id per row
  expect_silent({
    task = TaskSurv$new(
      id = "interval_censored", backend = survival::bladder1,
      time = "start", time2 = "stop", event = "status", type = "interval")
  })
  expect_equal(task$censtype, "interval")
  expect_equal(ncol(task$truth()), 3L)
  expect_numeric(task$times())
  expect_integer(task$status())
  expect_equal(task$formula(), as.formula(Surv(start, stop, status, type = "interval") ~ .))
})

test_that("interval2 censoring", {
  # test without 'event'
  expect_silent({
    task = TaskSurv$new(
      id = "interval2_censored", backend = survival::bladder2[, -c(1, 7)],
      time = "start", time2 = "stop", type = "interval2")
  })
  expect_equal(task$censtype, "interval2")
  expect_equal(ncol(task$truth()), 3)
  expect_numeric(task$times())
  expect_integer(task$status())
  expect_equal(task$formula(), as.formula(Surv(start, stop, type = "interval2") ~ .))
})

test_that("surv methods", {
  status = c(1, 1, 1, 0, 0, 0)
  times = c(1, 1, 2, 4, 4, 5)
  task = TaskSurv$new(
    id = "test",
    backend = data.frame(event = status, time = times, x = runif(6))
  )

  expect_equal(task$times(), times)
  expect_equal(task$status(), status)
  expect_equal(task$unique_times(), c(1, 2, 4, 5))
  expect_equal(task$unique_event_times(), c(1, 2))
  expect_equal(task$risk_set(time = 2), c(3, 4, 5, 6))
  expect_class(task$kaplan(), "survfit")
  expect_class(task$kaplan(rows = 1:3), "survfit")
  expect_equal(task$cens_prop(), 3 / 6)
  expect_equal(task$admin_cens_prop(), 1 / 3) # t_max = 5
  expect_equal(task$admin_cens_prop(admin_time = 5), 1 / 3)
  expect_equal(task$admin_cens_prop(quantile_prob = 0.9), 1) # t_max = 4
  expect_equal(task$admin_cens_prop(admin_time = 3), 1)
  expect_equal(task$admin_cens_prop(rows = 1:3), 0) # only events
  expect_equal(task$dep_cens_prop(), 0)
  expect_number(task$prop_haz(), finite = TRUE)
  expect_true(tsk("veteran")$prop_haz() < 0.01) # non-PH

  # task with two features, one can predict status
  set.seed(42L)
  n = 100L
  x1 = rnorm(n)
  x2 = rnorm(n)
  event_time = rexp(n, rate = 0.1)
  censor_time = rexp(n, rate = 0.1)
  time = pmin(event_time, censor_time)
  status = as.numeric(event_time <= censor_time)
  data = data.frame(time = time, event = status, x1 = x1, x2 = x2)
  data$event[data$x1 > 1] = 0 # dependent censoring
  task2 = TaskSurv$new(id = "dep_censor", backend = data)
  expect_equal(task2$dep_cens_prop(), 1 / 2)
  expect_equal(task2$dep_cens_prop(sign_level = 0.5), 1)
})

test_that("as_task_surv", {
  expect_task_surv(as_task_surv(data.frame(time = 1, event = 1)))
  expect_task_surv(as_task_surv(data.frame(time = 1, status = 1),
    event = "status"
  ))
  t1 = tsk("rats")
  t2 = as_task_surv(t1, clone = TRUE)
  expect_task_surv(t2)
  t1$select("sex")
  expect_false("litter" %in% names(t1$data()))
  expect_true("litter" %in% names(t2$data()))

  t1 = tsk("rats")
  t2 = as_task_surv(t1, clone = FALSE)
  expect_task_surv(t2)
  t1$select("sex")
  expect_false("litter" %in% names(t1$data()))
  expect_false("litter" %in% names(t2$data()))
})

test_that("reverse", {
  t = tsk("rats")
  expect_equal(t$kaplan()$surv,
    survival::survfit(Surv(time, status) ~ 1, t$data())$surv)
  expect_equal(t$kaplan(reverse = TRUE)$surv,
    survival::survfit(Surv(time, 1 - status) ~ 1, t$data())$surv)

  t2 = tsk("rats")$reverse()
  expect_equal(t$kaplan(reverse = TRUE)$surv, t2$kaplan()$surv)
  expect_equal(t2$data()$status, 1 - t$data()$status)
})
