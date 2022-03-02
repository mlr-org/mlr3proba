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
  expect_silent({
    task = TaskSurv$new(
      id = "interval_censored", backend = survival::bladder1,
      time = "start", time2 = "stop", event = "status", type = "interval")
  })
  expect_equal(task$censtype, "interval")
  expect_equal(ncol(task$truth()), 3)
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
  task = TaskSurv$new(
    id = "test", backend = data.frame(event = c(1, 1, 1, 0, 0), time = c(1, 1, 2, 4, 4),
      x1 = runif(5)))
  expect_equal(task$times(), c(1, 1, 2, 4, 4))
  expect_equal(task$unique_times(), c(1, 2, 4))
  expect_equal(task$unique_event_times(), c(1, 2))
  expect_equal(task$risk_set(2), c(3L, 4L, 5L))
  expect_is(task$kaplan(), "survfit")
  expect_is(task$kaplan(rows = 1:3), "survfit")
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
