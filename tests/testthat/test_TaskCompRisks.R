test_that("TaskCompRisks + methods work", {
  event = c(1, 0, 1, 0, 2, 2)
  times = c(1, 3, 2, 4, 4, 5)
  df = data.frame(time = times, event = event, x = runif(6))
  task = TaskCompRisks$new(id = "test", backend = df)

  expect_equal(task$task_type, "cmprsk")
  assert_integer(task$event(), len = 6, lower = 0, upper = 2)
  assert_surv(task$truth(), len = 6, any.missing = FALSE)
  assert_formula(task$formula())
  assert_numeric(task$times(), len = 6)
  assert_numeric(task$unique_times(), len = 5)
  assert_numeric(task$unique_event_times(), len = 4)
  assert_class(task$aalen_johansen(), "survfit")
  expect_equal(task$cens_prop(), 2/6)
  expect_equal(task$cmp_events, c("1", "2"))

  # doesn't work with factor event column
  df2 = data.frame(time = times, event = as.factor(event), x = runif(6))
  expect_error(TaskCompRisks$new(id = "test", backend = df2), "Must be of type")

  # check that after task filtering the cmp events don't change and the events
  # are reported correctly
  task2 = task$clone()$filter(1:3) # events = 0 and 1
  expect_equal(task2$event(), c(1, 0, 1))
  expect_equal(task2$cmp_events, c("1", "2")) # competing events don't change

  task3 = task$clone()$filter(4:6) # events = 0 and 2
  expect_equal(task3$event(), c(0, 2, 2))
  expect_equal(task3$cmp_events, c("1", "2")) # competing events don't change

  # at least two competing risks
  df = data.frame(time = 1:2, event = c(0,1), x = runif(2))
  expect_error(TaskCompRisks$new(id = "test", backend = df), "at least two competing events")

  # works with no censoring at all!
  df = data.frame(time = 1:2, event = c(2,1), x = runif(2))
  task = TaskCompRisks$new(id = "test", backend = df)
  expect_equal(task$cmp_events, c("1", "2"))
  expect_equal(task$filter(1)$cmp_events, c("1", "2")) # competing events don't change
  expect_equal(task$cens_prop(), 0)
})

test_that("as_task_cmprsk", {
  expect_task_cmprsk(as_task_cmprisk(data.frame(time = c(1, 2), event = c(1, 4))))
  expect_task_cmprsk(as_task_cmprisk(data.frame(time = c(1, 2), status = c(1, 2)),
                                     event = "status"))
  expect_task_cmprsk(as_task_cmprisk(data.frame(t = c(1, 2, 3), s = c(0, 1, 2)),
                                     time = "t", event = "s", id = "test"))

  t1 = tsk("pbc")
  t2 = as_task_cmprisk(t1, clone = TRUE)
  expect_task_cmprsk(t2)
  t1$select("edema")
  expect_false("sex" %in% names(t1$data()))
  expect_true("sex" %in% names(t2$data()))

  t1 = tsk("pbc")
  t2 = as_task_cmprisk(t1, clone = FALSE)
  expect_task_cmprsk(t2)
  t1$select("edema")
  expect_false("sex" %in% names(t1$data()))
  expect_false("sex" %in% names(t2$data()))
})
