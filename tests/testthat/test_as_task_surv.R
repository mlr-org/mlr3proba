test_that("as_task_surv.formula()", {
  df = data.frame(start = 1:2, stop = 3:4, event = 0:1, x = 1:2)

  task = as_task_surv(Surv(start, stop, event, "counting", 0) ~ ., df)
  expect_equal(attr(task$truth(), "type"), "counting")

  task = as_task_surv(Surv(start, event) ~ ., df)
  expect_equal(attr(task$truth(), "type"), "right")

  task = as_task_surv(Surv(start, stop, type = "interval2") ~ ., df[, -3L])
  expect_equal(attr(task$truth(), "type"), "interval")

  task = as_task_surv(Surv(start, stop, event, "interval", 1) ~ ., df)
  expect_equal(attr(task$truth(), "type"), "interval")

  expect_error(fixed = TRUE,
    as_task_surv(Surv(start, stop, event, "mstate") ~ ., df),
    "supported"
  )

  setDT(df)
  task = as_task_surv(Surv(start, event) ~ ., df)
  expect_equal(attr(task$truth(), "type"), "right")
})
