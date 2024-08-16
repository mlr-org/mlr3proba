test_that("partition works", {
  with_seed(42, {
    task = tsk("lung") # ~28% censored
    # stratify
    part = partition(task, ratio = 0.8)
    all = task$cens_prop()
    train = task$cens_prop(rows = part$train)
    test = task$cens_prop(rows = part$test)

    expect_true(abs(all - train) < 0.01)
    expect_true(abs(all - test) < 0.01)

    # don't stratify
    part = partition(task, ratio = 0.8, stratify = FALSE)
    train = task$cens_prop(rows = part$train)
    test = task$cens_prop(rows = part$test)

    expect_true(abs(all - train) > 0.01)
    expect_true(abs(all - test) > 0.01)
  })
})
