test_that("partition w/ stratification works", {
  with_seed(42, {
    task = tsk("rats")
    part = partition(task, ratio = 0.8)

    ratio = function(status) {
      tab = table(status)
      unname(tab[1L] / tab[2L])
    }

    all = task$cens_prop()
    train = task$cens_prop(rows = part$train)
    test = task$cens_prop(rows = part$test)

    expect_equal(all, train, tolerance = 0.01)
    expect_equal(all, test, tolerance = 0.01)
  })
})
