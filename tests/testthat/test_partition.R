test_that("partition w/ stratification works", {
  task = tsk("rats")
  sets = partition(task)

  ratio = function(status) {
    tab = table(status)
    unname(tab[1] / tab[2])
  }

  all = ratio(task$status())
  train = ratio(task$status(sets$train))
  test = ratio(task$status(sets$test))

  expect_numeric(all, lower = 6, upper = 6.2)
  expect_numeric(train, lower = 6, upper = 6.2)
  expect_numeric(test, lower = 6, upper = 6.2)
})
