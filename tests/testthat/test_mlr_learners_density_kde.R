context("dens.kde")

test_that("autotest", {
  set.seed(1)
  learner = lrn("dens.kde")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})


data = data.frame("A" = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6))
a <- ks::kde(x = data, h = 0.1,  eval.points = c(0, 0.5, 1, 1.5, 2, -1, 3))$estimate
a
task = TaskDens$new(id = "a", data, target = "A")
lrn = lrn("dens.kde", bandwidth = 0.1, kernel = "Norm")
p = lrn$train(task)
dist = p$model

test_that("pdf", {
  expect_equal(dist$pdf(0.5), 0.6160504979)
  # expect_equal(dist$pdf(0.5), 0.5)
  # expect_equal(dist$pdf(1.0), 0.75)
  # expect_equal(dist$pdf(1.5), 0.25)
  # expect_equal(dist$pdf(2.0), 0.25)
  # expect_equal(dist$pdf(-1), 0)
  # expect_equal(dist$pdf(3), 0)
})

