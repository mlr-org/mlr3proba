test_that("autotest", {
  set.seed(1)
  learner = lrn("dens.hist")
  expect_learner(learner)
  result = run_autotest(learner, check_replicable = FALSE)
  expect_true(result, info = result$error)
})

x = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6)
a = graphics::hist(x = x, include.lowest = TRUE, plot = TRUE, right = FALSE, probability = T)

task = TaskDens$new(id = "a", x)
lrn = lrn("dens.hist", breaks = 5)
p = lrn$train(task)
dist = p$model$dist

test_that("pdf", {
  expect_equal(dist$pdf(0), 0.5)
  expect_equal(dist$pdf(0.5), 0.5)
  expect_equal(dist$pdf(1.0), 0.75)
  expect_equal(dist$pdf(1.5), 0.25)
  expect_equal(dist$pdf(2.0), 0.25)
  expect_equal(dist$pdf(-1), 0)
  expect_equal(dist$pdf(3), 0)
})

test_that("cdf", {
  expect_equal(dist$cdf(0), 0)
  expect_equal(dist$cdf(0.5), 0.25)
  expect_equal(dist$cdf(1.0), 0.5)
  expect_equal(dist$cdf(1.5), 0.875)
  expect_equal(dist$cdf(2.0), 1)
  expect_equal(dist$cdf(-1), 0)
  expect_equal(dist$cdf(3), 1)
})
