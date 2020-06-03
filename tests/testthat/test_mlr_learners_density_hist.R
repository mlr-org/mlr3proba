context("dens.hist")

test_that("autotest", {
  set.seed(1)
  learner = lrn("dens.hist")
  expect_learner(learner)
  result = run_autotest(learner, check_replicable = FALSE)
  expect_true(result, info = result$error)
})

data = data.frame("A" = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6))
a = graphics::hist(x = data$A, include.lowest = TRUE, plot = TRUE, right = FALSE, probability = T)

task = TaskDens$new(id = "a", data, target = "A")
lrn = lrn("dens.hist", breaks = 5)
p = lrn$train(task)
dist = p$model$distr

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


# test with multiple breaks
# ---------------------------
# data = data.frame("A" = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6))
# a = graphics::hist(x = data$A, breaks =c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1.0, 1.1, 1.5, 2),
#                     include.lowest = TRUE, plot = TRUE, right = FALSE, probability = T)
#
# task = TaskDens$new(id = "a", data, target = "A")
# lrn = lrn("dens.hist", breaks =c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1.0, 1.1, 1.5, 2))
# p = lrn$train(task)
# dist = p$model$distr
#
# test_that("pdf", {
#   expect_equal(dist$pdf(0), 0)
#   expect_equal(dist$pdf(0.5),  0.625)
#   expect_equal(dist$pdf(1.0), 1.25)
#   expect_equal(dist$pdf(1.1), 0.625)
#   expect_equal(dist$pdf(2.0), 0.25)
#   expect_equal(dist$pdf(-1), 0)
#   expect_equal(dist$pdf(3), 0)
# })
#
# test_that("cdf", {
#   expect_equal(dist$cdf(0), 0)
#   expect_equal(dist$cdf(0.5), 0.1875)
#   expect_equal(dist$cdf(1.0), 0.5)
#   expect_equal(dist$cdf(1.1), 0.625)
#   expect_equal(dist$cdf(2.0), 1)
#   expect_equal(dist$cdf(-1), 0)
#   expect_equal(dist$cdf(3), 1)
# })
