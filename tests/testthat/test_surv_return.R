# make a survival array for testing
set.seed(1L)
n = 7 # Number of observations
k = 8 # Number of times
w = 3 # Number of curves

surv_array = array(dim = c(n, k, w), dimnames = list(NULL, seq(1:k), NULL))
for (i in 1:w) {
  # Create the matrix with random values between 0 and 1
  mat = matrix(runif(n * k, min = 0, max = 1), nrow = n, ncol = k)

  # Sort the values within each row in descending order (survival probabilities)
  surv_array[, , i] = t(apply(mat, 1L, function(row) sort(row, decreasing = TRUE)))
}

test_that("surv_return input check", {
  arr = array(data = 0, dim = c(1, 1, 1, 1))
  expect_error(surv_return(surv = arr), "3D survival arrays supported only")

  expect_error(surv_return(surv = surv_array, times = 1:4), "must have the same")
  expect_error(surv_return(surv = surv_array, which.curve = -1), "has to be")
  expect_error(surv_return(surv = surv_array, which.curve = "aaa"), "has to be")
  expect_error(surv_return(surv = surv_array, which.curve = 5), "third dimension")
})

test_that("surv_return works correctly when input is survival matrix or array", {
  # input surv => matrix
  res1 = surv_return(surv = surv_array[, , 1L])
  res2 = surv_return(surv = surv_array[, , 2L])
  surv_mat_mean = apply(surv_array, 1:2, mean)
  surv_mat_median = apply(surv_array, 1:2, median)
  res_mean = surv_return(surv = surv_mat_mean) # array + which.curve = 'mean'
  res_median = surv_return(surv = surv_mat_median) # array + which.curve = 0.5

  # input surv => 3d array
  res11 = surv_return(surv = surv_array, which.curve = 1)
  res22 = surv_return(surv = surv_array, which.curve = 2)
  res_mean1 = surv_return(surv = surv_array, which.curve = "mean") # 'mean' curve
  res_median1 = surv_return(surv = surv_array) # default => median curve
  res_median2 = surv_return(surv = surv_array, which.curve = 0.5) # same as above

  # surv_array is returned unchanged
  expect_equal(res11$distr, surv_array)
  expect_equal(res22$distr, surv_array)
  expect_equal(res_mean1$distr, surv_array)
  expect_equal(res_median1$distr, surv_array)
  expect_equal(res_median2$distr, surv_array)

  # compare cranks
  expect_equal(res1$crank, res11$crank)
  expect_equal(res2$crank, res22$crank)
  expect_equal(res_mean$crank, res_mean1$crank)
  expect_equal(res_median$crank, res_median1$crank)
  expect_equal(res_median$crank, res_median2$crank)
  expect_true(all(res11$crank != res22$crank))
  expect_true(all(res_mean1$crank != res_median1$crank))

  # lp is given so crank it taken from there
  res3 = surv_return(surv = surv_array, lp = 1:n)
  expect_equal(res3$crank, 1:n)
})

test_that("edge cases work", {
  # minimal matrix 1: few observations, one time point
  min_mat1 = matrix(surv_array[1:3, 1, 1], nrow = 3L, dimnames = list(NULL, 1))
  res1 = surv_return(surv = min_mat1)
  expect_equal(res1$distr, min_mat1)
  expect_equal(res1$crank, as.vector(-log(min_mat1))) # expected mortality

  # minimal matrix 2: one observation, few time points (vector essentially)
  min_vec2 = surv_array[1, 1:3, 1]
  min_mat2 = matrix(min_vec2, nrow = 1L, dimnames = list(NULL, names(min_vec2)))
  res2 = surv_return(surv = min_vec2)
  res22 = surv_return(surv = min_mat2)
  expect_equal(res2, res22)
  expect_equal(res2$distr, min_mat2) # returns the matrix
  expect_equal(res2$crank, sum(-log(min_vec2))) # expected mortality

  # minimal array 1: few obs, one time point, few curves
  arrmin1 = surv_array[1:3, 1L, , drop = FALSE]
  res_arr1 = surv_return(surv = arrmin1, which.curve = 2)
  expect_equal(res_arr1$distr, arrmin1)
  expect_equal(res_arr1$crank, -log(arrmin1[, , 2])) # expected mortality

  # minimal array 2: one obs, few time points, few curves
  arrmin2 = surv_array[1L, 1:3, , drop = FALSE]
  res_arr2 = surv_return(surv = arrmin2, which.curve = 2)
  expect_equal(res_arr2$distr, arrmin2)
  expect_equal(res_arr2$crank, sum(-log(arrmin2[, , 2]))) # expected mortality

  # minimal array 3: one obs, one time point, one curve!
  arrmin3 = surv_array[1L, 1L, 1L, drop = FALSE]
  res_arr3 = surv_return(surv = arrmin3, which.curve = "mean")
  expect_equal(res_arr3$distr, arrmin3)
  expect_equal(res_arr3$crank, unname(-log(arrmin3[, , 1L]))) # expected mortality
})
