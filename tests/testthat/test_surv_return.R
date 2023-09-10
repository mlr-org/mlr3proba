# make a survival array for testing
set.seed(1)
n = 7 # Number of observations
k = 8 # Number of times
w = 3 # Number of curves

surv_array = array(dim = c(n,k,w), dimnames = list(NULL, seq(1:k), NULL))
for (i in 1:w) {
  # Create the matrix with random values between 0 and 1
  mat = matrix(runif(n * k, min = 0, max = 1), nrow = n, ncol = k)

  # Sort the values within each row in descending order (survival probabilities)
  surv_array[,,i] = t(apply(mat, 1, function(row) sort(row, decreasing = TRUE)))
}

test_that("surv_return input check", {
  arr = array(data = 0, dim = c(1,1,1,1))
  expect_error(.surv_return(surv = arr), "3D survival arrays supported only")

  expect_error(.surv_return(surv = surv_array, which.curve = -1), "has to be")
  expect_error(.surv_return(surv = surv_array, which.curve = 'aaa'), "has to be")
  expect_error(.surv_return(surv = surv_array, which.curve = 5), "third dimension")
})

test_that("surv_return works correctly when input is survival matrix or array", {
  # input surv => matrix
  res1 = .surv_return(surv = surv_array[,,1])
  res2 = .surv_return(surv = surv_array[,,2])
  surv_mat_mean = apply(surv_array, c(1,2), mean)
  res3 = .surv_return(surv = surv_mat_mean) # same as which.curve = 'mean' later

  # input surv => 3d array
  res4 = .surv_return(surv = surv_array) # default => 'mean' curve
  expect_equal(res4$distr, surv_array)
  expect_vector(res4$crank, size = dim(surv_array)[1L])
  expect_equal(res4$crank, res3$crank)

  res5 = .surv_return(surv = surv_array, which.curve = 'mean')
  expect_equal(res4, res5)

  # different values for 'which.curve'
  res6 = .surv_return(surv = surv_array, which.curve = 1)
  res7 = .surv_return(surv = surv_array, which.curve = 2)

  # survival arrays the same, cranks different
  expect_equal(res6$distr, res7$distr)
  expect_true(all(res6$crank != res7$crank))
  # cranks the same as using just the survival matrices
  expect_equal(res6$crank, res1$crank)
  expect_equal(res7$crank, res2$crank)

  # lp is given so crank it taken from there
  res8 = .surv_return(surv = surv_array, lp = 1:n)
  expect_equal(res8$distr, surv_array)
  expect_equal(res8$crank, 1:n)
})
