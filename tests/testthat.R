if (requireNamespace("testthat", quietly = TRUE)) {
  library(checkmate)
  library(testthat)
  library(mlr3pro)
  library(mlr3)
  library(distr6)

  test_check("mlr3pro")
}
