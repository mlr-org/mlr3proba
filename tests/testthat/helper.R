library(checkmate)
library(mlr3)
library(mlr3misc)

# source helper files from mlr3 and mlr3proba
lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]",
  full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3proba"), pattern = "^helper.*\\.[rR]",
  full.names = TRUE), source)

assert_ro_binding = function(rhs) {
  if (!missing(rhs)) {
    stopf("Field/Binding is read-only")
  }
}

# substitute survival matrix with array (3d) in a 'PredictionSurv'
reshape_distr_to_3d = function(p, num_seq = seq(0.1, 0.2, 0.05)) {
  p2 = p$clone()
  surv_mat = p2$data$distr
  p2$data$distr = abind::abind(map(num_seq, function(n) surv_mat - n), along = 3L)
  p2
}
