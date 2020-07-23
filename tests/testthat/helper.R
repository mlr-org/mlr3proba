library(mlr3proba)
library(checkmate)
library(testthat)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]",
                  full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3proba"), pattern = "^helper.*\\.[rR]",
                  full.names = TRUE), source)

assert_ro_binding = function (rhs) {
  if (!missing(rhs)) {
    stopf("Field/Binding is read-only")
  }
}
