library(mlr3)
library(mlr3proba)
library(checkmate)
library(testthat)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]",
                  full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3proba"), pattern = "^helper.*\\.[rR]",
                  full.names = TRUE), source)
