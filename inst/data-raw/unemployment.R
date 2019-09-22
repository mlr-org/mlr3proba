devtools::load_all()
data = mlr3misc::load_dataset("UnempDur", "Ecdat")
data = data[, c("spell", "censor1", "age", "ui", "logwage", "tenure")]

data$censor1 = as.integer(data$censor1)
data$tenure = as.integer(data$tenure)

saveRDS(data,
  file = file.path(system.file("extdata", package = "mlr3survival"), "unemployment.rds"),
  compress = "xz",
  version = 2L
)
