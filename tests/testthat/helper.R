library(checkmate)
library(mlr3)
library(mlr3misc)

# source helper files from mlr3 and mlr3proba
lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]",
  full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3proba"), pattern = "^helper.*\\.[rR]",
  full.names = TRUE), source)

# substitute survival matrix with array (3d) in a 'PredictionSurv'
reshape_distr_to_3d = function(p, num_seq = seq(0.1, 0.2, 0.05)) {
  p2 = p$clone()
  surv_mat = p2$data$distr
  p2$data$distr = abind::abind(map(num_seq, function(n) surv_mat - n), along = 3L)
  p2
}

gen_cmprsk_task = function(n = 50, n_events = 2) {
  # Generate exp distribution for event times
  times = stats::rexp(n, rate = 0.2)

  # Generate competing risks event types (0 = censored, 1 to n_events = events)
  event = sample(0:n_events, size = n, replace = TRUE,
                 prob = c(0.3, rep(0.7 / n_events, n_events)))

  # Covariate (could be expanded)
  x = runif(n)

  # Create data frame
  df = data.frame(time = times, event = event, x = x)

  # Return a TaskCompRisks object
  TaskCompRisks$new(id = "test", backend = df)
}

# Generate a list of CIF matrices
gen_cif = function(n = 20, n_events = 2, n_times = 20) {
  cif_list = lapply(1:n_events, function(i) {
    # Randomly choose the number of time points for this event
    k = sample(n_times, 1)

    # Generate a CIF matrix where each row starts at 0 and increases
    matrix(apply(matrix(runif(n * k, min = 0, max = 1), nrow = n), 1, function(x) {
      c(0, sort(x)) # Ensure first value is 0 and sequence is increasing
    }), nrow = n, byrow = TRUE) |> set_col_names(1:(k+1))
  })
  names(cif_list) = 1:n_events

  cif_list
}
