weighted_survival_score = function(truth, distribution, times, loss, ...) {
  # get unique observation times (death and censoring) and order accordingly

  obs_times = truth[, 1]
  unique_times = sort(unique(obs_times))
  if (length(times) != 0) {
    times = unique(times[times >= min(unique_times) & times <= max(unique_times)])
    if (length(times) == 0) {
      stop(sprintf("Times are all outside the observed range, [%s,%s].", min(obs_times), max(obs_times)))
    }
    unique_times = unique(unique_times[findInterval(times, unique_times, TRUE, TRUE)])
  }

  nr_obs = length(obs_times)
  nc_times = length(unique_times)

  # get indicator of whether a patient is alive > t
  alive = matrix(obs_times, nrow = nr_obs, ncol = nc_times) >
    matrix(unique_times, nrow = nr_obs, ncol = nc_times, byrow = T)
  alive = apply(alive, 2, as.numeric)

  # calculated unweighted loss
  score = loss(alive = alive, distribution = distribution, unique_times = unique_times, ...)

  # To account for censoring the score is weighted according to each individuals contribution to
  # censoring.
  # G(t*) = L(S, t*)I(t <= t*, died = 1)(1/C(t)) + L(S, t*)I(t > t*)(1/C(t*))
  # where C(t) is Kaplan-Meier estimator for censoring distribution

  # First get matrix of observed death/censor times and matrix of all unique times
  obs_times_mat = matrix(obs_times, nrow = nr_obs, ncol = nc_times, byrow = F)
  unique_times_mat = matrix(unique_times, nrow = nr_obs, ncol = nc_times, byrow = T)

  # For a given patient's true death/censor time, t, if t <= t* then they are weighted with C(t).
  # If t > t* then weighted with C(t*)
  weights_mat = matrix(0, nrow = nr_obs, ncol = nc_times)
  weights_mat = weights_mat + (alive * unique_times_mat) + ((1 - alive) * obs_times_mat)

  # Find the Kaplan-Meier estimate of the censoring distribution, save as distr6 object
  cens_dist = survival::survfit(Surv(time, 1 - event) ~ 1, data = data.frame(time = truth[, 1], event = truth[, 2]))
  cens_dist = distr6::WeightedDiscrete$new(data.frame(x = cens_dist$time, cdf = 1 - cens_dist$surv),
    decorators = "ExoticStatistics")

  # Find the estimated censoring probability at each of the respective times derived above
  weights_mat = apply(weights_mat, 1, function(x) cens_dist$survival(x))

  weighted_score = as.matrix(score / t(weights_mat))

  # Censored individuals contribute zero to the score.
  weighted_score[truth[, 2] == 0, ][alive[truth[, 2] == 0, ] == 0] = 0

  colnames(weighted_score) = unique_times
  weighted_score
}
