#' @title Graf Score
#'
#' @usage NULL
#' @aliases mlr_measures_surv.graf
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvGraf$new()
#' mlr_measures$get("surv.graf")
#' msr("surv.graf")
#' ```
#'
#' @description
#' Calculates the Graf score, aka integrated Brier survival score or squared loss.
#'
#' @references
#' Graf, G. (1950).
#' Verification of forecasts expressed in terms of probability.
#' Monthly Weather Review, 78(1), 1-3.
#' \doi{10.1175/1520-0493(1950)078<0001:VOFEIT>2.0.CO;2}
#'
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).
#' Assessment and comparison of prognostic classification schemes for survival data.
#' Statistics in Medicine, 18(17), 2529-2545.
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
#'
#' @template seealso_measure
#' @export
MeasureSurvGraf = R6::R6Class("MeasureSurvGraf",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.graf",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
      )
    },

    score_internal = function(prediction, ...) {
      mean(graf(prediction$truth, prediction$distr))
    }
  )
)

graf = function(truth, distribution) {
  # get unique observation times (death and censoring) and order accordingly
  obs_times = truth[ ,1]
  unique_times = sort(unique(obs_times))
  nr_obs = length(obs_times)
  nc_times = length(unique_times)

  # get indicator of whether a patient is alive > t
  alive = matrix(obs_times, nrow = nr_obs, ncol = nc_times) >
    matrix(unique_times, nrow = nr_obs, ncol = nc_times, byrow = T)
  alive = apply(alive, 2, as.numeric)

  # calculate unweighted graf score at time t* as G(t*) = (I(t > t*) - S(t*))^2
  graf = (alive - transpose(1 - distribution$cdf(unique_times)))^2

  # To account for censoring the score is weighted according to each individuals contribution to
  # censoring. If an individual is censored they don't contribute to Graf score but contribute
  # to censoring distr.
  # G(t*) = S(t*)^2 * I(t <= t*, died = 1) * (1/C(t)) + (1-S(t*))^2 * I(t > t*) * (1/C(t*))
  # where C(t) is Kaplan-Meier estimator for censoring distribution

  # First get matrix of observed death/censor times and matrix of all unique times
  obs_times_mat = matrix(obs_times, nrow = nr_obs, ncol = nc_times, byrow = F)
  unique_times_mat = matrix(unique_times, nrow = nr_obs, ncol = nc_times, byrow = T)

  # For a given patient's true death/censor time, t, if t <= t* then they are weighted with C(t).
  # If t > t* then weighted with C(t*)
  weights_mat = matrix(0, nrow = nr_obs, ncol = nc_times)
  weights_mat = weights_mat + (alive * unique_times_mat) + ((1 - alive) * obs_times_mat)

  # Find the Kaplan-Meier estimate of the censoring distribution, save as distr6 object
  cens_dist = survfit(Surv(time, event) ~ 1, data = data.frame(time = truth[,1], event = 1-truth[,2]))
  cens_dist = distr6::WeightedDiscrete$new(data.frame(x = cens_dist$time, cdf = 1 - cens_dist$surv),
                                      decorators = "ExoticStatistics")

  # Find the estimated censoring probability at each of the respective times derived above
  weights_mat = apply(weights_mat, 1, function(x) cens_dist$survival(x))

  weighted_graf = as.matrix(graf / weights_mat)

  # Censoring does not contribute to the Graf score, so remove erroneous calculations
  # when censored
  weighted_graf[truth[, 2] == 0, ][alive[truth[, 2] == 0, ] == 0] = NaN

  # Finally approximate integration by taking the mean over all unique time-points
  rowSums(weighted_graf, na.rm = TRUE)/apply(weighted_graf, 1, function(x) sum(!is.nan(x)))
}
