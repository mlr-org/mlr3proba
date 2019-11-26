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

  # get indicator of whether a patient is alive >= t
  alive = matrix(obs_times, nrow = nr_obs, ncol = nc_times) >= matrix(unique_times, nrow = nr_obs, ncol = nc_times, byrow = T)

  surv = transpose(1 - distribution$cdf(unique_times))

  graf = (alive - surv)^2

  # get indicator of whether a patient is censored > t
  censored = truth[, 2] == 0
  cens_ind = matrix(truth[censored, 1], nrow = sum(censored), ncol = nc_times) >
    matrix(unique_times, nrow = sum(censored), ncol = nc_times, byrow = T)

  # for everyone who was censored, set their graf score to 0 at all times > censoring time
  graf[censored, ] = graf[censored, ] * cens_ind

  # find the number of 'times' a patient is not censored
  n = rep(nc_times, nr_obs)
  n[censored] = match(truth[censored, 1], unique_times)

  # finally get a score for each individual by taking the weighted average over non-censored times
  #  and then a single score by taking the mean over all individuals
  rowSums(graf)/n
}
