#' @title Integrated Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.loglossint
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvLoglossInt$new()
#' mlr_measures$get("surv.loglossint")
#' msr("surv.loglossint")
#' ```
#'
#' @description
#' Calculates the integrated cross-entropy, or logarithmic, loss.
#'
#' @details
#' The integrated logloss, in the context of probabilistic predictions, is defined as the classical
#' logloss for a given individual, integrated over all time-points.
#'
#' @template seealso_measure
#' @export
MeasureSurvLoglossInt = R6::R6Class("MeasureSurvLoglossInt",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.loglossint",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
        )
      },

    score_internal = function(prediction, ...) {
        mean(integrated_logloss(prediction$truth, prediction$distr))
      }
  )
)

integrated_logloss = function(truth, distribution){
  # get unique observation times (death and censoring) and order accordingly
  obs_times = truth[ ,1]
  unique_times = sort(unique(obs_times))
  nr_obs = length(obs_times)
  nc_times = length(unique_times)

  # evaluate the survival at every unique time
  surv = 1 - transpose(distribution$cdf(unique_times))

  # get indicator of whether a patient is alive >= t
  alive = matrix(obs_times, nrow = nr_obs, ncol = nc_times) >= matrix(unique_times, nrow = nr_obs, ncol = nc_times, byrow = T)

  # if a patient is alive at t then find the survival, otherwise find cdf
  ll = (surv * alive) + ((1 - surv) * (1 - alive))
  # set prediction to be very small but non-zero then find negative log
  ll[ll == 0] = 1e-15
  ll = -log(ll)

  # get indicator of whether a patient is censored > t
  censored = truth[, 2] == 0
  cens_ind = matrix(truth[censored, 1], nrow = sum(censored), ncol = nc_times) >
    matrix(unique_times, nrow = sum(censored), ncol = nc_times, byrow = T)

  # for everyone who was censored, set their logloss to 0 at all times > censoring time
  ll[censored, ] = ll[censored, ] * cens_ind

  # find the number of 'times' a patient is not censored
  n = rep(nc_times, nr_obs)
  n[censored] = match(truth[censored, 1], unique_times)

  # finally get a score for each individual by taking the weighted average over non-censored times
  #  and then a single score by taking the mean over all individuals
  rowSums(ll)/n
}
