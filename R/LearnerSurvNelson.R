#' @template surv_learner
#' @templateVar title Nelson-Aalen Estimator
#' @templateVar fullname LearnerSurvNelson
#' @templateVar caller [survival::survfit()]
#' @templateVar distr by estimating the cumulative hazard function with [survival::survfit()]
#'
#' @references
#' Nelson, W. (1969).
#' Hazard plotting for incomplete failure data.
#' Journal of Quality Technology, 1, 27–52.
#' \doi{10.1080/00224065.1969.11980344}.
#'
#' Nelson, W. (1972).
#' Theory and applications of hazard plotting for censored failure data.
#' Technometrics, 14, 945–965.
#' \doi{10.1080/00401706.1972.10488991}.
#'
#' Aalen, Odd (1978).
#' Nonparametric inference for a family of counting processes.
#' Annals of Statistics, 6(4), 701–726.
#'
#' @export
LearnerSurvNelson = R6Class("LearnerSurvNelson", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.nelson",
        predict_types = c("crank", "distr"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = "missings",
        packages = c("survival", "distr6")
     )
    },

    train_internal = function(task) {
      invoke(survival::survfit, formula = task$formula(1), data = task$data())
      },

    predict_internal = function(task) {
      # Ensures that at all times before the first observed time the cumulative hazard is 0, as expected.
      # cumhaz = c(0, self$model$cumhaz)
      # time = c(0, self$model$time)

      # Define WeightedDiscrete distr6 distribution from the cumulative hazard
      x = rep(list(data = data.frame(x = self$model$time, cdf = 1 - exp(-self$model$cumhaz))), task$nrow)
      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      # Define crank as the mean of the survival distribution
      crank = as.numeric(sum(x[[1]][,1] * c(x[[1]][,2][1], diff(x[[1]][,2]))))

      PredictionSurv$new(task = task, crank = rep(crank, task$nrow), distr = distr)
    }
  )
)
