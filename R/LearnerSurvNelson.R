#' @template surv_learner
#' @templateVar title Nelson-Aalen Estimator
#' @templateVar fullname LearnerSurvNelson
#' @templateVar caller [survival::survfit()]
#' @templateVar distr by estimating the cumulative hazard function with [survival::survfit()]
#'
#' @references
#' \cite{mlr3proba}{nelson_1969}
#'
#' \cite{mlr3proba}{nelson_1972}
#'
#' \cite{mlr3proba}{aalen_1978}
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
