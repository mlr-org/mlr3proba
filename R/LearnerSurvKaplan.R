#' @template surv_learner
#' @templateVar title Kaplan-Meier Estimator
#' @templateVar fullname LearnerSurvKaplan
#' @templateVar caller [survival::survfit()]
#' @templateVar distr by estimating the survival function with [survival::survfit()]
#'
#' @description
#'
#'
#' @references
#' \cite{mlr3proba}{kaplan_1958}
#'
#' @export
LearnerSurvKaplan = R6Class("LearnerSurvKaplan", inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.kaplan",
        predict_types = c("crank", "distr"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = "missings",
        packages = c("survival", "distr6")
      )
    }
  ),

  private = list(
    .train = function(task) {
      invoke(survival::survfit, formula = task$formula(1), data = task$data())
    },

    .predict = function(task) {
      # Ensures that at all times before the first observed time the survival is 1, as expected.
      # surv = c(1, self$model$surv)
      # time = c(0, self$model$time)

      # Define WeightedDiscrete distr6 distribution from the survival function
      x = rep(list(data = data.frame(x = self$model$time, cdf = 1 - self$model$surv)), task$nrow)
      distr = distr6::VectorDistribution$new(distribution = "WeightedDiscrete", params = x,
                                             decorators = c("CoreStatistics", "ExoticStatistics"))

      # Define crank as the mean of the survival distribution
      crank = as.numeric(sum(x[[1]][,1] * c(x[[1]][,2][1], diff(x[[1]][,2]))))

      PredictionSurv$new(task = task, crank = rep(crank, task$nrow), distr = distr)
    }
  )
)
