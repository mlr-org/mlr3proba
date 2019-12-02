#' @template surv_learner
#' @templateVar title Kaplan-Meier Estimator
#' @templateVar fullname LearnerSurvKaplan
#' @templateVar caller [survival::survfit()]
#' @templateVar distr by estimating the survival function with [survival::survfit()]
#'
#' @references
#' Kaplan, E. L. and Meier, P. (1958).
#' Nonparametric Estimation from Incomplete Observations.
#' Journal of the American Statistical Association, 53(282), 457-481.
#' \doi{10.2307/2281868}.
#'
#' @export
LearnerSurvKaplan = R6Class("LearnerSurvKaplan", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.kaplan",
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
