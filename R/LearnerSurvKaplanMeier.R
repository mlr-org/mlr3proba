#' @title Kaplan Meier Estimator Survival Learner
#'
#' @usage NULL
#' @aliases mlr_learners_surv.kaplan
#' @format [R6::R6Class()] inheriting from [LearnerSurv].
#' @include LearnerSurv.R
#'
#' @section Construction:
#' ```
#' LearnerSurvKaplanMeier$new()
#' mlr_learners$get("surv.kaplan")
#' lrn("surv.kaplan")
#' ```
#'
#' @description
#' Kaplan Meier estimator called from [survival::survfit()] in package \CRANpkg{survival}.
#'
#' @references
#' Kaplan, E. L. and Meier, P. (1958).
#' Nonparametric Estimation from Incomplete Observations.
#' Journal of the American Statistical Association, 53(282), 457-481.
#' \doi{10.2307/2281868}.
#'
#' @template seealso_learner
#'
#' @export
LearnerSurvKaplanMeier = R6Class("LearnerSurvKaplanMeier", inherit = LearnerSurv,
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
      surv = c(1, self$model$surv)
      time = c(0, self$model$time)

      # Define WeightedDiscrete distr6 distribution from the survival function
      distr = suppressAll(
        distr6::WeightedDiscrete$new(data.frame(x = time, cdf = 1 - surv),
                                     decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics)))

      # Define crank as the mean of the survival distribution
      crank = distr$mean()

      PredictionSurv$new(task = task, crank = rep(crank, task$nrow), distr = rep(list(distr), task$nrow))
    }
  )
)
