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
#' `r format_bib("kaplan_1958")`
#'
#' @export
LearnerSurvKaplan = R6Class("LearnerSurvKaplan",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.kaplan",
        predict_types = c("crank", "distr"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = "missings",
        packages = c("survival", "distr6"),
        man = "mlr3proba::mlr_learners_surv.kaplan"
      )
    }
  ),

  private = list(
    .train = function(task) {
      invoke(survival::survfit, formula = task$formula(1), data = task$data())
    },

    .predict = function(task) {

      # Define WeightedDiscrete distr6 distribution from the survival function
      times = self$model$time
      surv = matrix(rep(self$model$surv, task$nrow), ncol = length(times), nrow = task$nrow,
                    byrow = TRUE)

      .surv_return(times = times, surv = surv)
    }
  )
)
