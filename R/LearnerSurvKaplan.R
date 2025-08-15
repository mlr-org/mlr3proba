#' @templateVar title Kaplan-Meier Estimator
#' @templateVar fullname LearnerSurvKaplan
#' @templateVar caller [survival::survfit()]
#' @templateVar distr by estimating the survival function with [survival::survfit()]
#' @templateVar id surv.kaplan
#' @template surv_learner
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
        properties = c("missings", "weights", "importance", "selected_features"),
        packages = c("survival", "distr6"),
        label = "Kaplan-Meier Estimator",
        man = "mlr3proba::mlr_learners_surv.kaplan"
      )
    },

    #' @description
    #' All features have a score of `0` for this learner.
    #' #' This method exists solely for compatibility with the `mlr3` ecosystem,
    #' as this learner is used as a fallback for other survival learners that
    #' require an `importance()` method.
    #'
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      fn = self$model$features
      named_vector(fn, 0)
    },

    #' @description
    #' Selected features are always the empty set for this learner.
    #' This method is implemented only for compatibility with the `mlr3` API,
    #' as this learner does not perform feature selection.
    #'
    #' @return `character(0)`.
    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      character()
    }
  ),

  private = list(
    .train = function(task) {
      fit = invoke(
        survival::survfit,
        formula = task$formula(1),
        data = task$data(),
        .args = list(weights = private$.get_weights(task))
      )

      # keep features for importance
      list(model = fit, features = task$feature_names)
    },

    .predict = function(task) {
      times = self$model$model$time
      surv = matrix(rep(self$model$model$surv, task$nrow), ncol = length(times),
                    nrow = task$nrow, byrow = TRUE)

      .surv_return(times = times, surv = surv)
    }
  )
)

register_learner("surv.kaplan", LearnerSurvKaplan)
