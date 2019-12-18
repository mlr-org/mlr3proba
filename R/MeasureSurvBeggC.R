#' @template surv_measure
#' @templateVar title Begg's C-Index
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvBeggC
#' @templateVar shortname surv.beggC
#'
#' @description
#' Calls [survAUC::BeggC()].
#'
#' Assumes Cox PH model specification.
#'
#' @template measure_survAUC
#'
#' @references
#' Begg, B. C., L. D. Craemer, E. S. Venkatraman and J. Rosai (2000).\cr
#' Comparing tumor staging and grading systems: a case study and a review of the issues, using thymoma as a model.\cr
#' Statistics in Medicine 19, 1997–2014.
#'
#' @family Concordance survival measures
#' @export
MeasureSurvBeggC = R6Class("MeasureSurvBeggC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.beggC",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = c("requires_learner", "requires_task", "requires_train_set")
      )
    },

    score_internal = function(prediction, learner, task, train_set, ...) {
      surv_train = task$truth(train_set)
      lp_train = learner$model$linear.predictors

      survAUC::BeggC(surv_train, prediction$truth, lp_train, prediction$lp)
    }
  )
)
