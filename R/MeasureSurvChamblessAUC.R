#' @template surv_measure
#' @templateVar title Chambless and Diao's AUC
#' @templateVar inherit `MeasureSurvAUC`/[MeasureSurv]
#' @templateVar fullname MeasureSurvChamblessAUC
#' @templateVar shortname surv.chamblessAUC
#' @templateVar pars integrated = TRUE, times
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#'
#' @description
#' Calls [survAUC::AUC.cd()].
#'
#' Assumes Cox PH model specification.
#'
#' @template measure_survAUC
#'
#' @references
#' Chambless, L. E. and G. Diao (2006).\cr
#' Estimation of time-dependent area under the ROC curve for long-term risk prediction.\cr
#' Statistics in Medicine 25, 3474–3486.
#'
#' @family AUC survival measures
#' @export
MeasureSurvChamblessAUC = R6Class("MeasureSurvChamblessAUC",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(integrated = TRUE, times) {
      super$initialize(integrated = integrated,
                       times = times,
                       id = "surv.chamblessAUC",
                       properties = c("requires_learner", "requires_task", "requires_train_set"))
    },

    score_internal = function(prediction, learner, task, train_set, ...) {
      super$score_internal(prediction = prediction,
                           learner = learner,
                           task = task,
                           train_set = train_set,
                           FUN = survAUC::AUC.cd)
    }
  )
)
