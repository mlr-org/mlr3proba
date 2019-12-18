#' @template surv_measure
#' @templateVar title Xu and O'Quigley's R2
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvXuR2
#' @templateVar shortname surv.xuR2
#' @description
#' Calls [survAUC::XO()].
#'
#' Assumes Cox PH model specification.
#'
#' @template measure_survAUC
#'
#' @references
#' Xu, R. and J. O'Quigley (1999).\cr
#' A measure of dependence for proportional hazards models.\cr
#' Journal of Nonparametric Statistics 12, 83–107.\cr
#'
#' @family R2 survival measures
#' @export
MeasureSurvXuR2 = R6Class("MeasureSurvXuR2",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.xuR2",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = c("requires_task", "requires_train_set")
      )
    },

    score_internal = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)

      survAUC::XO(surv_train, prediction$lp, rep(0, length(prediction$lp)))
    }
  )
)
