#' @title Song and Zhou's AUC
#'
#' @usage NULL
#' @aliases mlr_measures_surv.songAUC
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvSongAUC$new()
#' mlr_measures$get("surv.songAUC")
#' msr("surv.songAUC")
#' ```
#'
#' @description
#' Calls [survAUC::AUC.sh()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes Cox PH model specification.
#'
#' @references
#' Song, X. and X.-H. Zhou (2008).
#' A semiparametric approach for the covariate specific ROC curve with survival outcome.
#' Statistica Sinica 18, 947–965.
#'
#' @template seealso_measure
#' @export
MeasureSurvSongAUC = R6Class("MeasureSurvSongAUC",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(integrated = TRUE, times, type = c("incident","cumulative")) {
      super$initialize(integrated = integrated,
                       times = times,
                       id = "surv.songAUC",
                       properties = c("requires_learner", "requires_task", "requires_train_set")
                       )

      private$.type <- match.arg(type)
    },

    score_internal = function(prediction, learner, task, train_set, ...) {
      super$score_internal(prediction = prediction,
                           learner = learner,
                           task = task,
                           train_set = train_set,
                           FUN = survAUC::AUC.sh,
                           type = self$type,
                           ...)
    }
  ),

  active = list(
    type = function(type){
      if (mising(type)) {
        return(private$.type)
      } else {
        type = c("incident", "cumulative")[pmatch(type, c("incident","cumulative"))]
        if(is.na(type))
          stop("'type' must be on: 'incident', 'cumulative'. Abbreviations allowed.")
        private$.type <- type
      }
    }
  ),

  private = list(
    .type = character(0)
  )
)
