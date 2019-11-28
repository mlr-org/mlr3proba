#' @title Song and Zhou's TPR
#'
#' @usage NULL
#' @aliases mlr_measures_surv.songTPR
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvSongTPR$new()
#' mlr_measures$get("surv.songTPR")
#' msr("surv.songTPR")
#' ```
#'
#' @description
#' Calls [survAUC::sens.sh()].
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
MeasureSurvSongTPR = R6Class("MeasureSurvSongTPR",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(times, lp_thresh, type = c("incident","cumulative")) {
      super$initialize(integrated = FALSE,
                       times = times,
                       id = "surv.songTPR",
                       properties = c("requires_task", "requires_train_set", "requires_learner"))

      if(missing(lp_thresh))
        private$.lp_thresh = numeric(0)
      else {
        assertNumeric(lp_thresh)
        private$.lp_thresh = lp_thresh
      }

      private$.type <- match.arg(type)
    },

    score_internal = function(prediction, learner, task, train_set, ...) {
      tpr = super$score_internal(prediction = prediction,
                                 learner = learner,
                                 task = task,
                                 train_set = train_set,
                                 FUN = survAUC::sens.sh,
                                 type = self$type,
                                 ...
      )

      if(length(self$lp_thresh) == 0)
        return(list(tpr = tpr, thresh = sort(unique(prediction$lp))))
      else
        return(tpr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))])
    }
  ),

  active = list(
    lp_thresh = function(lp_thresh) {
      if (missing(lp_thresh)) {
        return(private$.lp_thresh)
      } else {
        assertNumeric(lp_thresh)
        private$.lp_thresh = lp_thresh
      }
    },

    type = function(type){
      if (missing(type)) {
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
    .lp_thresh = numeric(0),
    .type = character(0)
  )
)
