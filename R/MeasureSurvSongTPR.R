#' @template surv_measure
#' @templateVar title Song and Zhou's TPR
#' @templateVar inherit `MeasureSurvAUC`/[MeasureSurv]
#' @templateVar fullname MeasureSurvSongTPR
#' @templateVar shortname surv.songTPR
#' @templateVar pars times = 0, lp_thresh = 0, type = c("incident","cumulative")
#' @templateVar times_par TRUE
#' @templateVar thresh_par TRUE
#' @templateVar type_par TRUE
#'
#' @description
#' Calls [survAUC::sens.sh()].
#'
#' Assumes Cox PH model specification.
#'
#' `times` and `lp_thresh` are arbitrarily set to `0` to prevent crashing, these should be further
#' specified.
#'
#' @template measure_survAUC
#'
#' @references
#' \cite{mlr3proba}{song_2008}
#'
#' @family AUC survival measures
#' @export
MeasureSurvSongTPR = R6Class("MeasureSurvSongTPR",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(times = 0, lp_thresh = 0, type = c("incident","cumulative")) {

      assertNumeric(times, len = 1)

      super$initialize(integrated = FALSE,
                       times = times,
                       id = "surv.songTPR",
                       properties = c("requires_task", "requires_train_set", "requires_learner"))

      assertNumeric(lp_thresh, len = 1)
      private$.lp_thresh = lp_thresh
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

      tpr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))]
    }
  ),

  active = list(
    lp_thresh = function(lp_thresh) {
      if (missing(lp_thresh)) {
        return(private$.lp_thresh)
      } else {
        assertNumeric(lp_thresh, len = 1)
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
