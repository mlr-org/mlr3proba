#' @template surv_measure
#' @templateVar title Song and Zhou's TNR
#' @templateVar inherit `MeasureSurvAUC`/[MeasureSurv]
#' @templateVar fullname MeasureSurvSongTNR
#' @templateVar shortname surv.songTNR
#' @templateVar pars times = 0, lp_thresh = 0
#' @templateVar times_par TRUE
#' @templateVar thresh_par TRUE
#'
#' @description
#' Calls [survAUC::spec.sh()].
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
MeasureSurvSongTNR = R6Class("MeasureSurvSongTNR",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(times = 0, lp_thresh = 0) {

      assertNumeric(times, len = 1)

      super$initialize(integrated = FALSE,
                       times = times,
                       id = "surv.songTNR",
                       properties = c("requires_task", "requires_train_set", "requires_learner"))

      assertNumeric(lp_thresh, len = 1)
      private$.lp_thresh = lp_thresh
    },

    score_internal = function(prediction, learner, task, train_set, ...) {
      tnr = super$score_internal(prediction = prediction,
                                 learner = learner,
                                 task = task,
                                 train_set = train_set,
                                 FUN = survAUC::spec.sh,
                                 ...
      )

      tnr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))]
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
    }
  ),

  private = list(
    .lp_thresh = numeric(0)
  )
)
