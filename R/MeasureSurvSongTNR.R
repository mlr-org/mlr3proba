#' @template surv_measure
#' @templateVar title Song and Zhou's TNR
#' @templateVar fullname MeasureSurvSongTNR
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
#' @template param_times
#' @template param_thresh
#' @template field_thresh
#'
#' @references
#' `r format_bib("song_2008")`
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvSongTNR = R6Class("MeasureSurvSongTNR",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(times = 0, lp_thresh = 0) {
      assertNumeric(times, len = 1)

      super$initialize(
        integrated = FALSE,
        times = times,
        id = "surv.song_tnr",
        properties = c("requires_task", "requires_train_set", "requires_learner"),
        man = "mlr3proba::mlr_measures_surv.song_tnr"
      )

      assertNumeric(lp_thresh, len = 1)
      private$.lp_thresh = lp_thresh
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
    .lp_thresh = numeric(0),
    .score = function(prediction, learner, task, train_set, ...) {
      tnr = super$.score(
        prediction = prediction,
        learner = learner,
        task = task,
        train_set = train_set,
        FUN = survAUC::spec.sh,
        ...
      )

      tnr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))]
    }
  )
)
