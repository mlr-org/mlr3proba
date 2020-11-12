#' @template surv_measure
#' @templateVar title Song and Zhou's TPR
#' @templateVar fullname MeasureSurvSongTPR
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
#' @template param_times
#' @template param_thresh
#' @template param_measure_type
#' @template field_thresh
#' @template field_measure_type
#'
#' @references
#' `r format_bib("song_2008")`
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvSongTPR = R6Class("MeasureSurvSongTPR",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(times = 0, lp_thresh = 0, type = c("incident", "cumulative")) {
      assertNumeric(times, len = 1)

      super$initialize(
        integrated = FALSE,
        times = times,
        id = "surv.song_tpr",
        properties = c("requires_task", "requires_train_set", "requires_learner"),
        man = "mlr3proba::mlr_measures_surv.song_tpr"
      )

      assertNumeric(lp_thresh, len = 1)
      private$.lp_thresh = lp_thresh
      private$.type = match.arg(type)
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

    type = function(type) {
      if (missing(type)) {
        return(private$.type)
      } else {
        type = c("incident", "cumulative")[pmatch(type, c("incident", "cumulative"))]
        if (is.na(type)) {
          stop("'type' must be on: 'incident', 'cumulative'. Abbreviations allowed.")
        }
        private$.type = type
      }
    }
  ),

  private = list(
    .lp_thresh = numeric(0),
    .type = character(0),
    .score = function(prediction, learner, task, train_set, ...) {
      tpr = super$.score(
        prediction = prediction,
        learner = learner,
        task = task,
        train_set = train_set,
        FUN = survAUC::sens.sh,
        type = self$type,
        ...
      )

      tpr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))]
    }
  )
)
