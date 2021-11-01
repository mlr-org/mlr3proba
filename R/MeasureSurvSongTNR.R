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
    initialize = function() {
      ps = ps(
        times = p_dbl(0),
        lp_thresh = p_dbl(default = 0)
      )
      ps$values = list(lp_thresh = 0)

      super$initialize(
        id = "surv.song_tnr",
        properties = c("requires_task", "requires_train_set", "requires_learner"),
        man = "mlr3proba::mlr_measures_surv.song_tnr",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, task, train_set, ...) {
      if (is.null(self$param_set$values$times)) {
        stop("`times` must be non-NULL")
      }

      tnr = super$.score(
        prediction = prediction,
        learner = learner,
        task = task,
        train_set = train_set,
        FUN = survAUC::spec.sh,
        ...
      )

      tnr[, findInterval(self$param_set$values$lp_thresh, sort(unique(prediction$lp)))]
    }
  )
)
