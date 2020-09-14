#' @template surv_measure
#' @templateVar title Song and Zhou's AUC
#' @templateVar fullname MeasureSurvSongAUC
#'
#' @template param_integrated
#' @template param_times
#' @template param_measure_type
#' @template field_measure_type
#'
#' @description
#' Calls [survAUC::AUC.sh()].
#'
#' Assumes Cox PH model specification.
#'
#' @template measure_survAUC
#'
#' @references
#' \cite{mlr3proba}{song_2008}
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvSongAUC = R6Class("MeasureSurvSongAUC",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times, type = c("incident", "cumulative")) {
      super$initialize(
        integrated = integrated,
        times = times,
        id = "surv.song_auc",
        properties = c("requires_learner", "requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.song_auc"
      )

      private$.type = match.arg(type)
    }
  ),

  active = list(
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
    .type = character(0),
    .score = function(prediction, learner, task, train_set, ...) {
      super$.score(
        prediction = prediction,
        learner = learner,
        task = task,
        train_set = train_set,
        FUN = survAUC::AUC.sh,
        type = self$type,
        ...)
    }
  )
)
