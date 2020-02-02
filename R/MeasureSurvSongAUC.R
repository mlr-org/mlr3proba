#' @template surv_measure
#' @templateVar title Song and Zhou's AUC
#' @templateVar inherit `MeasureSurvAUC`/[MeasureSurv]
#' @templateVar fullname MeasureSurvSongAUC
#' @templateVar shortname surv.songAUC
#' @templateVar pars integrated = TRUE, times, type = c("incident","cumulative")
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#' @templateVar type_par TRUE
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
