#' @template surv_measure
#' @templateVar title Song and Zhou's AUC
#' @templateVar fullname MeasureSurvSongAUC
#'
#' @template param_integrated
#' @template param_times
#' @template param_measure_type
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
    initialize = function() {
      ps = ps(
        times = p_uty(),
        integrated = p_lgl(default = TRUE),
        type = p_fct(c("incident", "cumulative"), default = "incident")
      )
      ps$values = list(integrated = TRUE, type = "incident")

      super$initialize(
        id = "surv.song_auc",
        properties = c("requires_learner", "requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.song_auc",
        label = "Song and Zhou's AUC",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, task, train_set, ...) {
      if (!inherits(learner, "LearnerSurvCoxPH")) {
        stop("surv.song_auc only compatible with Cox PH models")
      }
      ps = self$param_set$values
      if (!ps$integrated) {
        msg = "If `integrated=FALSE` then `times` should be a scalar numeric."
        assert_numeric(ps$times, len = 1, .var.name = msg)
      } else {
        if (!is.null(ps$times) && length(ps$times) == 1) {
          ps$integrated = FALSE
        }
      }

      super$.score(
        prediction = prediction,
        learner = learner,
        task = task,
        train_set = train_set,
        FUN = survAUC::AUC.sh,
        type = ps$type,
        ...)
    }
  )
)
