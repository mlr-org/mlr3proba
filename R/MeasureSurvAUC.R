#' @title Abstract Class for survAUC Measures
#' @description This is an abstract class that should not be constructed directly.
#' @include MeasureSurvIntegrated.R
#'
#' @template param_integrated
#' @template param_times
#' @template param_id
#' @template param_measure_properties
#' @template param_man
#' @export
MeasureSurvAUC = R6Class("MeasureSurvAUC",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times, id, properties = character(),
                          man = NA_character_) {
      if (class(self)[[1]] == "MeasureSurvAUC") {
        stop("This is an abstract class that should not be constructed directly.")
      }

      super$initialize(
        integrated = integrated,
        times = times,
        id = id,
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = properties,
        man = man
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, task, train_set, FUN, ...) {

      args = list()
      if ("requires_train_set" %in% self$properties) {
        args$Surv.rsp = task$truth(train_set) # nolint
      }
      if ("requires_learner" %in% self$properties) {
        args$lp = learner$model$linear.predictors
      }

      args$times = self$times
      if (length(args$times) == 0) {
        args$times = sort(unique(prediction$truth[, 1]))
      }

      if ("Surv.rsp.new" %in% names(formals(FUN))) {
        args$Surv.rsp.new = prediction$truth # nolint
      }

      auc = mlr3misc::invoke(FUN, lpnew = prediction$lp, .args = args)

      if (self$integrated && !grepl("tnr|tpr", self$id)) {
        return(auc$iauc)
      } else {
        return(auc)
      }
    }
  )
)
