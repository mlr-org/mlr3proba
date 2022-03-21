#' @title Abstract Class for survAUC Measures
#' @description This is an abstract class that should not be constructed directly.
#'
#' @template param_integrated
#' @template param_times
#' @template param_id
#' @template param_measure_properties
#' @template param_label
#' @template param_man
#' @template param_param_set
#' @export
MeasureSurvAUC = R6Class("MeasureSurvAUC",
  inherit = MeasureSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, properties = character(), label = NA_character_,
      man = NA_character_, param_set = ps()) {
      if (class(self)[[1]] == "MeasureSurvAUC") {
        stop("This is an abstract class that should not be constructed directly.")
      }

      super$initialize(
        id = id,
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = properties,
        label = label,
        man = man,
        param_set = param_set
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, task, train_set, FUN, ...) {

      args = list()
      ps = self$param_set$values

      if ("requires_train_set" %in% self$properties) {
        args$Surv.rsp = task$truth(train_set) # nolint
      }
      if ("requires_learner" %in% self$properties) {
        args$lp = learner$model$linear.predictors
      }

      args$times = ps$times
      if (length(args$times) == 0) {
        args$times = sort(unique(prediction$truth[, 1]))
      }

      if ("Surv.rsp.new" %in% names(formals(FUN))) {
        args$Surv.rsp.new = prediction$truth # nolint
      }

      auc = mlr3misc::invoke(FUN, lpnew = prediction$lp, .args = args)

      if (is.null(ps$integrated) || !ps$integrated || grepl("tnr|tpr", self$id)) {
        auc
      } else {
        auc$iauc
      }
    }
  )
)
