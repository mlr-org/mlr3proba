#' @title Competing Risks Learner
#'
#' @description
#' This Learner specializes [Learner] for competing risks problems:
#'
#' - `task_type` is set to `"cmprsk"`
#' - Creates [Prediction]s of class [PredictionCompRisks].
#' - The only currently available option for `predict_types` is `"cif"`, which
#' represents the predicted **cumulative incidence function** for each observation
#' in the test set.
#'
#' @template param_id
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @family Learner
#' @export
#' @examples
#' library(mlr3)
#' # get all survival learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^cmprsk"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' mlr_learners$get("cmprsk.aalen")
#' lrn("cmprsk.aalen")
LearnerCompRisks = R6Class("LearnerCompRisks",
  inherit = Learner,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), predict_types = "cif",
      feature_types = character(), properties = character(),
      packages = character(), label = NA_character_, man = NA_character_) {

      super$initialize(
        id = id, task_type = "cmprsk", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties,
        packages = c("mlr3proba", packages), label = label, man = man
      )
    }
  )
)
