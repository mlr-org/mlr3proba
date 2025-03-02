#' @title Survival Learner
#'
#' @description
#' This Learner specializes [Learner] for survival problems:
#'
#' * `task_type` is set to `"surv"`
#' * Creates [Prediction]s of class [PredictionSurv].
#' * Possible values for `predict_types` are:
#'   - `"distr"`: Predicts a probability distribution for each observation in the test set,
#'                uses [distr6].
#'   - `"lp"`: Predicts a linear predictor for each observation in the test set.
#'   - `"crank"`: Predicts a continuous ranking for each observation in the test set.
#'   - `"response"`: Predicts a survival time for each observation in the test set.
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
#' lrns = mlr_learners$mget(mlr_learners$keys("^surv"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' mlr_learners$get("surv.coxph")
#' lrn("surv.coxph")
LearnerSurv = R6Class("LearnerSurv",
  inherit = Learner,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), predict_types = "distr",
      feature_types = character(), properties = character(),
      packages = character(), label = NA_character_, man = NA_character_) {

      super$initialize(
        id = id, task_type = "surv", param_set = param_set, predict_types = predict_types,
        feature_types = feature_types, properties = properties,
        packages = c("mlr3proba", packages), label = label, man = man
      )
    }
  )
)
